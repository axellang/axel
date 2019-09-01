{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Axel.Parse where

import Axel.Eff.Error (Error(ParseError))
import Axel.Haskell.Language (haskellOperatorSymbols, haskellSyntaxSymbols)
import Axel.Parse.AST
  ( Expression(LiteralChar, LiteralInt, LiteralString, SExpression,
           Symbol)
  , bottomUpFmapSplicing
  , getAnn
  )
import qualified Axel.Sourcemap as SM (Expression)
import Axel.Sourcemap
  ( Position(Position, _column, _line)
  , SourceMetadata
  , quoteSourceMetadata
  , wrapCompoundExpressions
  )
import Axel.Utils.List (takeUntil)

import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Error (throwError)
import qualified Control.Monad.Freer.Error as Effs (Error)

import Data.List ((\\))
import Data.Maybe (fromMaybe)

import Text.Parsec (Parsec, (<|>), eof, parse, try)
import Text.Parsec.Char (alphaNum, char, digit, noneOf, oneOf, space, string)
import Text.Parsec.Combinator (many1, optional)
import Text.Parsec.Language (haskellDef)
import Text.Parsec.Pos (sourceColumn, sourceLine, sourceName)
import Text.Parsec.Prim (getPosition, many)
import Text.Parsec.Token (makeTokenParser, stringLiteral)

ann :: (SourceMetadata -> a -> b) -> Parsec s u a -> Parsec s u b
ann f x = do
  parsecPosition <- getPosition
  let sourcePosition =
        ( sourceName parsecPosition
        , Position
            { _line = sourceLine parsecPosition
            , _column = sourceColumn parsecPosition
            })
  f (Just sourcePosition) <$> x

parseReadMacro :: String -> String -> Parsec String u SM.Expression
parseReadMacro prefix wrapper = do
  expr <- string prefix *> expression
  ann SExpression (pure [Symbol Nothing wrapper, expr])

any' :: Parsec String u Char
any' = noneOf ""

whitespace :: Parsec String u String
whitespace = many space

literalChar :: Parsec String u SM.Expression
literalChar = ann LiteralChar (string "#\\" *> any')

literalInt :: Parsec String u SM.Expression
literalInt = ann LiteralInt (read <$> many1 digit)

literalList :: Parsec String u SM.Expression
literalList =
  ann
    SExpression
    ((Symbol Nothing "list" :) <$> (char '[' *> many item <* char ']'))
  where
    item = try (whitespace *> expression) <|> expression

literalString :: Parsec String u SM.Expression
literalString = ann LiteralString (stringLiteral (makeTokenParser haskellDef))

quasiquotedExpression :: Parsec String u SM.Expression
quasiquotedExpression = parseReadMacro "`" "quasiquote"

quotedExpression :: Parsec String u SM.Expression
quotedExpression = parseReadMacro "'" "quote"

sExpressionItem :: Parsec String u SM.Expression
sExpressionItem = try (whitespace *> expression) <|> expression

sExpression :: Parsec String u SM.Expression
sExpression = ann SExpression (char '(' *> many sExpressionItem <* char ')')

infixSExpression :: Parsec String u SM.Expression
infixSExpression =
  ann
    SExpression
    ((Symbol Nothing "applyInfix" :) <$>
     (char '{' *> many sExpressionItem <* char '}'))

spliceUnquotedExpression :: Parsec String u SM.Expression
spliceUnquotedExpression = parseReadMacro "~@" "unquoteSplicing"

symbol :: Parsec String u SM.Expression
symbol =
  ann
    Symbol
    (many1
       (alphaNum <|> oneOf "'_" <|>
        oneOf (map fst haskellSyntaxSymbols \\ syntaxSymbols) <|>
        oneOf (map fst haskellOperatorSymbols)))

unquotedExpression :: Parsec String u SM.Expression
unquotedExpression = parseReadMacro "~" "unquote"

expression :: Parsec String u SM.Expression
expression =
  literalChar <|> literalInt <|> literalList <|> literalString <|>
  quotedExpression <|>
  quasiquotedExpression <|>
  try spliceUnquotedExpression <|>
  unquotedExpression <|>
  sExpression <|>
  infixSExpression <|>
  symbol

-- Adapted from Appendix A of "Quasiquotation in Lisp" by Alan Bawden.
expandQuasiquote :: SM.Expression -> SM.Expression
expandQuasiquote (SExpression _ [Symbol _ "unquote", expr]) = expr
expandQuasiquote (SExpression _ [Symbol _ "unquoteSplicing", _]) =
  error
    "Illegal splicing unquote at the top level of a quasiquote! (`~@foo is not allowed, but `(~@foo) is.)"
expandQuasiquote (SExpression ann' xs) =
  SExpression
    ann'
    [ Symbol ann' "AST.SExpression"
    , quoteSourceMetadata ann'
    , SExpression
        ann'
        [ Symbol ann' "concat"
        , SExpression ann' (Symbol ann' "list" : map expandQuasiquoteInList xs)
        ]
    ]
expandQuasiquote expr =
  let ann' = getAnn expr
   in SExpression ann' [Symbol ann' "quote", expr]

expandQuasiquoteInList :: SM.Expression -> SM.Expression
expandQuasiquoteInList (SExpression _ [Symbol _ "unquoteSplicing", expr]) =
  let ann' = getAnn expr
   in SExpression ann' [Symbol ann' "AST.toExpressionList", expr]
expandQuasiquoteInList expr =
  let ann' = getAnn expr
   in SExpression ann' [Symbol ann' "list", expandQuasiquote expr]

parseMultiple ::
     (Member (Effs.Error Error) effs)
  => Maybe FilePath
  -> String
  -> Eff effs [SM.Expression]
parseMultiple maybeFilePath input =
  either throwErr (pure . expandQuasiquotes) $ parse program filePath input
  where
    filePath = fromMaybe "" maybeFilePath
    program =
      many1 (optional whitespace *> expression <* optional whitespace) <* eof
    throwErr = throwError @Error . ParseError filePath . show
    expandQuasiquotes =
      map $
      bottomUpFmapSplicing
        (\case
           SExpression _ (Symbol _ "quasiquote":xs') -> map expandQuasiquote xs'
           x -> [x])

stripComments :: String -> String
stripComments = unlines . map cleanLine . lines
  where
    cleanLine = takeUntil "--"

parseSource ::
     (Member (Effs.Error Error) effs)
  => Maybe FilePath
  -> String
  -> Eff effs SM.Expression
parseSource filePath input = do
  statements <- parseMultiple filePath $ stripComments input
  pure $ wrapCompoundExpressions statements

syntaxSymbols :: String
syntaxSymbols = "()[]{}"
