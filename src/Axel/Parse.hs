{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Axel.Parse where

import Axel.Error (Error(ParseError), unsafeIgnoreError)

import Axel.Haskell.Language (haskellOperatorSymbols, haskellSyntaxSymbols)
import Axel.Parse.AST
  ( Expression(LiteralChar, LiteralInt, LiteralString, SExpression,
           Symbol)
  , bottomUpFmapSplicing
  , getAnn
  )
import qualified Axel.Sourcemap as SM (Expression)
import Axel.Sourcemap
  ( SourcePosition(SourcePosition, _column, _line)
  , quoteSourceMetadata
  , wrapCompoundExpressions
  )
import Axel.Utils.List (takeUntil)

import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Error (throwError)
import qualified Control.Monad.Freer.Error as Effs (Error)

import Data.Functor.Identity (Identity)
import Data.List ((\\))
import Data.Maybe (fromMaybe)

import Text.Parsec (ParsecT, Stream, (<|>), eof, parse, try)
import Text.Parsec.Char (alphaNum, char, digit, noneOf, oneOf, space, string)
import Text.Parsec.Combinator (many1, optional)
import Text.Parsec.Language (haskellDef)
import Text.Parsec.Pos (sourceColumn, sourceLine)
import Text.Parsec.Prim (getPosition, many)
import Text.Parsec.Token (makeTokenParser, stringLiteral)

ann ::
     (Monad m)
  => (Maybe SourcePosition -> a -> b)
  -> ParsecT s u m a
  -> ParsecT s u m b
ann f x = do
  parsecPosition <- getPosition
  let sourcePosition =
        SourcePosition
          { _line = sourceLine parsecPosition
          , _column = sourceColumn parsecPosition
          }
  f (Just sourcePosition) <$> x

parseReadMacro :: String -> String -> ParsecT String u Identity SM.Expression
parseReadMacro prefix wrapper = do
  expr <- string prefix *> expression
  ann SExpression (pure [Symbol Nothing wrapper, expr])

any' :: (Stream s m Char) => ParsecT s u m Char
any' = noneOf ""

whitespace :: (Stream s m Char) => ParsecT s u m String
whitespace = many space

literalChar :: (Stream s m Char) => ParsecT s u m SM.Expression
literalChar = ann LiteralChar (string "#\\" *> any')

literalInt :: (Stream s m Char) => ParsecT s u m SM.Expression
literalInt = ann LiteralInt (read <$> many1 digit)

literalList :: ParsecT String u Identity SM.Expression
literalList =
  ann
    SExpression
    ((Symbol Nothing "list" :) <$> (char '[' *> many item <* char ']'))
  where
    item = try (whitespace *> expression) <|> expression

literalString :: ParsecT String u Identity SM.Expression
literalString = ann LiteralString (stringLiteral (makeTokenParser haskellDef))

quasiquotedExpression :: ParsecT String u Identity SM.Expression
quasiquotedExpression = parseReadMacro "`" "quasiquote"

quotedExpression :: ParsecT String u Identity SM.Expression
quotedExpression = parseReadMacro "'" "quote"

sExpressionItem :: ParsecT String u Identity SM.Expression
sExpressionItem = try (whitespace *> expression) <|> expression

sExpression :: ParsecT String u Identity SM.Expression
sExpression = ann SExpression (char '(' *> many sExpressionItem <* char ')')

infixSExpression :: ParsecT String u Identity SM.Expression
infixSExpression =
  ann
    SExpression
    ((Symbol Nothing "applyInfix" :) <$>
     (char '{' *> many sExpressionItem <* char '}'))

spliceUnquotedExpression :: ParsecT String u Identity SM.Expression
spliceUnquotedExpression = parseReadMacro "~@" "unquoteSplicing"

symbol :: (Stream s Identity Char) => ParsecT s u Identity SM.Expression
symbol =
  ann
    Symbol
    (many1
       (alphaNum <|> oneOf "'_" <|>
        oneOf (map fst haskellSyntaxSymbols \\ syntaxSymbols) <|>
        oneOf (map fst haskellOperatorSymbols)))

unquotedExpression :: ParsecT String u Identity SM.Expression
unquotedExpression = parseReadMacro "~" "unquote"

expression :: ParsecT String u Identity SM.Expression
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
parseMultiple filePath input =
  either throwErr (pure . expandQuasiquotes) $ parse program "" input
  where
    program =
      many1 (optional whitespace *> expression <* optional whitespace) <* eof
    throwErr = throwError @Error . ParseError (fromMaybe "" filePath) . show
    expandQuasiquotes =
      map $
      bottomUpFmapSplicing
        (\case
           SExpression _ (Symbol _ "quasiquote":xs') -> map expandQuasiquote xs'
           x -> [x])

-- | Will error at runtime if a parse error occurs.
--   If multiple expressions are able to be parsed, only the first will be returned.
unsafeParseSingle :: Maybe FilePath -> String -> SM.Expression
unsafeParseSingle filePath =
  head . unsafeIgnoreError @Error . parseMultiple filePath

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
