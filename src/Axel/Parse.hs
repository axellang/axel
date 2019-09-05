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

import Control.Monad (void)
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Error (throwError)
import qualified Control.Monad.Freer.Error as Effs (Error)

import Data.List ((\\))
import Data.Maybe (fromMaybe)

import Text.Parsec (Parsec, (<|>), parse)
import Text.Parsec.Char
  ( alphaNum
  , anyChar
  , char
  , digit
  , newline
  , oneOf
  , space
  , string
  )
import Text.Parsec.Combinator (eof, many1, manyTill)
import Text.Parsec.Language (haskellDef)
import Text.Parsec.Pos (sourceColumn, sourceLine, sourceName)
import Text.Parsec.Prim (getPosition, many, skipMany, try)
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

eol :: Parsec String u ()
eol = try (void newline) <|> eof

ignored :: Parsec String u ()
ignored = skipMany $ try comment <|> void space

literalChar :: Parsec String u SM.Expression
literalChar = ann LiteralChar (string "#\\" *> anyChar)

literalInt :: Parsec String u SM.Expression
literalInt = ann LiteralInt (read <$> many1 digit)

literalList :: Parsec String u SM.Expression
literalList =
  ann
    SExpression
    ((Symbol Nothing "list" :) <$>
     (char '[' *> many sExpressionItem <* char ']'))

literalString :: Parsec String u SM.Expression
literalString = ann LiteralString (stringLiteral (makeTokenParser haskellDef))

quasiquotedExpression :: Parsec String u SM.Expression
quasiquotedExpression = parseReadMacro "`" "quasiquote"

quotedExpression :: Parsec String u SM.Expression
quotedExpression = parseReadMacro "'" "quote"

sExpressionItem :: Parsec String u SM.Expression
sExpressionItem = ignored *> expression <* ignored

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
       (try alphaNum <|> try (oneOf "'_") <|>
        try (oneOf (map fst haskellSyntaxSymbols \\ syntaxSymbols)) <|>
        oneOf (map fst haskellOperatorSymbols)))

unquotedExpression :: Parsec String u SM.Expression
unquotedExpression = parseReadMacro "~" "unquote"

comment :: Parsec String u ()
comment =
  try (string "--" *> eol) <|>
  void (string "-- " *> manyTill (void anyChar) eol)

expression :: Parsec String u SM.Expression
expression =
  try literalChar <|> try literalInt <|> try literalList <|> try literalString <|>
  try quotedExpression <|>
  try quasiquotedExpression <|>
  try spliceUnquotedExpression <|>
  try unquotedExpression <|>
  try sExpression <|>
  try infixSExpression <|>
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
    program = many1 (ignored *> expression <* ignored) <* eol
    throwErr = throwError @Error . ParseError filePath . show
    expandQuasiquotes =
      map $
      bottomUpFmapSplicing
        (\case
           SExpression _ (Symbol _ "quasiquote":xs') -> map expandQuasiquote xs'
           x -> [x])

parseSource ::
     (Member (Effs.Error Error) effs)
  => Maybe FilePath
  -> String
  -> Eff effs SM.Expression
parseSource filePath input = do
  wrapCompoundExpressions <$> parseMultiple filePath input

syntaxSymbols :: String
syntaxSymbols = "()[]{}"
