{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

import Control.Applicative ((<|>))
import Control.Monad (void)

import Data.List ((\\))
import Data.Maybe (fromMaybe)

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem

import qualified Text.Parsec as P
import Text.Parsec.Language (haskellDef)
import Text.Parsec.Token (makeTokenParser, stringLiteral)

ann :: (SourceMetadata -> a -> b) -> P.Parsec s u a -> P.Parsec s u b
ann f x = do
  parsecPosition <- P.getPosition
  let sourcePosition =
        ( P.sourceName parsecPosition
        , Position
            { _line = P.sourceLine parsecPosition
            , _column = P.sourceColumn parsecPosition
            })
  f (Just sourcePosition) <$> x

parseReadMacro :: String -> String -> P.Parsec String u SM.Expression
parseReadMacro prefix wrapper = do
  expr <- P.string prefix *> expression
  ann SExpression (pure [Symbol Nothing wrapper, expr])

eol :: P.Parsec String u ()
eol = P.try (void P.newline) <|> P.eof

ignored :: P.Parsec String u ()
ignored = P.skipMany $ P.try comment <|> void P.space

literalChar :: P.Parsec String u SM.Expression
literalChar = ann LiteralChar (P.string "#\\" *> P.anyChar)

literalInt :: P.Parsec String u SM.Expression
literalInt = ann LiteralInt (read <$> P.many1 P.digit)

literalList :: P.Parsec String u SM.Expression
literalList =
  ann
    SExpression
    ((Symbol Nothing "list" :) <$>
     (P.char '[' *> P.many sExpressionItem <* P.char ']'))

literalString :: P.Parsec String u SM.Expression
literalString = ann LiteralString (stringLiteral (makeTokenParser haskellDef))

quasiquotedExpression :: P.Parsec String u SM.Expression
quasiquotedExpression = parseReadMacro "`" "quasiquote"

quotedExpression :: P.Parsec String u SM.Expression
quotedExpression = parseReadMacro "'" "quote"

sExpressionItem :: P.Parsec String u SM.Expression
sExpressionItem = ignored *> expression <* ignored

sExpression :: P.Parsec String u SM.Expression
sExpression =
  ann SExpression (P.char '(' *> P.many sExpressionItem <* P.char ')')

infixSExpression :: P.Parsec String u SM.Expression
infixSExpression =
  ann
    SExpression
    ((Symbol Nothing "applyInfix" :) <$>
     (P.char '{' *> P.many sExpressionItem <* P.char '}'))

spliceUnquotedExpression :: P.Parsec String u SM.Expression
spliceUnquotedExpression = parseReadMacro "~@" "unquoteSplicing"

symbol :: P.Parsec String u SM.Expression
symbol =
  ann
    Symbol
    (P.many1
       (P.try P.alphaNum <|> P.try (P.oneOf "'_") <|>
        P.try (P.oneOf (map fst haskellSyntaxSymbols \\ syntaxSymbols)) <|>
        P.oneOf (map fst haskellOperatorSymbols)))

unquotedExpression :: P.Parsec String u SM.Expression
unquotedExpression = parseReadMacro "~" "unquote"

comment :: P.Parsec String u ()
comment =
  P.try (P.string "--" *> eol) <|>
  void (P.string "-- " *> P.manyTill (void P.anyChar) eol)

expression :: P.Parsec String u SM.Expression
expression =
  P.try literalChar <|> P.try literalInt <|> P.try literalList <|>
  P.try literalString <|>
  P.try quotedExpression <|>
  P.try quasiquotedExpression <|>
  P.try spliceUnquotedExpression <|>
  P.try unquotedExpression <|>
  P.try sExpression <|>
  P.try infixSExpression <|>
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
     (Sem.Member (Sem.Error Error) effs)
  => Maybe FilePath
  -> String
  -> Sem.Sem effs [SM.Expression]
parseMultiple maybeFilePath input =
  either throwErr (pure . expandQuasiquotes) $ P.parse program filePath input
  where
    filePath = fromMaybe "" maybeFilePath
    program = P.many1 (ignored *> expression <* ignored) <* eol
    throwErr = Sem.throw . ParseError filePath . show
    expandQuasiquotes =
      map $
      bottomUpFmapSplicing
        (\case
           SExpression _ (Symbol _ "quasiquote":xs') -> map expandQuasiquote xs'
           x -> [x])

parseSource ::
     (Sem.Member (Sem.Error Error) effs)
  => Maybe FilePath
  -> String
  -> Sem.Sem effs SM.Expression
parseSource filePath input =
  wrapCompoundExpressions <$> parseMultiple filePath input

syntaxSymbols :: String
syntaxSymbols = "()[]{}"
