{-# LANGUAGE GADTs #-}

module Axel.Parse where

import Axel.Prelude

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
import Control.Lens (op)
import Control.Monad (void)

import Data.List ((\\))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Void (Void)

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P (charLiteral)

type Parser = P.Parsec Void Text

-- Adapted from https://hackage.haskell.org/package/megaparsec-7.0.5/docs/Text-Megaparsec-Char-Lexer.html#v:charLiteral.
stringLiteral :: Parser Text
stringLiteral = T.pack <$> (P.char '"' *> P.manyTill P.charLiteral (P.char '"'))

ann :: (SourceMetadata -> a -> b) -> Parser a -> Parser b
ann f x = do
  parsecPosition <- P.getSourcePos
  let sourcePosition =
        ( P.sourceName parsecPosition
        , Position
            { _line = P.unPos $ P.sourceLine parsecPosition
            , _column = P.unPos $ P.sourceColumn parsecPosition
            })
  f (Just sourcePosition) <$> x

parseReadMacro :: Text -> Text -> Parser SM.Expression
parseReadMacro prefix wrapper = do
  expr <- P.string prefix *> expression
  ann SExpression (pure [Symbol Nothing (T.unpack wrapper), expr])

eol :: Parser ()
eol = P.try (void P.newline) <|> P.eof

ignored :: Parser ()
ignored = P.skipMany $ P.try comment <|> void P.spaceChar

literalChar :: Parser SM.Expression
literalChar = ann LiteralChar (P.string "#\\" *> P.anySingle)

literalInt :: Parser SM.Expression
literalInt = ann LiteralInt (read <$> P.some P.digitChar)

literalList :: Parser SM.Expression
literalList =
  ann
    SExpression
    ((Symbol Nothing "list" :) <$>
     (P.char '[' *> P.many sExpressionItem <* P.char ']'))

literalString :: Parser SM.Expression
literalString = ann LiteralString (T.unpack <$> stringLiteral)

quasiquotedExpression :: Parser SM.Expression
quasiquotedExpression = parseReadMacro "`" "quasiquote"

quotedExpression :: Parser SM.Expression
quotedExpression = parseReadMacro "'" "quote"

sExpressionItem :: Parser SM.Expression
sExpressionItem = ignored *> expression <* ignored

sExpression :: Parser SM.Expression
sExpression =
  ann SExpression (P.char '(' *> P.many sExpressionItem <* P.char ')')

infixSExpression :: Parser SM.Expression
infixSExpression =
  ann
    SExpression
    ((Symbol Nothing "applyInfix" :) <$>
     (P.char '{' *> P.many sExpressionItem <* P.char '}'))

spliceUnquotedExpression :: Parser SM.Expression
spliceUnquotedExpression = parseReadMacro "~@" "unquoteSplicing"

symbol :: Parser SM.Expression
symbol =
  ann
    Symbol
    (P.some
       (P.try P.alphaNumChar <|> P.try (P.oneOf ['\'', '_']) <|>
        P.try (P.oneOf (map fst haskellSyntaxSymbols \\ syntaxSymbols)) <|>
        P.oneOf (map fst haskellOperatorSymbols)))

unquotedExpression :: Parser SM.Expression
unquotedExpression = parseReadMacro "~" "unquote"

comment :: Parser ()
comment =
  P.try (P.string "--" *> eol) <|>
  void (P.string "-- " *> P.manyTill (void P.anySingle) eol)

expression :: Parser SM.Expression
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
  -> Text
  -> Sem.Sem effs [SM.Expression]
parseMultiple maybeFilePath input =
  either throwErr (pure . expandQuasiquotes) $
  P.parse program (T.unpack $ op FilePath filePath) input
  where
    filePath = fromMaybe (FilePath "") maybeFilePath
    program = P.some (ignored *> expression <* ignored) <* eol
    throwErr = Sem.throw . ParseError filePath . showText
    expandQuasiquotes =
      map $
      bottomUpFmapSplicing
        (\case
           SExpression _ (Symbol _ "quasiquote":xs') -> map expandQuasiquote xs'
           x -> [x])

parseSource ::
     (Sem.Member (Sem.Error Error) effs)
  => Maybe FilePath
  -> Text
  -> Sem.Sem effs SM.Expression
parseSource filePath input =
  wrapCompoundExpressions <$> parseMultiple filePath input

syntaxSymbols :: String
syntaxSymbols = "()[]{}"
