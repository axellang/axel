module Axel.Parse where

import Axel.Prelude

import Axel.Eff.Error (Error(ParseError))
import Axel.Haskell.Language
  ( haskellKeywords
  , haskellOperatorSymbols
  , haskellSyntaxSymbols
  , isOperator
  )
import Axel.Parse.AST
  ( Expression(LiteralChar, LiteralFloat, LiteralInt, LiteralString,
           SExpression, Symbol)
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
import Axel.Utils.List (remove, unsafeHead)
import qualified Axel.Utils.Text as T (remove)

import Control.Applicative ((<|>))
import Control.Lens (op)
import Control.Monad (void)

import Data.List ((\\), foldl')
import Data.Maybe (fromMaybe)
import Data.MonoTraversable (onotElem)
import qualified Data.Text as T
import Data.Void (Void)

import Effectful ((:>))
import qualified Effectful as Eff
import qualified Effectful.Error.Static as Eff

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
  ( charLiteral
  , decimal
  , float
  , signed
  )

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
eol = void P.eol <|> P.eof

ignored :: Parser ()
ignored =
  P.skipMany $ P.try singleLineComment <|> multiLineComment <|> void P.spaceChar

literalChar :: Parser SM.Expression
literalChar = ann LiteralChar (P.string "#\\" *> P.charLiteral)

literalInt :: Parser SM.Expression
literalInt = ann LiteralInt (P.signed mempty P.decimal)

literalFloat :: Parser SM.Expression
literalFloat = ann LiteralFloat (P.signed mempty P.float)

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
  ann Symbol $
  hygenisizeIdentifier <$>
  P.some
    (P.alphaNumChar <|> P.oneOf ['\'', '_'] <|>
     P.oneOf (map fst haskellSyntaxSymbols \\ syntaxSymbols) <|>
     P.oneOf (map fst haskellOperatorSymbols))

unquotedExpression :: Parser SM.Expression
unquotedExpression = parseReadMacro "~" "unquote"

singleLineComment :: Parser ()
singleLineComment =
  P.try (P.string "--" *> eol) <|>
  void (P.string "-- " *> P.manyTill (void P.anySingle) eol)

multiLineComment :: Parser ()
multiLineComment =
  void $ P.try (P.string "{-") *> P.manyTill (void P.anySingle) (P.string "-}")

expression :: Parser SM.Expression
expression =
  P.try literalFloat <|> P.try literalInt <|> sExpression <|> infixSExpression <|>
  literalList <|>
  literalString <|>
  quotedExpression <|>
  quasiquotedExpression <|>
  spliceUnquotedExpression <|>
  unquotedExpression <|>
  literalChar <|>
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

parseMultiple' ::
     (Eff.Error Error :> effs)
  => ([SM.Expression] -> [SM.Expression])
  -> Maybe FilePath
  -> Text
  -> Eff.Eff effs [SM.Expression]
parseMultiple' postProcess maybeFilePath input =
  either throwErr (pure . postProcess) $
  P.parse program (T.unpack $ op FilePath filePath) input
  where
    filePath = fromMaybe (FilePath "") maybeFilePath
    program = P.some (ignored *> expression <* ignored) <* P.eof
    throwErr =
      Eff.throwError . ParseError filePath . T.pack . P.errorBundlePretty

parseMultiple ::
     (Eff.Error Error :> effs)
  => Maybe FilePath
  -> Text
  -> Eff.Eff effs [SM.Expression]
parseMultiple = parseMultiple' expandQuasiquotes
  where
    expandQuasiquotes =
      map $
      bottomUpFmapSplicing
        (\case
           SExpression _ (Symbol _ "quasiquote":xs') -> map expandQuasiquote xs'
           x -> [x])

parseSource ::
     (Eff.Error Error :> effs)
  => Maybe FilePath
  -> Text
  -> Eff.Eff effs SM.Expression
parseSource filePath input =
  wrapCompoundExpressions <$> parseMultiple filePath input

syntaxSymbols :: String
syntaxSymbols = "()[]{}\""

charReplacements ::
     [(Char, String)] -> [(Char, String)] -> String -> [(Char, String)]
charReplacements syntaxCharReplacements operatorCharReplacements x =
  let targetCharReplacements =
        syntaxCharReplacements <>
        if isOperator x
          then []
          else operatorCharReplacements
   in allowQualifiedNames $
      filter (\(sym, _) -> sym `onotElem` syntaxSymbols) targetCharReplacements
  where
    allowQualifiedNames = remove $ (== '.') . fst

valueHygienePrefix :: Text
valueHygienePrefix = "aXEL_VALUE_"

hygenisizeIdentifier :: String -> String
hygenisizeIdentifier x
  | [(_, new)] <- filter ((x ==) . fst) haskellKeywords = new
  | otherwise =
    let prefix =
          if unsafeHead x `elem` map fst replacements
            then valueHygienePrefix
            else ""
     in T.unpack $
        prefix <>
        foldl'
          (\acc (old, new) -> T.replace (T.singleton old) (T.pack new) acc)
          (T.pack x)
          replacements
  where
    replacements =
      charReplacements haskellSyntaxSymbols haskellOperatorSymbols x

unhygenisizeIdentifier :: String -> String
unhygenisizeIdentifier x
  | [(old, _)] <- filter ((x ==) . snd) haskellKeywords = old
  | otherwise -- There _could_ be multiple matches, but that would be VERY, VERY BAD™.
   =
    T.unpack $
    T.remove valueHygienePrefix $ -- The prefix isn't always there, in which case this just no-ops.
    foldl'
      (\acc (old, new) -> T.replace (T.pack new) (T.singleton old) acc)
      (T.pack x) $
    charReplacements haskellSyntaxSymbols haskellOperatorSymbols x
