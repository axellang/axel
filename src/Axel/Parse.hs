-- NOTE Because `Axel.Parse.AST` will be used as the header of auto-generated macro programs,
--      it can't have any project-specific dependencies. As such, the instance definition for
--      `BottomUp Expression` can't be defined in the same file as `Expression` itself
--      (due to the dependency on `BottomUp`). Fortunately, `Axel.Parse.AST` will (should)
--      never be imported by itself but only implicitly as part of this module.
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Axel.Parse
  ( module Axel.Parse
  , module Axel.Parse.AST
  ) where

import Axel.Error (Error(ParseError), fatal)

-- Re-exporting these so that consumers of parsed ASTs do not need
-- to know about the internal file.
import Axel.Parse.AST
  ( Expression(LiteralChar, LiteralInt, LiteralString, SExpression,
           Symbol)
  )
import Axel.Utils.List (takeUntil)
import Axel.Utils.Recursion
  ( Recursive(bottomUpFmap, bottomUpTraverse, topDownFmap)
  )

import Control.Monad.Except (MonadError, throwError)

import Text.Parsec (ParsecT, Stream, (<|>), eof, parse, try)
import Text.Parsec.Char
  ( alphaNum
  , char
  , digit
  , letter
  , noneOf
  , oneOf
  , space
  , string
  )
import Text.Parsec.Combinator (many1, optional)
import Text.Parsec.Prim (many)

-- TODO `Expression` should probably instead be an instance of `Traversable`, use recursion schemes, etc.
--      If so, should I provide `toFix` and `fromFix` functions for macros to take advantage of?
--      (Maybe all macros have the argument automatically `fromFix`-ed to make consumption simpler?)
instance Recursive Expression where
  bottomUpFmap :: (Expression -> Expression) -> Expression -> Expression
  bottomUpFmap f x =
    f $
    case x of
      LiteralChar _ -> x
      LiteralInt _ -> x
      LiteralString _ -> x
      SExpression xs -> SExpression (map (bottomUpFmap f) xs)
      Symbol _ -> x
  bottomUpTraverse ::
       (Monad m) => (Expression -> m Expression) -> Expression -> m Expression
  bottomUpTraverse f x =
    f =<<
    case x of
      LiteralChar _ -> pure x
      LiteralInt _ -> pure x
      LiteralString _ -> pure x
      SExpression xs -> SExpression <$> traverse (bottomUpTraverse f) xs
      Symbol _ -> pure x
  topDownFmap :: (Expression -> Expression) -> Expression -> Expression
  topDownFmap f x =
    case f x of
      LiteralChar _ -> x
      LiteralInt _ -> x
      LiteralString _ -> x
      SExpression xs -> SExpression (map (topDownFmap f) xs)
      Symbol _ -> x

parseReadMacro ::
     (Stream s m Char) => String -> String -> ParsecT s u m Expression
parseReadMacro prefix wrapper = applyWrapper <$> (string prefix *> expression)
  where
    applyWrapper x = SExpression [Symbol wrapper, x]

any' :: (Stream s m Char) => ParsecT s u m Char
any' = noneOf ""

whitespace :: (Stream s m Char) => ParsecT s u m String
whitespace = many space

literalChar :: (Stream s m Char) => ParsecT s u m Expression
literalChar = LiteralChar <$> (char '{' *> any' <* char '}')

literalInt :: (Stream s m Char) => ParsecT s u m Expression
literalInt = LiteralInt . read <$> many1 digit

literalList :: (Stream s m Char) => ParsecT s u m Expression
literalList =
  SExpression . (Symbol "list" :) <$> (char '[' *> many item <* char ']')
  where
    item = try (whitespace *> expression) <|> expression

literalString :: (Stream s m Char) => ParsecT s u m Expression
literalString = LiteralString <$> (char '"' *> many (noneOf "\"") <* char '"')

quasiquotedExpression :: (Stream s m Char) => ParsecT s u m Expression
quasiquotedExpression = parseReadMacro "`" "quasiquote"

quotedExpression :: (Stream s m Char) => ParsecT s u m Expression
quotedExpression = parseReadMacro "'" "quote"

sExpression :: (Stream s m Char) => ParsecT s u m Expression
sExpression = SExpression <$> (char '(' *> many item <* char ')')
  where
    item = try (whitespace *> expression) <|> expression

spliceUnquotedExpression :: (Stream s m Char) => ParsecT s u m Expression
spliceUnquotedExpression = parseReadMacro "~@" "unquoteSplicing"

symbol :: (Stream s m Char) => ParsecT s u m Expression
symbol =
  Symbol <$>
  ((:) <$> (letter <|> validSymbol) <*> many (alphaNum <|> validSymbol))
  where
    validSymbol = oneOf "!@#$%^&*-=~_+,./<>?\\|':"

unquotedExpression :: (Stream s m Char) => ParsecT s u m Expression
unquotedExpression = parseReadMacro "~" "unquote"

expression :: (Stream s m Char) => ParsecT s u m Expression
expression =
  literalChar <|> literalInt <|> literalList <|> literalString <|>
  quotedExpression <|>
  quasiquotedExpression <|>
  try spliceUnquotedExpression <|>
  unquotedExpression <|>
  sExpression <|>
  symbol

-- TODO Derive this with Template Haskell (it's really brittle, currently).
quoteParseExpression :: Expression -> Expression
quoteParseExpression (LiteralChar x) =
  SExpression [Symbol "AST.LiteralChar", LiteralChar x]
quoteParseExpression (LiteralInt x) =
  SExpression [Symbol "AST.LiteralInt", LiteralInt x]
quoteParseExpression (LiteralString x) =
  SExpression [Symbol "AST.LiteralString", LiteralString x]
quoteParseExpression (SExpression xs) =
  SExpression
    [ Symbol "AST.SExpression"
    , SExpression (Symbol "list" : map quoteParseExpression xs)
    ]
quoteParseExpression (Symbol x) =
  SExpression [Symbol "AST.Symbol", LiteralString (handleEscapes x)]
  where
    handleEscapes =
      concatMap $ \case
        '\\' -> "\\\\"
        c -> [c]

parseMultiple :: (MonadError Error m) => String -> m [Expression]
parseMultiple =
  either (throwError . ParseError . show) (pure . map expandQuotes) .
  parse
    (many1 (optional whitespace *> expression <* optional whitespace) <* eof)
    ""
  where
    expandQuotes =
      topDownFmap
        (\case
           SExpression [Symbol "quote", x] -> quoteParseExpression x
           x -> x)

parseSingle :: (MonadError Error m) => String -> m Expression
parseSingle input =
  parseMultiple input >>= \case
    [x] -> pure x
    _ -> throwError $ ParseError "Only one expression was expected"

stripComments :: String -> String
stripComments = unlines . map cleanLine . lines
  where
    cleanLine = takeUntil "--"

parseSource :: (MonadError Error m) => String -> m Expression
parseSource input = do
  statements <- parseMultiple $ stripComments input
  pure $ SExpression (Symbol "begin" : statements)

programToTopLevelExpressions :: Expression -> [Expression]
programToTopLevelExpressions (SExpression (Symbol "begin":stmts)) = stmts
programToTopLevelExpressions _ = fatal "programToTopLevelExpressions" "0001"

topLevelExpressionsToProgram :: [Expression] -> Expression
topLevelExpressionsToProgram stmts = SExpression (Symbol "begin" : stmts)
