-- NOTE Because `Lihsp.Parse.AST` will be used as the header of auto-generated macro programs,
--      it can't have any project-specific dependencies. As such, the instance definition for
--      `Recursively Expression` can't be defined in the same file as `Expression` itself
--      (due to the dependency on `Recursively`). Fortunately, `Lihsp.Parse.AST` will (should)
--      never be imported by itself but only implicitly as part of this module.
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lihsp.Parse
  ( module Lihsp.Parse
  , module Lihsp.Parse.AST
  ) where

import Control.Monad.Except (MonadError, throwError)

import Lihsp.Error (Error(ParseError))
import Lihsp.Parse.AST
       (Expression(LiteralChar, LiteralInt, LiteralList, SExpression,
                   Symbol))
import Lihsp.Utils
       (Recursively(recursively), isOperator, kebabToCamelCase)

import Text.Parsec (ParsecT, Stream, (<|>), eof, parse, try)
import Text.Parsec.Char
       (alphaNum, char, digit, letter, noneOf, oneOf, space)
import Text.Parsec.Combinator (many1, optional)
import Text.Parsec.Prim (many)

any' :: Stream s m Char => ParsecT s u m Char
any' = noneOf ""

whitespace :: Stream s m Char => ParsecT s u m String
whitespace = many space

literalChar :: Stream s m Char => ParsecT s u m Expression
literalChar = LiteralChar <$> (char '\\' *> any')

literalInt :: Stream s m Char => ParsecT s u m Expression
literalInt = LiteralInt . read <$> many1 digit

literalList :: Stream s m Char => ParsecT s u m Expression
literalList = LiteralList <$> (char '[' *> many item <* char ']')
  where
    item = try (whitespace *> expression) <|> expression

literalString :: Stream s m Char => ParsecT s u m Expression
literalString =
  LiteralList <$>
  (map LiteralChar <$> (char '"' *> many (noneOf "\"") <* char '"'))

sExpression :: Stream s m Char => ParsecT s u m Expression
sExpression = SExpression <$> (char '(' *> many item <* char ')')
  where
    item = try (whitespace *> expression) <|> expression

symbol :: Stream s m Char => ParsecT s u m Expression
symbol =
  Symbol <$>
  ((:) <$> (letter <|> validSymbol) <*> many (alphaNum <|> validSymbol))
  where
    validSymbol = oneOf "!@#$%^&*-=~_+,./<>?\\|':"

expression :: Stream s m Char => ParsecT s u m Expression
expression =
  literalChar <|> literalInt <|> literalList <|> literalString <|> sExpression <|>
  symbol

program :: Stream s m Char => ParsecT s u m [Expression]
program =
  many (try (whitespace *> sExpression) <|> sExpression) <* optional whitespace <*
  eof

normalizeCase :: Expression -> Expression
normalizeCase (Symbol x) =
  if not (isOperator x) && '-' `elem` x
    then Symbol (kebabToCamelCase x)
    else Symbol x
normalizeCase x = x

instance Recursively Expression where
  recursively :: (Expression -> Expression) -> Expression -> Expression
  recursively f x =
    case x of
      LiteralChar _ -> f x
      LiteralInt _ -> f x
      LiteralList xs -> f $ LiteralList (map (recursively f) xs)
      SExpression xs -> f $ SExpression (map (recursively f) xs)
      Symbol _ -> f x

parseProgram :: (MonadError Error m) => String -> m [Expression]
parseProgram =
  either (throwError . ParseError) (return . map (recursively normalizeCase)) .
  parse program ""
