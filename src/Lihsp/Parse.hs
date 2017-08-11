{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lihsp.Parse where

import Control.Monad.Except (MonadError, throwError)

import Lihsp.Error (Error(ParseError))
import Lihsp.Utils
       (Bracket(Parentheses, SingleQuotes, SquareBrackets),
        Delimiter(Spaces), delimit, surround)

import Text.Parsec (ParsecT, Stream, (<|>), parse, try)
import Text.Parsec.Char
       (alphaNum, char, digit, letter, noneOf, oneOf, space)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Prim (many)

data Expression
  = LiteralChar Char
  | LiteralInt Int
  | LiteralList [Expression]
  | SExpression [Expression]
  | Symbol String

-- For debugging purposes
instance Show Expression where
  show :: Expression -> String
  show (LiteralChar x) = surround SingleQuotes [x]
  show (LiteralInt x) = show x
  show (LiteralList xs) = surround SquareBrackets $ delimit Spaces (map show xs)
  show (SExpression xs) = surround Parentheses $ delimit Spaces (map show xs)
  show (Symbol x) = x

any' :: Stream s m Char => ParsecT s u m Char
any' = noneOf ""

whitespace :: Stream s m Char => ParsecT s u m String
whitespace = many space

literalChar :: Stream s m Char => ParsecT s u m Expression
literalChar = LiteralChar <$> (char '\'' *> any' <* char '\'')

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
    validSymbol = oneOf "!@#$%^&*-=~_+,./<>?\\|"

expression :: Stream s m Char => ParsecT s u m Expression
expression =
  literalChar <|> literalInt <|> literalList <|> sExpression <|> symbol

program :: Stream s m Char => ParsecT s u m [Expression]
program = many $ try (whitespace *> sExpression) <|> sExpression

parseProgram :: (MonadError Error m) => String -> m [Expression]
parseProgram = either (throwError . ParseError) return . parse program ""
