{-# LANGUAGE InstanceSigs #-}

-- NOTE Because this file will be used as the header of auto-generated macro programs,
--      it can't have any project-specific dependencies.
module Lihsp.Parse.AST where

import Data.Semigroup ((<>))

data Expression
  = LiteralChar Char
  | LiteralInt Int
  | LiteralList [Expression]
  | SExpression [Expression]
  | Symbol String

instance Show Expression where
  show :: Expression -> String
  show (LiteralChar x) = ['\'', x, '\'']
  show (LiteralInt x) = show x
  show (LiteralList xs) = "[" <> unwords (map show xs) <> "]"
  show (SExpression xs) = "(" <> unwords (map show xs) <> ")"
  show (Symbol x) = x
