{-# LANGUAGE InstanceSigs #-}

-- NOTE Because this file will be used as the header of auto-generated macro programs,
--      it can't have any project-specific dependencies (such as `Fix`).
module Lihsp.Parse.AST where

data Expression
  = LiteralChar Char
  | LiteralInt Int
  | SExpression [Expression]
  | Symbol String
  deriving (Show)

toLihsp :: Expression -> String
toLihsp (LiteralChar x) = ['\\', x]
toLihsp (LiteralInt x) = show x
toLihsp (SExpression xs) = "(" ++ unwords (map toLihsp xs) ++ ")"
toLihsp (Symbol x) = x

-- TODO `Expression` should probably be `Traversable`, use recursion schemes, etc.
--      I should provide `toFix` and `fromFix` functions for macros to take advantage of.
--      (Maybe all macros have the argument automatically `fromFix`-ed to make consumption simpler?)
traverseExpression ::
     (Monad m) => (Expression -> m Expression) -> Expression -> m Expression
traverseExpression f expression =
  case expression of
    LiteralChar _ -> f expression
    LiteralInt _ -> f expression
    SExpression expressions ->
      f =<< (SExpression <$> traverse (traverseExpression f) expressions)
    Symbol _ -> f expression
