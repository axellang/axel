-- NOTE Because this file will be used as the header of auto-generated macro programs,
--      it can't have any project-specific dependencies (such as `Fix`).
module Axel.Parse.AST where

-- TODO `Expression` should probably be `Traversable`, use recursion schemes, etc.
--      I should provide `toFix` and `fromFix` functions for macros to take advantage of.
--      (Maybe all macros have the argument automatically `fromFix`-ed to make consumption simpler?)
data Expression
  = LiteralChar Char
  | LiteralInt Int
  | LiteralString String
  | SExpression [Expression]
  | Symbol String
  deriving (Eq, Show)

toAxel :: Expression -> String
toAxel (LiteralChar x) = ['\\', x]
toAxel (LiteralInt x) = show x
toAxel (LiteralString xs) = "\"" ++ xs ++ "\""
toAxel (SExpression xs) = "(" ++ unwords (map toAxel xs) ++ ")"
toAxel (Symbol x) = x
