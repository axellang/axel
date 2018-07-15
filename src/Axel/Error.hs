module Axel.Error where

import Data.Semigroup ((<>))

import Text.Parsec (ParseError)

data Error
  = MacroError String
  | NormalizeError String
  | ParseError ParseError
  deriving (Show)

fatal :: String -> String -> a
fatal context message = error $ "[FATAL] " <> context <> " - " <> message
