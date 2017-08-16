module Lihsp.Error where

import Text.Parsec (ParseError)

data Error
  = MacroError String
  | NormalizeError String
  | ParseError ParseError
  deriving (Show)
