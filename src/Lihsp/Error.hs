module Lihsp.Error where

import Text.Parsec (ParseError)

data Error
  = NormalizeError String
  | ParseError ParseError
  deriving (Show)
