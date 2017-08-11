module Lihsp.Utils where

import Data.List (intercalate)
import Data.Semigroup ((<>))

data Delimiter
  = Commas
  | Newlines
  | Pipes
  | Spaces

delimit :: Delimiter -> [String] -> String
delimit delimiter = intercalate (lookupDelimiter delimiter)
  where
    lookupDelimiter Commas = ","
    lookupDelimiter Newlines = "\n"
    lookupDelimiter Pipes = "|"
    lookupDelimiter Spaces = " "

renderBlock :: [String] -> String
renderBlock = surround CurlyBraces . intercalate ";"

data Bracket
  = CurlyBraces
  | DoubleQuotes
  | Parentheses
  | SingleQuotes
  | SquareBrackets

surround :: Bracket -> String -> String
surround CurlyBraces x = "{" <> x <> "}"
surround DoubleQuotes x = "\"" <> x <> "\""
surround Parentheses x = "(" <> x <> ")"
surround SingleQuotes x = "'" <> x <> "'"
surround SquareBrackets x = "[" <> x <> "]"
