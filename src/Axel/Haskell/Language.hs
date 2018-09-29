module Axel.Haskell.Language where

import Data.Char (isSymbol)

-- https://stackoverflow.com/questions/10548170/what-characters-are-permitted-for-haskell-operators
isOperator :: String -> Bool
isOperator = all $ \x -> isSymbol x || x `elem` map fst haskellOperatorSymbols

haskellOperatorSymbols :: [(Char, String)]
haskellOperatorSymbols =
  [ (':', "axelSymbolColon")
  , ('!', "axelSymbolBang")
  , ('#', "axelSymbolHash")
  , ('$', "axelSymbolDollar")
  , ('%', "axelSymbolPercent")
  , ('&', "axelSymbolAmpersand")
  , ('+', "axelSymbolPlus")
  , ('*', "axelSymbolAsterisk")
  , ('/', "axelSymbolSlash")
  , ('<', "axelSymbolLess")
  , ('>', "axelSymbolGreater")
  , ('=', "axelSymbolEquals")
  , ('\\', "axelSymbolBackslash")
  , ('@', "axelSymbolAt")
  , ('?', "axelSymbolQuestion")
  , ('^', "axelSymbolCaret")
  , ('-', "axelSymbolDash")
  , ('|', "axelSymbolPipe")
  , ('~', "axelSymbolTilde")
  ]

haskellSyntaxSymbols :: [(Char, String)]
haskellSyntaxSymbols =
  [ ('.', "axelSymbolDot")
  , (',', "axelSymbolComma")
  , (';', "axelSymbolSemicolon")
  , ('[', "axelSymbolLeftBracket")
  , (']', "axelSymbolRightBracket")
  , ('{', "axelSymbolLeftBrace")
  , ('}', "axelSymbolRightBrace")
  , ('`', "axelSymbolGrave")
  ]
