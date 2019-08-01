module Axel.Haskell.Language where

import Data.Char (isSymbol, toUpper)

-- https://stackoverflow.com/questions/10548170/what-characters-are-permitted-for-haskell-operators
isOperator :: String -> Bool
isOperator =
  all $ \x -> isSymbol x || x `elem` map fst haskellOperatorSymbols || x == ','

type SymbolReplacementMap = [(Char, String)]

mkHygenicSymbolReplacementMap :: SymbolReplacementMap -> SymbolReplacementMap
mkHygenicSymbolReplacementMap =
  map (\(symbol, name) -> (symbol, "aXEL_SYMBOL_" <> map toUpper name <> "_"))

haskellOperatorSymbols :: SymbolReplacementMap
haskellOperatorSymbols =
  mkHygenicSymbolReplacementMap
    [ (':', "colon")
    , ('!', "bang")
    , ('#', "hash")
    , ('$', "dollar")
    , ('%', "percent")
    , ('&', "ampersand")
    , ('+', "plus")
    , ('*', "asterisk")
    , ('/', "slash")
    , ('<', "lessThan")
    , ('>', "greaterThan")
    , ('=', "equals")
    , ('\\', "backslash")
    , ('@', "at")
    , ('?', "questionMark")
    , ('^', "caret")
    , ('-', "hyphen")
    , ('|', "pipe")
    , ('~', "tilde")
    , ('.', "dot")
    ]

haskellSyntaxSymbols :: SymbolReplacementMap
haskellSyntaxSymbols =
  mkHygenicSymbolReplacementMap
    [ (',', "comma")
    , (';', "semicolon")
    , ('[', "leftBracket")
    , (']', "rightBracket")
    , ('{', "rightBrace")
    , ('}', "leftBrace")
    , ('`', "tilde")
    ]
