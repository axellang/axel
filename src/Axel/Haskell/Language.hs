module Axel.Haskell.Language where

import Axel.Prelude

import Data.Char (isSymbol, toUpper)

-- https://stackoverflow.com/questions/10548170/what-characters-are-permitted-for-haskell-operators
isOperator :: String -> Bool
isOperator = all $ \x -> isSymbol x || x `elem` map fst haskellOperatorSymbols

type SymbolReplacementMap = [(String, String)]

mkHygenicReplacementMap :: [(a, String)] -> [(a, String)]
mkHygenicReplacementMap =
  map (\(symbol, name) -> (symbol, "aXEL_SYMBOL_" <> map toUpper name <> "_"))

haskellOperatorSymbols :: [(Char, String)]
haskellOperatorSymbols =
  mkHygenicReplacementMap
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
    , (',', "comma")
    ]

haskellSyntaxSymbols :: [(Char, String)]
haskellSyntaxSymbols =
  mkHygenicReplacementMap
    [ (';', "semicolon")
    , ('[', "leftBracket")
    , (']', "rightBracket")
    , ('{', "rightBrace")
    , ('}', "leftBrace")
    , ('`', "tilde")
    ]

-- TODO Add a test that this list has no duplicates.
haskellKeywords :: [(String, String)]
haskellKeywords =
  mkHygenicReplacementMap $ map (\x -> (x, x)) ["do", "if", "let"]
