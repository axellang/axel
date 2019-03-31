module Axel.Utils.Display where

import Axel.Sourcemap (Bracket, Delimiter)
import qualified Axel.Sourcemap as SM (delimit, raw, surround, unassociated)

import Data.Char (toLower, toUpper)

lowerFirst :: String -> String
lowerFirst (x:xs) = toLower x : xs
lowerFirst "" = ""

surround :: Bracket -> String -> String
surround bracket x = SM.raw $ SM.surround bracket $ SM.unassociated x

delimit :: Delimiter -> [String] -> String
delimit delimiter xs = SM.raw $ SM.delimit delimiter $ map SM.unassociated xs

renderPragma :: String -> String
renderPragma pragma = "{-# " <> pragma <> " #-}"

upperFirst :: String -> String
upperFirst (x:xs) = toUpper x : xs
upperFirst "" = ""
