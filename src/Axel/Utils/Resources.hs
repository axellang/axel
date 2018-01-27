module Axel.Utils.Resources where

import Data.Semigroup ((<>))

import Paths_axel (getDataFileName)

readDataFile :: String -> IO String
readDataFile fileName = getDataFileName ("resources/" <> fileName) >>= readFile
