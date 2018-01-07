module Lihsp.Utils.Resources where

import Data.Semigroup ((<>))

import Paths_lihsp (getDataFileName)

readDataFile :: String -> IO String
readDataFile fileName = getDataFileName ("resources/" <> fileName) >>= readFile
