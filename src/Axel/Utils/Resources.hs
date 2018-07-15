module Axel.Utils.Resources where

import Paths_axel (getDataFileName)

import System.FilePath ((</>))

import qualified System.IO.Strict as S (readFile)

readDataFile :: String -> IO String
readDataFile fileName =
  getDataFileName ("resources" </> fileName) >>= S.readFile
