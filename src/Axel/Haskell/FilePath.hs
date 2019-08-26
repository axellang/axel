module Axel.Haskell.FilePath where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T (isSuffixOf, pack)

import System.FilePath (stripExtension)

convertExtension :: String -> String -> FilePath -> FilePath
convertExtension oldExt newExt axelPath =
  let basePath =
        if T.pack newExt `T.isSuffixOf` T.pack axelPath
          then fromMaybe axelPath $ stripExtension newExt axelPath
          else axelPath
   in basePath <> oldExt

axelPathToHaskellPath :: FilePath -> FilePath
axelPathToHaskellPath = convertExtension ".hs" ".axel"

haskellPathToAxelPath :: FilePath -> FilePath
haskellPathToAxelPath = convertExtension ".axel" ".hs"
