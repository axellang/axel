module Axel.Utils.Directory where

import Control.Monad (forM)

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

-- Adapted from http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html.
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents startDir = do
  names <- getDirectoryContents startDir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <-
    forM properNames $ \name -> do
      let path = startDir </> name
      isDirectory <- doesDirectoryExist path
      if isDirectory
        then getRecursiveContents path
        else pure [path]
  pure $ concat paths
