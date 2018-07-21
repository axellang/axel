{-# LANGUAGE FlexibleContexts #-}

module Axel.Utils.Directory where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl, control)

import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , getDirectoryContents
  , getTemporaryDirectory
  , withCurrentDirectory
  )
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

withCurrentDirectoryLifted :: (MonadBaseControl IO m) => FilePath -> m a -> m a
withCurrentDirectoryLifted directory f =
  control $ \runInIO -> withCurrentDirectory directory (runInIO f)

withTempDirectory :: (MonadIO m) => (FilePath -> m a) -> m a
withTempDirectory f = do
  temporaryDirectory <- liftIO getTemporaryDirectory
  liftIO $ createDirectoryIfMissing True temporaryDirectory
  result <- f temporaryDirectory
  pure result
