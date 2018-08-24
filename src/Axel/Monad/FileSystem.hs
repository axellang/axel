{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Axel.Monad.FileSystem where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , getCurrentDirectory
  , getDirectoryContents
  , getTemporaryDirectory
  , removeFile
  , setCurrentDirectory
  )
import System.FilePath ((</>))
import qualified System.IO.Strict as S (readFile)

class MonadFileSystem m where
  copyFile :: FilePath -> FilePath -> m ()
  createDirectoryIfMissing :: Bool -> FilePath -> m ()
  getCurrentDirectory :: m FilePath
  getDirectoryContentsRec :: FilePath -> m [FilePath]
  getTemporaryDirectory :: m FilePath
  readFile :: FilePath -> m String
  removeFile :: FilePath -> m ()
  setCurrentDirectory :: FilePath -> m ()
  writeFile :: String -> FilePath -> m ()

instance (Monad m, MonadIO m) => MonadFileSystem m where
  copyFile :: FilePath -> FilePath -> m ()
  copyFile src dest = liftIO $ System.Directory.copyFile src dest
  createDirectoryIfMissing :: Bool -> FilePath -> m ()
  createDirectoryIfMissing createParentDirs path =
    liftIO $ System.Directory.createDirectoryIfMissing createParentDirs path
  getCurrentDirectory :: m FilePath
  getCurrentDirectory = liftIO System.Directory.getCurrentDirectory
  -- Adapted from http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html.
  getDirectoryContentsRec :: FilePath -> m [FilePath]
  getDirectoryContentsRec dir = do
    names <- liftIO $ System.Directory.getDirectoryContents dir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <-
      forM properNames $ \name -> do
        let path = dir </> name
        isDirectory <- liftIO $ System.Directory.doesDirectoryExist path
        if isDirectory
          then getDirectoryContentsRec path
          else pure [path]
    pure $ concat paths
  getTemporaryDirectory :: m FilePath
  getTemporaryDirectory = liftIO System.Directory.getTemporaryDirectory
  readFile :: FilePath -> m String
  readFile = liftIO . S.readFile
  removeFile :: FilePath -> m ()
  removeFile = liftIO . System.Directory.removeFile
  setCurrentDirectory :: FilePath -> m ()
  setCurrentDirectory = liftIO . System.Directory.setCurrentDirectory
  writeFile :: FilePath -> String -> m ()
  writeFile filePath contents = liftIO $ Prelude.writeFile filePath contents

withTemporaryDirectory ::
     (Monad m, MonadFileSystem m) => (FilePath -> m a) -> m a
withTemporaryDirectory action = getTemporaryDirectory >>= action

withCurrentDirectory :: (Monad m, MonadFileSystem m) => FilePath -> m a -> m a
withCurrentDirectory directory f = do
  originalDirectory <- getCurrentDirectory
  setCurrentDirectory directory
  result <- f
  setCurrentDirectory originalDirectory
  pure result
