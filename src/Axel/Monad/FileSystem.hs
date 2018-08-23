{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Axel.Monad.FileSystem where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl, control)

import qualified System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , getCurrentDirectory
  , getDirectoryContents
  , getTemporaryDirectory
  , removeFile
  , setCurrentDirectory
  , withCurrentDirectory
  )
import System.FilePath ((</>))
import qualified System.IO.Strict as S (readFile)

class MonadFileSystem m where
  copyFile :: FilePath -> FilePath -> m ()
  createDirectoryIfMissing :: Bool -> FilePath -> m ()
  getCurrentDirectory :: m FilePath
  getDirectoryContentsRec :: FilePath -> m [FilePath]
  readFile :: FilePath -> m String
  removeFile :: FilePath -> m ()
  setCurrentDirectory :: FilePath -> m ()
  withCurrentDirectory :: FilePath -> m a -> m a
  withTempDirectory :: (FilePath -> m a) -> m a
  writeFile :: String -> FilePath -> m ()

-- NOTE This is undecidable, but `mtl` uses undecidable instances in this scenario(?)....
--      Plus, I can't actually come up with a better solution.
instance (MonadBaseControl IO m, MonadIO m) => MonadFileSystem m where
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
  readFile :: FilePath -> m String
  readFile = liftIO . S.readFile
  removeFile :: FilePath -> m ()
  removeFile = liftIO . System.Directory.removeFile
  setCurrentDirectory :: FilePath -> m ()
  setCurrentDirectory = liftIO . System.Directory.setCurrentDirectory
  withCurrentDirectory :: FilePath -> m a -> m a
  withCurrentDirectory directory f =
    control $ \runInIO ->
      System.Directory.withCurrentDirectory directory (runInIO f)
  withTempDirectory :: (FilePath -> m a) -> m a
  withTempDirectory f = do
    tempDirectory <- liftIO System.Directory.getTemporaryDirectory
    createDirectoryIfMissing True tempDirectory
    f tempDirectory
  writeFile :: FilePath -> String -> m ()
  writeFile filePath contents = liftIO $ Prelude.writeFile filePath contents
