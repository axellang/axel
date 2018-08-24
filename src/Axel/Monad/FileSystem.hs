{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Axel.Monad.FileSystem where

import Control.Monad (forM)
import Control.Monad.Free (Free, liftF)
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
  doesDirectoryExist :: FilePath -> m Bool
  getCurrentDirectory :: m FilePath
  getDirectoryContents :: FilePath -> m [FilePath]
  getTemporaryDirectory :: m FilePath
  readFile :: FilePath -> m String
  removeFile :: FilePath -> m ()
  setCurrentDirectory :: FilePath -> m ()
  writeFile :: String -> FilePath -> m ()

instance {-# OVERLAPPABLE #-} MonadIO m => MonadFileSystem m where
  copyFile :: FilePath -> FilePath -> m ()
  copyFile src dest = liftIO $ System.Directory.copyFile src dest
  createDirectoryIfMissing :: Bool -> FilePath -> m ()
  createDirectoryIfMissing createParentDirs path =
    liftIO $ System.Directory.createDirectoryIfMissing createParentDirs path
  doesDirectoryExist :: FilePath -> m Bool
  doesDirectoryExist = liftIO . System.Directory.doesDirectoryExist
  getCurrentDirectory :: m FilePath
  getCurrentDirectory = liftIO System.Directory.getCurrentDirectory
  getDirectoryContents :: FilePath -> m [FilePath]
  getDirectoryContents = liftIO . System.Directory.getDirectoryContents
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

data FileSystemAction next
  = CopyFile FilePath
             FilePath
             next
  | CreateDirectoryIfMissing Bool
                             FilePath
                             next
  | DoesDirectoryExist FilePath
                       (Bool -> next)
  | GetCurrentDirectory (FilePath -> next)
  | GetDirectoryContents FilePath
                         ([FilePath] -> next)
  | GetTemporaryDirectory (FilePath -> next)
  | ReadFile FilePath
             (String -> next)
  | RemoveFile FilePath
               next
  | SetCurrentDirectory FilePath
                        next
  | WriteFile String
              FilePath
              next
  deriving (Functor)

type FileSystem = Free FileSystemAction

instance MonadFileSystem FileSystem where
  copyFile src dest = liftF $ CopyFile src dest ()
  createDirectoryIfMissing createParents path =
    liftF $ CreateDirectoryIfMissing createParents path ()
  doesDirectoryExist path = liftF $ DoesDirectoryExist path id
  getCurrentDirectory = liftF $ GetCurrentDirectory id
  getDirectoryContents path = liftF $ GetDirectoryContents path id
  getTemporaryDirectory = liftF $ GetTemporaryDirectory id
  readFile path = liftF $ ReadFile path id
  removeFile path = liftF $ RemoveFile path ()
  setCurrentDirectory path = liftF $ SetCurrentDirectory path ()
  writeFile contents path = liftF $ WriteFile contents path ()

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

-- Adapted from http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html.
getDirectoryContentsRec ::
     (Monad m, MonadFileSystem m) => FilePath -> m [FilePath]
getDirectoryContentsRec dir = do
  names <- getDirectoryContents dir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <-
    forM properNames $ \name -> do
      let path = dir </> name
      isDirectory <- doesDirectoryExist path
      if isDirectory
        then getDirectoryContentsRec path
        else pure [path]
  pure $ concat paths
