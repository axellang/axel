{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Axel.Monad.FileSystem where

import Prelude hiding (readFile, writeFile)
import qualified Prelude (readFile, writeFile)

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Identity (IdentityT)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (RWST)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (RWST)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.State.Lazy as LazyState (StateT)
import qualified Control.Monad.Trans.State.Strict as StrictState (StateT)
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter (WriterT)

import Axel.Utils.Debug

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

class (Monad m) =>
      MonadFileSystem m
  where
  copyFile :: FilePath -> FilePath -> m ()
  default copyFile :: (MonadTrans t, MonadFileSystem m', m ~ t m') =>
    FilePath -> FilePath -> m ()
  copyFile src dest = lift $ copyFile src dest
  createDirectoryIfMissing :: Bool -> FilePath -> m ()
  default createDirectoryIfMissing :: ( MonadTrans t
                                      , MonadFileSystem m'
                                      , m ~ t m'
                                      ) =>
    Bool -> FilePath -> m ()
  createDirectoryIfMissing createParents path =
    lift $ createDirectoryIfMissing createParents path
  doesDirectoryExist :: FilePath -> m Bool
  default doesDirectoryExist :: (MonadTrans t, MonadFileSystem m', m ~ t m') =>
    FilePath -> m Bool
  doesDirectoryExist = lift . doesDirectoryExist
  getCurrentDirectory :: m FilePath
  default getCurrentDirectory :: (MonadTrans t, MonadFileSystem m', m ~ t m') =>
    m FilePath
  getCurrentDirectory = lift getCurrentDirectory
  getDirectoryContents :: FilePath -> m [FilePath]
  default getDirectoryContents :: (MonadTrans t, MonadFileSystem m', m ~ t m') =>
    FilePath -> m [FilePath]
  getDirectoryContents = lift . getDirectoryContents
  getTemporaryDirectory :: m FilePath
  default getTemporaryDirectory :: (MonadTrans t, MonadFileSystem m', m ~ t m') =>
    m FilePath
  getTemporaryDirectory = lift getTemporaryDirectory
  readFile :: FilePath -> m String
  default readFile :: (MonadTrans t, MonadFileSystem m', m ~ t m') =>
    FilePath -> m String
  readFile = lift . readFile
  removeFile :: FilePath -> m ()
  default removeFile :: (MonadTrans t, MonadFileSystem m', m ~ t m') =>
    FilePath -> m ()
  removeFile = lift . removeFile
  setCurrentDirectory :: FilePath -> m ()
  default setCurrentDirectory :: (MonadTrans t, MonadFileSystem m', m ~ t m') =>
    FilePath -> m ()
  setCurrentDirectory = lift . setCurrentDirectory
  writeFile :: FilePath -> String -> m ()
  default writeFile :: (MonadTrans t, MonadFileSystem m', m ~ t m') =>
    String -> FilePath -> m ()
  writeFile path contents = lift $ writeFile path contents

instance (MonadFileSystem m) => MonadFileSystem (ContT r m)

instance (MonadFileSystem m) => MonadFileSystem (ExceptT e m)

instance (MonadFileSystem m) => MonadFileSystem (IdentityT m)

instance (MonadFileSystem m) => MonadFileSystem (MaybeT m)

instance (MonadFileSystem m) => MonadFileSystem (ReaderT r m)

instance (Monoid w, MonadFileSystem m) =>
         MonadFileSystem (LazyRWS.RWST r w s m)

instance (Monoid w, MonadFileSystem m) =>
         MonadFileSystem (StrictRWS.RWST r w s m)

instance (MonadFileSystem m) => MonadFileSystem (LazyState.StateT s m)

instance (MonadFileSystem m) => MonadFileSystem (StrictState.StateT s m)

instance (Monoid w, MonadFileSystem m) =>
         MonadFileSystem (LazyWriter.WriterT w m)

instance (Monoid w, MonadFileSystem m) =>
         MonadFileSystem (StrictWriter.WriterT w m)

instance {-# OVERLAPPABLE #-} (Monad m, MonadIO m) => MonadFileSystem m where
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

withCurrentDirectory :: (MonadFileSystem m) => FilePath -> m a -> m a
withCurrentDirectory directory f = do
  originalDirectory <- getCurrentDirectory
  setCurrentDirectory directory
  result <- f
  setCurrentDirectory originalDirectory
  pure result

withTemporaryDirectory :: (MonadFileSystem m) => (FilePath -> m a) -> m a
withTemporaryDirectory action = getTemporaryDirectory >>= action
