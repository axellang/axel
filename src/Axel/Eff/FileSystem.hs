{-# LANGUAGE TemplateHaskell #-}

module Axel.Eff.FileSystem where

import Axel.Prelude

import Axel.Utils.FilePath ((</>))

import Control.Monad (forM)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Effectful ((:>))
import qualified Effectful as Eff
import qualified Effectful.Dispatch.Dynamic as Eff
import qualified Effectful.TH as Eff

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

data FileSystem :: Eff.Effect where
  AppendFile :: FilePath -> Text -> FileSystem m ()
  CopyFile :: FilePath -> FilePath -> FileSystem m ()
  CreateDirectoryIfMissing :: Bool -> FilePath -> FileSystem m ()
  DoesDirectoryExist :: FilePath -> FileSystem m Bool
  GetCurrentDirectory :: FileSystem m FilePath
  GetDirectoryContents :: FilePath -> FileSystem m [FilePath]
  GetTemporaryDirectory :: FileSystem m FilePath
  ReadFile :: FilePath -> FileSystem m Text
  RemoveFile :: FilePath -> FileSystem m ()
  SetCurrentDirectory :: FilePath -> FileSystem m ()
  WriteFile :: FilePath -> Text -> FileSystem m ()

Eff.makeEffect ''FileSystem

runFileSystem ::
     (Eff.IOE :> effs) => Eff.Eff (FileSystem ': effs) a -> Eff.Eff effs a
runFileSystem =
  Eff.interpret $ \_ ->
    \case
      AppendFile (FilePath path) contents ->
        Eff.liftIO $ T.appendFile (T.unpack path) contents
      CopyFile (FilePath src) (FilePath dest) ->
        Eff.liftIO $ System.Directory.copyFile (T.unpack src) (T.unpack dest)
      CreateDirectoryIfMissing createParentDirs (FilePath path) ->
        Eff.liftIO $
        System.Directory.createDirectoryIfMissing
          createParentDirs
          (T.unpack path)
      DoesDirectoryExist (FilePath path) ->
        Eff.liftIO $ System.Directory.doesDirectoryExist (T.unpack path)
      GetCurrentDirectory ->
        Eff.liftIO $ FilePath . T.pack <$> System.Directory.getCurrentDirectory
      GetDirectoryContents (FilePath path) ->
        Eff.liftIO $
        map (FilePath . T.pack) <$>
        System.Directory.getDirectoryContents (T.unpack path)
      GetTemporaryDirectory ->
        Eff.liftIO $
        FilePath . T.pack <$> System.Directory.getTemporaryDirectory
      ReadFile (FilePath path) -> Eff.liftIO $ T.readFile (T.unpack path)
      RemoveFile (FilePath path) ->
        Eff.liftIO $ System.Directory.removeFile (T.unpack path)
      SetCurrentDirectory (FilePath path) ->
        Eff.liftIO $ System.Directory.setCurrentDirectory (T.unpack path)
      WriteFile (FilePath path) contents ->
        Eff.liftIO $ T.writeFile (T.unpack path) contents

-- Adapted from http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html.
getDirectoryContentsRec ::
     (FileSystem :> effs) => FilePath -> Eff.Eff effs [FilePath]
getDirectoryContentsRec dir = do
  names <- getDirectoryContents dir
  let properNames =
        filter (\(FilePath path) -> path `notElem` [".", ".."]) names
  paths <-
    forM properNames $ \name -> do
      let path = dir </> name
      isDirectory <- doesDirectoryExist path
      if isDirectory
        then getDirectoryContentsRec path
        else pure [path]
  pure $ concat paths

withCurrentDirectory ::
     (FileSystem :> effs) => FilePath -> Eff.Eff effs a -> Eff.Eff effs a
withCurrentDirectory directory f = do
  originalDirectory <- getCurrentDirectory
  setCurrentDirectory directory
  result <- f
  setCurrentDirectory originalDirectory
  pure result

withTemporaryDirectory ::
     (FileSystem :> effs) => (FilePath -> Eff.Eff effs a) -> Eff.Eff effs a
withTemporaryDirectory action = getTemporaryDirectory >>= action
