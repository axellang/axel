{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Axel.Eff.FileSystem where

import Axel.Prelude

import Control.Monad (forM)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Polysemy as Sem

import Axel.Utils.FilePath ((</>))
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

data FileSystem m a where
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

Sem.makeSem ''FileSystem

runFileSystem ::
     (Sem.Member (Sem.Embed IO) effs)
  => Sem.Sem (FileSystem ': effs) a
  -> Sem.Sem effs a
runFileSystem =
  Sem.interpret
    (\case
       AppendFile (FilePath path) contents ->
         Sem.embed $ T.appendFile (T.unpack path) contents
       CopyFile (FilePath src) (FilePath dest) ->
         Sem.embed $ System.Directory.copyFile (T.unpack src) (T.unpack dest)
       CreateDirectoryIfMissing createParentDirs (FilePath path) ->
         Sem.embed $
         System.Directory.createDirectoryIfMissing
           createParentDirs
           (T.unpack path)
       DoesDirectoryExist (FilePath path) ->
         Sem.embed $ System.Directory.doesDirectoryExist (T.unpack path)
       GetCurrentDirectory ->
         Sem.embed $ FilePath . T.pack <$> System.Directory.getCurrentDirectory
       GetDirectoryContents (FilePath path) ->
         Sem.embed $
         map (FilePath . T.pack) <$>
         System.Directory.getDirectoryContents (T.unpack path)
       GetTemporaryDirectory ->
         Sem.embed $
         FilePath . T.pack <$> System.Directory.getTemporaryDirectory
       ReadFile (FilePath path) -> Sem.embed $ T.readFile (T.unpack path)
       RemoveFile (FilePath path) ->
         Sem.embed $ System.Directory.removeFile (T.unpack path)
       SetCurrentDirectory (FilePath path) ->
         Sem.embed $ System.Directory.setCurrentDirectory (T.unpack path)
       WriteFile (FilePath path) contents ->
         Sem.embed $ T.writeFile (T.unpack path) contents)

-- Adapted from http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html.
getDirectoryContentsRec ::
     (Sem.Member FileSystem effs) => FilePath -> Sem.Sem effs [FilePath]
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
     (Sem.Member FileSystem effs)
  => FilePath
  -> Sem.Sem effs a
  -> Sem.Sem effs a
withCurrentDirectory directory f = do
  originalDirectory <- getCurrentDirectory
  setCurrentDirectory directory
  result <- f
  setCurrentDirectory originalDirectory
  pure result

withTemporaryDirectory ::
     (Sem.Member FileSystem effs)
  => (FilePath -> Sem.Sem effs a)
  -> Sem.Sem effs a
withTemporaryDirectory action = getTemporaryDirectory >>= action
