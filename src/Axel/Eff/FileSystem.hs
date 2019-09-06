{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Eff.FileSystem where

import Prelude hiding (readFile, writeFile)
import qualified Prelude (writeFile)

import Control.Monad (forM)

import qualified Polysemy as Sem

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

data FileSystem m a where
  AppendFile :: String -> FilePath -> FileSystem m ()
  CopyFile :: FilePath -> FilePath -> FileSystem m ()
  CreateDirectoryIfMissing :: Bool -> FilePath -> FileSystem m ()
  DoesDirectoryExist :: FilePath -> FileSystem m Bool
  GetCurrentDirectory :: FileSystem m FilePath
  GetDirectoryContents :: FilePath -> FileSystem m [FilePath]
  GetTemporaryDirectory :: FileSystem m FilePath
  ReadFile :: FilePath -> FileSystem m String
  RemoveFile :: FilePath -> FileSystem m ()
  SetCurrentDirectory :: FilePath -> FileSystem m ()
  WriteFile :: String -> FilePath -> FileSystem m ()

Sem.makeSem ''FileSystem

runFileSystem ::
     (Sem.Member (Sem.Embed IO) effs)
  => Sem.Sem (FileSystem ': effs) a
  -> Sem.Sem effs a
runFileSystem =
  Sem.interpret
    (\case
       AppendFile path contents -> Sem.embed $ Prelude.appendFile path contents
       CopyFile src dest -> Sem.embed $ System.Directory.copyFile src dest
       CreateDirectoryIfMissing createParentDirs path ->
         Sem.embed $
         System.Directory.createDirectoryIfMissing createParentDirs path
       DoesDirectoryExist path ->
         Sem.embed $ System.Directory.doesDirectoryExist path
       GetCurrentDirectory -> Sem.embed System.Directory.getCurrentDirectory
       GetDirectoryContents path ->
         Sem.embed $ System.Directory.getDirectoryContents path
       GetTemporaryDirectory -> Sem.embed System.Directory.getTemporaryDirectory
       ReadFile path -> Sem.embed $ S.readFile path
       RemoveFile path -> Sem.embed $ System.Directory.removeFile path
       SetCurrentDirectory path ->
         Sem.embed $ System.Directory.setCurrentDirectory path
       WriteFile path contents -> Sem.embed $ Prelude.writeFile path contents)

-- Adapted from http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html.
getDirectoryContentsRec ::
     (Sem.Member FileSystem effs) => FilePath -> Sem.Sem effs [FilePath]
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
