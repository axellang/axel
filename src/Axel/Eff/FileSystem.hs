{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Eff.FileSystem where

import Prelude hiding (readFile, writeFile)
import qualified Prelude (readFile, writeFile)

import Control.Monad (forM)
import Control.Monad.Freer
  ( type (~>)
  , Eff
  , LastMember
  , Member
  , interpretM
  , send
  )

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

data FileSystem a where
  CopyFile :: FilePath -> FilePath -> FileSystem ()
  CreateDirectoryIfMissing :: Bool -> FilePath -> FileSystem ()
  DoesDirectoryExist :: FilePath -> FileSystem Bool
  GetCurrentDirectory :: FileSystem FilePath
  GetDirectoryContents :: FilePath -> FileSystem [FilePath]
  GetTemporaryDirectory :: FileSystem FilePath
  ReadFile :: FilePath -> FileSystem String
  RemoveFile :: FilePath -> FileSystem ()
  SetCurrentDirectory :: FilePath -> FileSystem ()
  WriteFile :: String -> FilePath -> FileSystem ()

copyFile :: (Member FileSystem effs) => FilePath -> FilePath -> Eff effs ()
copyFile src dest = send $ CopyFile src dest

createDirectoryIfMissing ::
     (Member FileSystem effs) => Bool -> FilePath -> Eff effs ()
createDirectoryIfMissing createParentDirs path =
  send $ CreateDirectoryIfMissing createParentDirs path

doesDirectoryExist :: (Member FileSystem effs) => FilePath -> Eff effs Bool
doesDirectoryExist = send . DoesDirectoryExist

getCurrentDirectory :: (Member FileSystem effs) => Eff effs FilePath
getCurrentDirectory = send GetCurrentDirectory

getDirectoryContents ::
     (Member FileSystem effs) => FilePath -> Eff effs [FilePath]
getDirectoryContents = send . GetDirectoryContents

getTemporaryDirectory :: (Member FileSystem effs) => Eff effs FilePath
getTemporaryDirectory = send GetTemporaryDirectory

readFile :: (Member FileSystem effs) => FilePath -> Eff effs String
readFile = send . ReadFile

removeFile :: (Member FileSystem effs) => FilePath -> Eff effs ()
removeFile = send . RemoveFile

setCurrentDirectory :: (Member FileSystem effs) => FilePath -> Eff effs ()
setCurrentDirectory = send . SetCurrentDirectory

writeFile :: (Member FileSystem effs) => String -> FilePath -> Eff effs ()
writeFile contents path = send $ WriteFile contents path

runEff :: (LastMember IO effs) => Eff (FileSystem ': effs) ~> Eff effs
runEff =
  interpretM
    (\case
       CopyFile src dest -> System.Directory.copyFile src dest
       CreateDirectoryIfMissing createParentDirs path ->
         System.Directory.createDirectoryIfMissing createParentDirs path
       DoesDirectoryExist path -> System.Directory.doesDirectoryExist path
       GetCurrentDirectory -> System.Directory.getCurrentDirectory
       GetDirectoryContents path -> System.Directory.getDirectoryContents path
       GetTemporaryDirectory -> System.Directory.getTemporaryDirectory
       ReadFile path -> S.readFile path
       RemoveFile path -> System.Directory.removeFile path
       SetCurrentDirectory path -> System.Directory.setCurrentDirectory path
       WriteFile path contents -> Prelude.writeFile path contents)

-- Adapted from http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html.
getDirectoryContentsRec ::
     (Member FileSystem effs) => FilePath -> Eff effs [FilePath]
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
     (Member FileSystem effs) => FilePath -> Eff effs a -> Eff effs a
withCurrentDirectory directory f = do
  originalDirectory <- getCurrentDirectory
  setCurrentDirectory directory
  result <- f
  setCurrentDirectory originalDirectory
  pure result

withTemporaryDirectory ::
     (Member FileSystem effs) => (FilePath -> Eff effs a) -> Eff effs a
withTemporaryDirectory action = getTemporaryDirectory >>= action
