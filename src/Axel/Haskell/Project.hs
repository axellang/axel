{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Axel.Haskell.Project where

import qualified Axel.Eff.Console as Effs (Console)
import Axel.Eff.FileSystem
  ( copyFile
  , getCurrentDirectory
  , getDirectoryContentsRec
  , removeFile
  )
import qualified Axel.Eff.FileSystem as Effs (FileSystem)
import qualified Axel.Eff.Process as Effs (Process)
import Axel.Eff.Resource (getResourcePath, newProjectTemplate)
import qualified Axel.Eff.Resource as Effs (Resource)
import Axel.Error (Error)
import Axel.Haskell.File (transpileFile')
import Axel.Haskell.Stack
  ( addStackDependency
  , axelStackageId
  , buildStackProject
  , createStackProject
  , runStackProject
  )

import Control.Monad (void)
import Control.Monad.Freer (Eff, Members)
import qualified Control.Monad.Freer.Error as Effs (Error)

import Data.Semigroup ((<>))
import qualified Data.Text as T (isSuffixOf, pack)

import System.FilePath ((</>))

type ProjectPath = FilePath

newProject ::
     Members '[ Effs.FileSystem, Effs.Process, Effs.Resource] effs
  => String
  -> Eff effs ()
newProject projectName = do
  createStackProject projectName
  addStackDependency axelStackageId projectName
  templatePath <- getResourcePath newProjectTemplate
  let copyAxel filePath = do
        copyFile
          (templatePath </> filePath <> ".axel")
          (projectName </> filePath <> ".axel")
        removeFile (projectName </> filePath <> ".hs")
  mapM_ copyAxel ["Setup", "app" </> "Main", "src" </> "Lib", "test" </> "Spec"]

transpileProject ::
     (Members '[ Effs.Console, Effs.Error Error, Effs.FileSystem, Effs.Process, Effs.Resource] effs)
  => Eff effs [FilePath]
transpileProject = do
  files <- getDirectoryContentsRec "."
  let axelFiles =
        filter (\filePath -> ".axel" `T.isSuffixOf` T.pack filePath) files
  mapM transpileFile' axelFiles

buildProject ::
     (Members '[ Effs.Console, Effs.Error Error, Effs.FileSystem, Effs.Process, Effs.Resource] effs)
  => Eff effs ()
buildProject = do
  projectPath <- getCurrentDirectory
  void $ transpileProject
  buildStackProject projectPath

runProject ::
     (Members '[ Effs.Console, Effs.Error Error, Effs.FileSystem, Effs.Process] effs)
  => Eff effs ()
runProject = getCurrentDirectory >>= runStackProject
