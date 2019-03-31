{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Axel.Haskell.Project where

import qualified Axel.Eff.Console as Effs (Console)
import Axel.Eff.FileSystem
  ( copyFile
  , getCurrentDirectory
  , getDirectoryContentsRec
  , removeFile
  )
import qualified Axel.Eff.FileSystem as Effs (FileSystem)
import qualified Axel.Eff.Ghci as Effs (Ghci)
import qualified Axel.Eff.Process as Effs (Process)
import Axel.Eff.Resource (getResourcePath, newProjectTemplate)
import qualified Axel.Eff.Resource as Effs (Resource)
import Axel.Haskell.File (readModuleInfo, transpileFile')
import Axel.Haskell.Stack
  ( addStackDependency
  , axelStackageId
  , buildStackProject
  , createStackProject
  , runStackProject
  )
import qualified Axel.Parse.AST as Parse (SourceMetadata)
import qualified Axel.Sourcemap as SM (Error)

import Control.Monad (void)
import Control.Monad.Freer (Eff, Members)
import qualified Control.Monad.Freer.Error as Effs (Error)
import Control.Monad.Freer.State (evalState)

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
     (Members '[ Effs.Console, Effs.Error SM.Error, Effs.FileSystem, Effs.Ghci, Effs.Process, Effs.Resource] effs)
  => Eff effs [FilePath]
transpileProject = do
  files <- concat <$> mapM getDirectoryContentsRec ["app", "src", "test"]
  let axelFiles =
        filter (\filePath -> ".axel" `T.isSuffixOf` T.pack filePath) files
  moduleInfo <- readModuleInfo axelFiles
  evalState moduleInfo $ mapM transpileFile' axelFiles

buildProject ::
     (Members '[ Effs.Console, Effs.Error SM.Error, Effs.FileSystem, Effs.Ghci, Effs.Process, Effs.Resource] effs)
  => Eff effs ()
buildProject = do
  projectPath <- getCurrentDirectory
  void transpileProject
  buildStackProject @Parse.SourceMetadata projectPath

runProject ::
     (Members '[ Effs.Console, Effs.Error SM.Error, Effs.FileSystem, Effs.Process] effs)
  => Eff effs ()
runProject = getCurrentDirectory >>= runStackProject @Parse.SourceMetadata
