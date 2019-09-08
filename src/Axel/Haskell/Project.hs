{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Axel.Haskell.Project where

import qualified Axel.Eff.Console as Effs (Console)
import Axel.Eff.Error (Error)
import Axel.Eff.FileSystem
  ( copyFile
  , getCurrentDirectory
  , getDirectoryContentsRec
  , removeFile
  )
import qualified Axel.Eff.FileSystem as Effs (FileSystem)
import qualified Axel.Eff.Ghci as Effs (Ghci)
import qualified Axel.Eff.Ghci as Ghci
import qualified Axel.Eff.Log as Effs (Log)
import qualified Axel.Eff.Process as Effs (Process)
import qualified Axel.Eff.Random as Effs (Random)
import Axel.Eff.Resource (getResourcePath, newProjectTemplate)
import qualified Axel.Eff.Resource as Effs (Resource)
import Axel.Haskell.File (readModuleInfo, transpileFileInPlace)
import Axel.Haskell.Stack
  ( addStackDependency
  , axelStackageId
  , buildStackProject
  , createStackProject
  , runStackProject
  )
import Axel.Sourcemap (ModuleInfo)

import Data.Semigroup ((<>))
import qualified Data.Text as T (isSuffixOf, pack)

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem
import qualified Polysemy.State as Sem

import System.FilePath ((</>))

type ProjectPath = FilePath

newProject ::
     Sem.Members '[ Effs.FileSystem, Effs.Process, Effs.Resource] effs
  => String
  -> Sem.Sem effs ()
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
     (Sem.Members '[ Effs.Console, Sem.Error Error, Effs.FileSystem, Effs.Ghci, Effs.Log, Effs.Process, Effs.Random, Effs.Resource] effs)
  => Sem.Sem effs ModuleInfo
transpileProject =
  Ghci.withGhci $ do
    files <- concat <$> mapM getDirectoryContentsRec ["app", "src", "test"]
    let axelFiles =
          filter (\filePath -> ".axel" `T.isSuffixOf` T.pack filePath) files
    initialModuleInfo <- readModuleInfo axelFiles
    (moduleInfo, _) <-
      Sem.runState initialModuleInfo $ mapM transpileFileInPlace axelFiles
    pure moduleInfo

buildProject ::
     (Sem.Members '[ Effs.Console, Sem.Error Error, Effs.FileSystem, Effs.Ghci, Effs.Log, Effs.Process, Effs.Random, Effs.Resource] effs)
  => Sem.Sem effs ()
buildProject = do
  projectPath <- getCurrentDirectory
  transpiledFiles <- transpileProject
  buildStackProject transpiledFiles projectPath

runProject ::
     (Sem.Members '[ Effs.Console, Sem.Error Error, Effs.FileSystem, Effs.Process] effs)
  => Sem.Sem effs ()
runProject = getCurrentDirectory >>= runStackProject
