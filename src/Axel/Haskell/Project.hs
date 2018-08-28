{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Axel.Haskell.Project where

import Axel.Error (Error)
import Axel.Haskell.File (transpileFile')
import Axel.Monad.FileSystem
  ( MonadFileSystem(copyFile, getCurrentDirectory, removeFile)
  , getDirectoryContentsRec
  )
import Axel.Monad.Haskell.Stack
  ( MonadStackProject(addStackDependency, buildStackProject,
                  createStackProject, runStackProject)
  , axelStackageSpecifier
  )
import Axel.Monad.Process (MonadProcess)
import Axel.Monad.Resource (MonadResource(getResourcePath), newProjectTemplate)

import Control.Monad.Except (MonadError)

import Data.Semigroup ((<>))
import qualified Data.Text as T (isSuffixOf, pack)

import System.FilePath ((</>))

type ProjectPath = FilePath

newProject ::
     (MonadFileSystem m, MonadResource m, MonadStackProject m) => String -> m ()
newProject projectName = do
  createStackProject projectName
  addStackDependency axelStackageSpecifier projectName
  templatePath <- getResourcePath newProjectTemplate
  let copyAxel filePath = do
        copyFile
          (templatePath </> filePath <> ".axel")
          (projectName </> filePath <> ".axel")
        removeFile (projectName </> filePath <> ".hs")
  mapM_ copyAxel ["Setup", "app" </> "Main", "src" </> "Lib", "test" </> "Spec"]

transpileProject ::
     (MonadError Error m, MonadFileSystem m, MonadProcess m, MonadResource m)
  => m [FilePath]
transpileProject = do
  files <- getDirectoryContentsRec "."
  let axelFiles =
        filter (\filePath -> ".axel" `T.isSuffixOf` T.pack filePath) files
  mapM transpileFile' axelFiles

buildProject ::
     ( MonadError Error m
     , MonadFileSystem m
     , MonadProcess m
     , MonadResource m
     , MonadStackProject m
     )
  => m ()
buildProject = do
  projectPath <- getCurrentDirectory
  hsPaths <- transpileProject
  buildStackProject projectPath
  mapM_ removeFile hsPaths

runProject ::
     (MonadError Error m, MonadFileSystem m, MonadStackProject m) => m ()
runProject = getCurrentDirectory >>= runStackProject
