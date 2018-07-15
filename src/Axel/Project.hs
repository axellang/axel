{-# LANGUAGE OverloadedStrings #-}

module Axel.Project where

import Axel.Entry (transpileFile')
import Axel.Utils.Directory (getRecursiveContents)

import Control.Monad (void)

import Data.Semigroup ((<>))
import qualified Data.Text as T (isSuffixOf, pack)

import Paths_axel (getDataFileName)

import System.Directory (copyFile, getCurrentDirectory, removeFile)
import System.FilePath ((</>))
import System.Process (readProcess)

newProject :: String -> IO ()
newProject projectName = do
  currentDirectory <- getCurrentDirectory
  void $ readProcess "stack" ["new", projectName, "new-template"] ""
  let projectDirectory = currentDirectory </> projectName
  templatePath <- getDataFileName ("resources" </> "new-project-template")
  let copyAxel filePath = do
        copyFile
          (templatePath </> filePath <> ".axel")
          (projectDirectory </> filePath <> ".axel")
        removeFile (projectDirectory </> filePath <> ".hs")
  mapM_ copyAxel ["Setup", "app" </> "Main", "src" </> "Lib", "test" </> "Spec"]

transpileProject :: IO ()
transpileProject = do
  files <- getRecursiveContents "."
  let axelFiles =
        filter (\filePath -> ".axel" `T.isSuffixOf` T.pack filePath) files
  newPaths <- mapM transpileFile' axelFiles
  mapM_ removeFile newPaths
