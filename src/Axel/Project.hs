{-# LANGUAGE OverloadedStrings #-}

module Axel.Project where

import Axel.Entry (transpileFile')
import Axel.Utils.Directory (getRecursiveContents)

import Control.Monad (void)
import Control.Monad.Except (throwError)

import Data.List (foldl')
import Data.Semigroup ((<>))
import qualified Data.Text as T (isSuffixOf, pack)

import Paths_axel (getDataFileName)

import System.Directory (copyFile, getCurrentDirectory, removeFile)
import System.FilePath ((</>))
import System.Process (readProcess, readProcessWithExitCode)

import Text.Regex.PCRE ((=~), getAllTextSubmatches)

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

transpileProject :: IO [FilePath]
transpileProject = do
  files <- getRecursiveContents "."
  let axelFiles =
        filter (\filePath -> ".axel" `T.isSuffixOf` T.pack filePath) files
  mapM transpileFile' axelFiles

buildProject :: IO ()
buildProject = do
  hsPaths <- transpileProject
  void $ readProcess "stack" ["build"] ""
  mapM_ removeFile hsPaths

runProject :: IO ()
runProject = do
  (_, _, stderr) <- readProcessWithExitCode "stack" ["ide", "targets"] ""
  let targets = lines stderr
  case findExeTargets targets of
    [target] -> readProcess "stack" ["exec", target] "" >>= putStr
    _ -> throwError (userError "No executable target was unambiguously found!")
  where
    findExeTargets =
      foldl'
        (\acc target ->
           case getAllTextSubmatches $
                target =~ ("([^:]*):exe:([^:]*)" :: String) of
             [_fullMatch, _projectName, targetName] -> targetName : acc
             _ -> acc)
        []
