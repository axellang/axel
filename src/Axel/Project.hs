{-# LANGUAGE OverloadedStrings #-}

module Axel.Project where

import Axel.Entry (transpileFile')
import Axel.Utils.Directory (getRecursiveContents)

import Control.Lens ((%~))
import Control.Monad (void)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson.Lens (_Array, key)
import Data.Function ((&))
import Data.List (foldl')
import Data.Semigroup ((<>))
import qualified Data.Text as T (append, isSuffixOf, pack)
import Data.Vector (cons)
import Data.Version (showVersion)
import Data.Yaml (Value(String), decodeFileEither, encodeFile)

import Paths_axel (getDataFileName, version)

import System.Directory (copyFile, getCurrentDirectory, removeFile)
import System.FilePath ((</>))
import System.Process (readProcess, readProcessWithExitCode)
import System.Process.Typed (proc, runProcess)

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

addAxelDependency :: IO ()
addAxelDependency = do
  let packageConfigPath = "package.yaml"
  let axelHackageVersion = T.pack $ showVersion version
  decodeResult <- decodeFileEither packageConfigPath
  case decodeResult of
    Right contents ->
      let newContents :: Value
          newContents =
            contents & key "dependencies" . _Array %~
            cons (String $ T.append "axel ==" axelHackageVersion)
      in encodeFile packageConfigPath newContents
    Left err ->
      throwError
        (userError $ "`package.yaml` could not be parsed: " <> show err)

buildProject :: IO ()
buildProject = do
  hsPaths <- transpileProject
  addAxelDependency
  void $ readProcess "stack" ["build"] ""
  mapM_ removeFile hsPaths

runProject :: IO ()
runProject = do
  (_, _, stderr) <- readProcessWithExitCode "stack" ["ide", "targets"] ""
  let targets = lines stderr
  case findExeTargets targets of
    [target] -> do
      liftIO $ putStrLn ("Running " <> target <> "...")
      void $ runProcess $ proc "stack" ["exec", target]
    _ -> throwError (userError "No executable target was unambiguously found!")
  where
    findExeTargets =
      foldl'
        (\acc target ->
           case getAllTextSubmatches $ target =~
                ("([^:]*):exe:([^:]*)" :: String) of
             [_fullMatch, _projectName, targetName] -> targetName : acc
             _ -> acc)
        []
