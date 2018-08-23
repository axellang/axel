{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Axel.Monad.Haskell.Stack where

import Axel.Error (Error(ProjectError), fatal)
import Axel.Monad.FileSystem (MonadFileSystem)
import qualified Axel.Monad.FileSystem as FS
  ( MonadFileSystem(readFile, withCurrentDirectory, writeFile)
  )
import Axel.Monad.Output (MonadOutput(outputStrLn))
import Axel.Monad.Process
  ( MonadProcess(readProcess, readProcessWithExitCode,
             runProcessInheritingStreams)
  )

import Control.Lens.Operators ((%~))
import Control.Monad (void)
import Control.Monad.Except (MonadError, throwError)

import Data.Aeson.Lens (_Array, key)
import qualified Data.ByteString.Char8 as B (pack, unpack)
import Data.Function ((&))
import Data.List (foldl')
import qualified Data.Text as T (pack)
import Data.Vector (cons)
import Data.Version (showVersion)
import qualified Data.Yaml as Yaml (Value(String), decodeEither', encode)

import Paths_axel (version)

import System.Process.Typed (proc)

import Text.Regex.PCRE ((=~), getAllTextSubmatches)

type ProjectPath = FilePath

type StackageId = String

type StackageResolver = String

type Target = String

type Version = String

stackageResolverWithAxel :: StackageResolver
stackageResolverWithAxel = "nightly-2018-08-20"

axelStackageVersion :: Version
axelStackageVersion = showVersion version

axelStackageId :: StackageId
axelStackageId = "axel-" <> showVersion version

axelStackageSpecifier :: StackageId
axelStackageSpecifier = "axel ==" <> axelStackageVersion

class MonadStackProject m where
  addStackDependency :: StackageId -> ProjectPath -> m ()
  buildStackProject :: ProjectPath -> m ()
  createStackProject :: String -> m ()
  runStackProject :: ProjectPath -> m ()
  setStackageResolver :: StackageResolver -> ProjectPath -> m ()

getStackProjectTargets ::
     (Monad m, MonadFileSystem m, MonadProcess m) => ProjectPath -> m [Target]
getStackProjectTargets projectPath =
  FS.withCurrentDirectory projectPath $ do
    (_, _, stderr) <- readProcessWithExitCode "stack" ["ide", "targets"] ""
    pure $ lines stderr

-- NOTE This is undecidable, but `mtl` uses undecidable instances in this scenario(?)....
--      Plus, I can't actually come up with a better solution.
instance (MonadError Error m, MonadFileSystem m, MonadOutput m, MonadProcess m) =>
         MonadStackProject m where
  addStackDependency :: StackageId -> ProjectPath -> m ()
  addStackDependency dependencyId projectPath =
    FS.withCurrentDirectory projectPath $ do
      let packageConfigPath = "package.yaml"
      packageConfigContents <- FS.readFile packageConfigPath
      case Yaml.decodeEither' $ B.pack packageConfigContents of
        Right contents ->
          let newContents :: Yaml.Value =
                contents & key "dependencies" . _Array %~
                cons (Yaml.String $ T.pack dependencyId)
              encodedContents = B.unpack $ Yaml.encode newContents
           in FS.writeFile packageConfigPath encodedContents
        Left _ -> fatal "addStackDependency" "0001"
  buildStackProject :: ProjectPath -> m ()
  buildStackProject projectPath =
    void $ FS.withCurrentDirectory projectPath $
    readProcess "stack" ["build"] ""
  createStackProject :: String -> m ()
  createStackProject projectName = do
    void $ readProcess "stack" ["new", projectName, "new-template"] ""
    setStackageResolver projectName stackageResolverWithAxel
  runStackProject :: ProjectPath -> m ()
  runStackProject projectPath = do
    targets <- getStackProjectTargets projectPath
    case findExeTargets targets of
      [target] -> do
        outputStrLn ("Running " <> target <> "...")
        void $ runProcessInheritingStreams $ proc "stack" ["exec", target]
      _ ->
        throwError $
        ProjectError "No executable target was unambiguously found!"
    where
      findExeTargets =
        foldl'
          (\acc target ->
             case getAllTextSubmatches $ target =~
                  ("([^:]*):exe:([^:]*)" :: String) of
               [_fullMatch, _projectName, targetName] -> targetName : acc
               _ -> acc)
          []
  setStackageResolver :: StackageResolver -> ProjectPath -> m ()
  setStackageResolver resolver projectPath =
    void $ FS.withCurrentDirectory projectPath $
    readProcess "stack" ["config", "set", "resolver", resolver] ""
