{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Axel.Haskell.Stack where

import Prelude hiding (putStrLn)

import Axel.Error (Error(ProjectError), fatal)
import Axel.Monad.Console (MonadConsole, putStrLn)
import Axel.Monad.FileSystem (MonadFileSystem)
import qualified Axel.Monad.FileSystem as FS
  ( MonadFileSystem(readFile, writeFile)
  , withCurrentDirectory
  )
import Axel.Monad.Process
  ( MonadProcess(runProcess, runProcessInheritingStreams)
  )
import Axel.Utils.Debug

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

getStackProjectTargets ::
     (Monad m, MonadFileSystem m, MonadProcess m) => ProjectPath -> m [Target]
getStackProjectTargets projectPath =
  FS.withCurrentDirectory projectPath $ do
    (_, _, stderr) <- runProcess "stack" ["ide", "targets"] ""
    pure $ lines stderr

addStackDependency :: (MonadFileSystem m) => StackageId -> ProjectPath -> m ()
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

buildStackProject :: (MonadFileSystem m, MonadProcess m) => ProjectPath -> m ()
buildStackProject projectPath =
  void $ FS.withCurrentDirectory projectPath $ runProcess "stack" ["build"] ""

createStackProject :: (MonadFileSystem m, MonadProcess m) => String -> m ()
createStackProject projectName = do
  void $ runProcess "stack" ["new", projectName, "new-template"] ""
  setStackageResolver projectName stackageResolverWithAxel

runStackProject ::
     (MonadConsole m, MonadError Error m, MonadFileSystem m, MonadProcess m)
  => ProjectPath
  -> m ()
runStackProject projectPath = do
  targets <- getStackProjectTargets projectPath
  case findExeTargets targets of
    [target] -> do
      putStrLn ("Running " <> target <> "...")
      void $ runProcessInheritingStreams "stack" ["exec", target]
    _ ->
      throwError $ ProjectError "No executable target was unambiguously found!"
  where
    findExeTargets =
      foldl'
        (\acc target ->
           case getAllTextSubmatches $ target =~
                ("([^:]*):exe:([^:]*)" :: String) of
             [_fullMatch, _projectName, targetName] -> targetName : acc
             _ -> acc)
        []

setStackageResolver ::
     (MonadFileSystem m, MonadProcess m)
  => StackageResolver
  -> ProjectPath
  -> m ()
setStackageResolver resolver projectPath =
  void $ FS.withCurrentDirectory projectPath $
  runProcess "stack" ["config", "set", "resolver", resolver] ""
