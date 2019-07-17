{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Axel.Haskell.Stack where

import Prelude hiding (putStrLn)

import Axel.Eff.Console (putStrLn)
import qualified Axel.Eff.Console as Effs (Console)
import qualified Axel.Eff.FileSystem as FS
  ( readFile
  , withCurrentDirectory
  , writeFile
  )
import qualified Axel.Eff.FileSystem as Effs (FileSystem)
import Axel.Eff.Process
  ( ProcessRunner
  , StreamSpecification(CreateStreams, InheritStreams)
  , runProcess
  )
import qualified Axel.Eff.Process as Effs (Process)
import Axel.Error (Error(ProjectError), fatal)
import Axel.Haskell.Error (processErrors)
import Axel.Macros (ModuleInfo, getTranspiledFiles)

import Control.Lens.Operators ((%~))
import Control.Monad (void)
import Control.Monad.Freer (Eff, Member, Members)
import Control.Monad.Freer.Error (throwError)
import qualified Control.Monad.Freer.Error as Effs (Error)

import Data.Aeson.Lens (_Array, key)
import qualified Data.ByteString.Char8 as B (pack, unpack)
import Data.Function ((&))
import Data.List (foldl')
import qualified Data.Text as T (pack)
import Data.Vector (cons)
import Data.Version (showVersion)
import qualified Data.Yaml as Yaml (Value(String), decodeEither', encode)

import Paths_axel (version)

import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath (takeFileName)

import Text.Regex.PCRE ((=~), getAllTextSubmatches)

type ProjectPath = FilePath

type StackageId = String

type StackageResolver = String

type Target = String

type Version = String

stackageResolverWithAxel :: StackageResolver
stackageResolverWithAxel = "nightly"

axelStackageVersion :: Version
axelStackageVersion = showVersion version

axelStackageId :: StackageId
axelStackageId = "axel"

getStackProjectTargets ::
     (Members '[ Effs.FileSystem, Effs.Process] effs)
  => ProjectPath
  -> Eff effs [Target]
getStackProjectTargets projectPath =
  FS.withCurrentDirectory projectPath $ do
    (_, _, stderr) <- runProcess @'CreateStreams "stack ide targets" ""
    pure $ lines stderr

addStackDependency ::
     (Member Effs.FileSystem effs) => StackageId -> ProjectPath -> Eff effs ()
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

buildStackProject ::
     forall ann effs.
     (Members '[ Effs.Console, Effs.Error (Error ann), Effs.FileSystem, Effs.Process] effs)
  => ModuleInfo
  -> ProjectPath
  -> Eff effs ()
buildStackProject moduleInfo projectPath = do
  putStrLn ("Building " <> takeFileName projectPath <> "...")
  result <-
    FS.withCurrentDirectory projectPath $
    runProcess @'CreateStreams "stack build --ghc-options='-ddump-json'" ""
  case result of
    (ExitSuccess, _, _) -> pure ()
    (ExitFailure _, _, stderr) ->
      throwError @(Error ann) $
      ProjectError
        ("Project failed to build.\n\n" <>
         processErrors (getTranspiledFiles moduleInfo) stderr)

createStackProject ::
     (Members '[ Effs.FileSystem, Effs.Process] effs) => String -> Eff effs ()
createStackProject projectName = do
  void $
    runProcess
      @'CreateStreams
      ("stack new " <> projectName <> " new-template")
      ""
  setStackageResolver projectName stackageResolverWithAxel

runStackProject ::
     forall ann effs.
     (Members '[ Effs.Console, Effs.Error (Error ann), Effs.FileSystem, Effs.Process] effs)
  => ProjectPath
  -> Eff effs ()
runStackProject projectPath = do
  targets <- getStackProjectTargets projectPath
  case findExeTargets targets of
    [target] -> do
      putStrLn ("Running " <> target <> "...")
      void $ runProcess @'InheritStreams ("stack exec " <> target)
    _ ->
      throwError @(Error ann) $
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

setStackageResolver ::
     (Members '[ Effs.FileSystem, Effs.Process] effs)
  => ProjectPath
  -> StackageResolver
  -> Eff effs ()
setStackageResolver projectPath resolver =
  void $ FS.withCurrentDirectory projectPath $
  runProcess @'CreateStreams ("stack config set resolver " <> resolver) ""

includeAxelArguments :: String
includeAxelArguments =
  unwords ["--resolver", stackageResolverWithAxel, "--package", axelStackageId]

compileFile ::
     forall (streamSpec :: StreamSpecification) effs. (Member Effs.Process effs)
  => FilePath
  -> ProcessRunner streamSpec (Eff effs)
compileFile filePath =
  let cmd = unwords ["stack", "ghc", includeAxelArguments, "--", filePath]
   in runProcess @streamSpec @effs cmd

interpretFile ::
     forall (streamSpec :: StreamSpecification) effs. (Member Effs.Process effs)
  => FilePath
  -> ProcessRunner streamSpec (Eff effs)
interpretFile filePath =
  let cmd = unwords ["stack", "runghc", includeAxelArguments, "--", filePath]
   in runProcess @streamSpec @effs cmd
