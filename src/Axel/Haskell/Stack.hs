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
import Axel.Eff.Error (Error(ProjectError), fatal)
import qualified Axel.Eff.FileSystem as FS
  ( readFile
  , withCurrentDirectory
  , writeFile
  )
import qualified Axel.Eff.FileSystem as Effs (FileSystem)
import Axel.Eff.Process
  ( ProcessRunner
  , StreamSpecification(CreateStreams, InheritStreams)
  , execProcess
  )
import qualified Axel.Eff.Process as Effs (Process)
import Axel.Haskell.Error (processErrors)
import Axel.Sourcemap (ModuleInfo)
import Control.Lens.Operators ((%~))
import Control.Monad (void)

import Data.Aeson.Lens (_Array, key)
import qualified Data.ByteString.Char8 as B (pack, unpack)
import Data.Function ((&))
import Data.List (foldl')
import qualified Data.Text as T (pack)
import Data.Vector (cons)
import Data.Version (showVersion)
import qualified Data.Yaml as Yaml (Value(String), decodeEither', encode)

import Paths_axel (version)

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem

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
     (Sem.Members '[ Effs.FileSystem, Effs.Process] effs)
  => ProjectPath
  -> Sem.Sem effs [Target]
getStackProjectTargets projectPath =
  FS.withCurrentDirectory projectPath $ do
    (_, _, stderr) <- execProcess @'CreateStreams "stack ide targets" ""
    pure $ lines stderr

addStackDependency ::
     (Sem.Member Effs.FileSystem effs)
  => StackageId
  -> ProjectPath
  -> Sem.Sem effs ()
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
     (Sem.Members '[ Effs.Console, Sem.Error Error, Effs.FileSystem, Effs.Process] effs)
  => ModuleInfo
  -> ProjectPath
  -> Sem.Sem effs ()
buildStackProject moduleInfo projectPath = do
  putStrLn ("Building " <> takeFileName projectPath <> "...")
  result <-
    FS.withCurrentDirectory projectPath $
    execProcess @'CreateStreams "stack build --ghc-options='-ddump-json'" ""
  case result of
    (ExitSuccess, _, _) -> pure ()
    (ExitFailure _, _, stderr) ->
      Sem.throw $
      ProjectError
        ("Project failed to build.\n\n" <> processErrors moduleInfo stderr)

createStackProject ::
     (Sem.Members '[ Effs.FileSystem, Effs.Process] effs)
  => String
  -> Sem.Sem effs ()
createStackProject projectName = do
  void $
    execProcess
      @'CreateStreams
      ("stack new " <> projectName <> " new-template")
      ""
  setStackageResolver projectName stackageResolverWithAxel

runStackProject ::
     (Sem.Members '[ Effs.Console, Sem.Error Error, Effs.FileSystem, Effs.Process] effs)
  => ProjectPath
  -> Sem.Sem effs ()
runStackProject projectPath = do
  targets <- getStackProjectTargets projectPath
  case findExeTargets targets of
    [target] -> do
      putStrLn ("Running " <> target <> "...")
      void $ execProcess @'InheritStreams ("stack exec " <> target)
    _ ->
      Sem.throw $ ProjectError "No executable target was unambiguously found!"
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
     (Sem.Members '[ Effs.FileSystem, Effs.Process] effs)
  => ProjectPath
  -> StackageResolver
  -> Sem.Sem effs ()
setStackageResolver projectPath resolver =
  void $ FS.withCurrentDirectory projectPath $
  execProcess @'CreateStreams ("stack config set resolver " <> resolver) ""

includeAxelArguments :: String
includeAxelArguments =
  unwords ["--resolver", stackageResolverWithAxel, "--package", axelStackageId]

compileFile ::
     forall (streamSpec :: StreamSpecification) effs.
     (Sem.Member Effs.Process effs)
  => FilePath
  -> ProcessRunner streamSpec (Sem.Sem effs)
compileFile filePath =
  let cmd = unwords ["stack", "ghc", includeAxelArguments, "--", filePath]
   in execProcess @streamSpec @effs cmd

interpretFile ::
     forall (streamSpec :: StreamSpecification) effs.
     (Sem.Member Effs.Process effs)
  => FilePath
  -> ProcessRunner streamSpec (Sem.Sem effs)
interpretFile filePath =
  let cmd = unwords ["stack", "runghc", includeAxelArguments, "--", filePath]
   in execProcess @streamSpec @effs cmd
