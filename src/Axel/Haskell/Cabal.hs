{-# LANGUAGE AllowAmbiguousTypes #-}

module Axel.Haskell.Cabal where

import Axel.Prelude

import Axel.Eff.Console (putStr, putStrLn)
import qualified Axel.Eff.Console as Effs
import Axel.Eff.Error (Error(ProjectError), fatal)
import qualified Axel.Eff.FileSystem as FS
import qualified Axel.Eff.FileSystem as Effs
import Axel.Eff.Process
  ( createIndependentProcess
  , handleGetLine
  , handleIsAtEnd
  , passthroughProcess
  , readProcess
  , waitOnProcess
  )
import qualified Axel.Eff.Process as Effs
import Axel.Haskell.Error (processStackOutputLine)
import Axel.Sourcemap (ModuleInfo)
import Axel.Utils.FilePath (takeFileName)
import Axel.Utils.Monad (whileM)

import Control.Lens (op)
import Control.Lens.Operators ((%~), (^?!))
import Control.Monad (void)

import Data.Aeson.Key (toText)
import Data.Aeson.KeyMap (keys)
import Data.Aeson.Lens (_Array, _Object, key)
import Data.Function ((&))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vector (cons)
import Data.Version (showVersion)
import qualified Data.Yaml as Yaml

import Effectful ((:>), (:>>))
import qualified Effectful as Eff
import qualified Effectful.Error.Static as Eff

import Paths_axel (version)

import System.Exit (ExitCode(ExitFailure, ExitSuccess))

type ProjectPath = FilePath

type StackageId = Text

type StackageResolver = Text

type Target = Text

type Version = Text

axelVersion :: Version
axelVersion = T.pack $ showVersion version

axelPackageId :: StackageId
axelPackageId = "axel"

getProjectExecutableTargets ::
     (Effs.FileSystem :> effs) => ProjectPath -> Eff.Eff effs [Target]
getProjectExecutableTargets projectPath =
  FS.withCurrentDirectory projectPath $ do
    config <- readPackageConfig
    pure $ map toText $ keys $ config ^?! key "executables" . _Object

packageConfigRelativePath :: FilePath
packageConfigRelativePath = FilePath "package.yaml"

readPackageConfig :: (Effs.FileSystem :> effs) => Eff.Eff effs Yaml.Value
readPackageConfig = do
  packageConfigContents <- FS.readFile packageConfigRelativePath
  case Yaml.decodeEither' $ T.encodeUtf8 packageConfigContents of
    Right contents -> pure contents
    Left _ -> fatal "readPackageConfig" "0001"

addDependency ::
     (Effs.FileSystem :> effs) => StackageId -> ProjectPath -> Eff.Eff effs ()
addDependency dependencyId projectPath =
  FS.withCurrentDirectory projectPath $ do
    config <- readPackageConfig
    let newContents :: Yaml.Value =
          config & key "dependencies" . _Array %~
          cons (Yaml.String dependencyId)
        encodedContents = T.decodeUtf8 $ Yaml.encode newContents
    FS.writeFile packageConfigRelativePath encodedContents

buildProject ::
     ('[ Effs.Console, Eff.Error Error, Effs.FileSystem, Effs.Process] :>> effs)
  => ModuleInfo
  -> ProjectPath
  -> Eff.Eff effs ()
buildProject moduleInfo projectPath = do
  FS.withCurrentDirectory projectPath $ do
    putStrLn ("Building " <> op FilePath (takeFileName projectPath) <> "...")
    (_, _, stderrHandle, processHandle) <-
      createIndependentProcess "cabal build --ghc-options='-ddump-json'"
    whileM (not <$> handleIsAtEnd stderrHandle) $ do
      stackOutputLine <- handleGetLine stderrHandle
      putStr $ T.unlines $ processStackOutputLine moduleInfo stackOutputLine
    exitCode <- waitOnProcess processHandle
    case exitCode of
      ExitSuccess -> pure ()
      ExitFailure _ -> Eff.throwError $ ProjectError "Project failed to build."

createProject ::
     ('[ Effs.FileSystem, Effs.Process] :>> effs) => Text -> Eff.Eff effs ()
createProject projectName =
  void $ readProcess ("cabal new " <> projectName <> " new-template")

runProject ::
     ('[ Effs.Console, Eff.Error Error, Effs.FileSystem, Effs.Process] :>> effs)
  => ProjectPath
  -> Eff.Eff effs ()
runProject projectPath = do
  targets <- getProjectExecutableTargets projectPath
  case targets of
    [target] -> do
      putStrLn $ "Running " <> target <> "..."
      void $ passthroughProcess ("cabal run " <> target)
    _ ->
      Eff.throwError $
      ProjectError "No executable target was unambiguously found!"

includeAxelArguments :: Text
includeAxelArguments = T.unwords ["--package", axelPackageId]
