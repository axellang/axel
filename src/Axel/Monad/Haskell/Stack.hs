{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Axel.Monad.Haskell.Stack where

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

import Control.Lens.Operators ((%~))
import Control.Monad (void)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Identity (IdentityT)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (RWST)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (RWST)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.State.Lazy as LazyState (StateT)
import qualified Control.Monad.Trans.State.Strict as StrictState (StateT)
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter (WriterT)

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

class (Monad m) =>
      MonadStackProject m
  where
  addStackDependency :: StackageId -> ProjectPath -> m ()
  default addStackDependency :: (MonadTrans t, MonadStackProject m', m ~ t m') =>
    StackageId -> ProjectPath -> m ()
  addStackDependency dependencyId projectPath =
    lift $ addStackDependency dependencyId projectPath
  buildStackProject :: ProjectPath -> m ()
  default buildStackProject :: (MonadTrans t, MonadStackProject m', m ~ t m') =>
    ProjectPath -> m ()
  buildStackProject = lift . buildStackProject
  createStackProject :: String -> m ()
  default createStackProject :: (MonadTrans t, MonadStackProject m', m ~ t m') =>
    String -> m ()
  createStackProject = lift . createStackProject
  runStackProject :: ProjectPath -> m ()
  default runStackProject :: (MonadTrans t, MonadStackProject m', m ~ t m') =>
    ProjectPath -> m ()
  runStackProject = lift . runStackProject
  setStackageResolver :: StackageResolver -> ProjectPath -> m ()
  default setStackageResolver :: (MonadTrans t, MonadStackProject m', m ~ t m') =>
    StackageResolver -> ProjectPath -> m ()
  setStackageResolver resolver projectPath =
    lift $ setStackageResolver resolver projectPath

instance (MonadStackProject m) => MonadStackProject (ContT r m)

instance (MonadStackProject m) => MonadStackProject (ExceptT e m)

instance (MonadStackProject m) => MonadStackProject (IdentityT m)

instance (MonadStackProject m) => MonadStackProject (MaybeT m)

instance (MonadStackProject m) => MonadStackProject (ReaderT r m)

instance (Monoid w, MonadStackProject m) =>
         MonadStackProject (LazyRWS.RWST r w s m)

instance (Monoid w, MonadStackProject m) =>
         MonadStackProject (StrictRWS.RWST r w s m)

instance (MonadStackProject m) => MonadStackProject (LazyState.StateT s m)

instance (MonadStackProject m) =>
         MonadStackProject (StrictState.StateT s m)

instance (Monoid w, MonadStackProject m) =>
         MonadStackProject (LazyWriter.WriterT w m)

instance (Monoid w, MonadStackProject m) =>
         MonadStackProject (StrictWriter.WriterT w m)

getStackProjectTargets ::
     (Monad m, MonadFileSystem m, MonadProcess m) => ProjectPath -> m [Target]
getStackProjectTargets projectPath =
  FS.withCurrentDirectory projectPath $ do
    (_, _, stderr) <- runProcess "stack" ["ide", "targets"] ""
    pure $ lines stderr

instance {-# OVERLAPPABLE #-} ( Monad m
                              , MonadConsole m
                              , MonadError Error m
                              , MonadFileSystem m
                              , MonadProcess m
                              ) =>
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
    void $ FS.withCurrentDirectory projectPath $ runProcess "stack" ["build"] ""
  createStackProject :: String -> m ()
  createStackProject projectName = do
    void $ runProcess "stack" ["new", projectName, "new-template"] ""
    setStackageResolver projectName stackageResolverWithAxel
  runStackProject :: ProjectPath -> m ()
  runStackProject projectPath = do
    targets <- getStackProjectTargets projectPath
    case findExeTargets targets of
      [target] -> do
        putStrLn ("Running " <> target <> "...")
        void $ runProcessInheritingStreams "stack" ["exec", target]
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
    runProcess "stack" ["config", "set", "resolver", resolver] ""
