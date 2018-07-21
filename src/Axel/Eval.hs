{-# LANGUAGE FlexibleContexts #-}

module Axel.Eval where

import Axel.Error (Error(MacroError))
import Axel.GHC (buildWithGHC, extractInvalidDefinitionNames, runWithGHC)

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl, control)

import System.Directory
  ( createDirectoryIfMissing
  , getTemporaryDirectory
  , withCurrentDirectory
  )
import System.FilePath ((</>))
import System.IO (writeFile)

withTempDirectory :: (MonadIO m) => (FilePath -> m a) -> m a
withTempDirectory f = do
  temporaryDirectory <- liftIO getTemporaryDirectory
  liftIO $ createDirectoryIfMissing True temporaryDirectory
  result <- f temporaryDirectory
  pure result

withCurrentDirectoryLifted :: (MonadBaseControl IO m) => FilePath -> m a -> m a
withCurrentDirectoryLifted directory f =
  control $ \runInIO -> withCurrentDirectory directory (runInIO f)

execInterpreter ::
     (MonadError Error m, MonadIO m)
  => FilePath
  -> FilePath
  -> m (Either [String] String)
execInterpreter scaffoldFilePath macroDefinitionAndEnvironmentFilePath = do
  debugResult <- liftIO $ buildWithGHC scaffoldFilePath
  case debugResult of
    Right _ -> Right <$> runWithGHC scaffoldFilePath
    Left (jsonLog, stderr) -> do
      invalidDefinitionNames <-
        liftIO $
        extractInvalidDefinitionNames
          macroDefinitionAndEnvironmentFilePath
          jsonLog
      case invalidDefinitionNames of
        [] -> throwError $ MacroError stderr
        _ -> pure $ Left invalidDefinitionNames

evalMacro ::
     (MonadBaseControl IO m, MonadError Error m, MonadIO m)
  => String
  -> String
  -> String
  -> m (Either [String] String)
evalMacro astDefinition scaffold macroDefinitionAndEnvironment =
  withTempDirectory $ \directoryName ->
    withCurrentDirectoryLifted directoryName $ do
      let astDirectoryPath = "Axel" </> "Parse"
      let macroDefinitionAndEnvironmentFileName =
            "MacroDefinitionAndEnvironment.hs"
      let scaffoldFileName = "Scaffold.hs"
      liftIO $ createDirectoryIfMissing True astDirectoryPath
      liftIO $ writeFile (astDirectoryPath </> "AST.hs") astDefinition
      liftIO $
        writeFile
          macroDefinitionAndEnvironmentFileName
          macroDefinitionAndEnvironment
      liftIO $ writeFile scaffoldFileName scaffold
      execInterpreter scaffoldFileName macroDefinitionAndEnvironmentFileName
