{-# LANGUAGE FlexibleContexts #-}

module Axel.Eval where

import Axel.Error (Error(MacroError))
import Axel.GHC (buildWithGHC, extractInvalidDefinitionNames, runWithGHC)
import Axel.Utils.Directory (withCurrentDirectoryLifted, withTempDirectory)

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (writeFile)

execInterpreter ::
     (MonadError Error m, MonadIO m)
  => FilePath
  -> FilePath
  -> m (Either (String, [String]) String)
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
        _ -> pure $ Left (stderr, invalidDefinitionNames)

evalMacro ::
     (MonadBaseControl IO m, MonadError Error m, MonadIO m)
  => String
  -> String
  -> String
  -> m (Either (String, [String]) String)
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
