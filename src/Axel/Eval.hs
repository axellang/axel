{-# LANGUAGE FlexibleContexts #-}

module Axel.Eval where

import Axel.Error (Error(MacroError))
import Axel.GHC (buildWithGHC, runWithGHC)
import Axel.Utils.Directory (withCurrentDirectoryLifted, withTempDirectory)

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (writeFile)

execInterpreter :: (MonadError Error m, MonadIO m) => FilePath -> m String
execInterpreter scaffoldFilePath = do
  debugResult <- liftIO $ buildWithGHC scaffoldFilePath
  case debugResult of
    Right _ -> runWithGHC scaffoldFilePath
    Left stderr -> throwError $ MacroError stderr

evalMacro ::
     (MonadBaseControl IO m, MonadError Error m, MonadIO m)
  => String
  -> String
  -> String
  -> m String
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
      execInterpreter scaffoldFileName
