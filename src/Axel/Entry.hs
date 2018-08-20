{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Axel.Entry where

import Axel.AST (ToHaskell(toHaskell))
import Axel.Error (Error)
import Axel.Haskell.GHC (runWithGHC)
import Axel.Macros (exhaustivelyExpandMacros, stripMacroDefinitions)
import Axel.Normalize (normalizeStatement)
import Axel.Parse (Expression(Symbol), parseSource)
import Axel.Utils.Directory (withTempDirectory)
import Axel.Utils.Recursion (Recursive(bottomUpFmap))
import Axel.Utils.Resources (readResource)
import qualified Axel.Utils.Resources as Res (astDefinition)

import Control.Lens.Operators ((.~))
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)

import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import qualified Data.Text as T (isSuffixOf, pack)

import System.FilePath ((</>), stripExtension)
import System.FilePath.Lens (directory)
import qualified System.IO.Strict as S (readFile)

convertList :: Expression -> Expression
convertList =
  bottomUpFmap $ \case
    Symbol "List" -> Symbol "[]"
    x -> x

convertUnit :: Expression -> Expression
convertUnit =
  bottomUpFmap $ \case
    Symbol "Unit" -> Symbol "()"
    Symbol "unit" -> Symbol "()"
    x -> x

transpileSource ::
     (MonadBaseControl IO m, MonadError Error m, MonadIO m)
  => String
  -> m String
transpileSource source =
  toHaskell . stripMacroDefinitions <$>
  (parseSource source >>= exhaustivelyExpandMacros . convertList . convertUnit >>=
   normalizeStatement)

axelPathToHaskellPath :: FilePath -> FilePath
axelPathToHaskellPath axelPath =
  let basePath =
        if ".axel" `T.isSuffixOf` T.pack axelPath
          then fromMaybe axelPath $ stripExtension ".axel" axelPath
          else axelPath
  in basePath <> ".hs"

-- TODO Switch this to `(MonadError Error m, MonadIO m)` and do the error check in `evalFile`.
transpileFile :: FilePath -> FilePath -> IO ()
transpileFile path newPath = do
  fileContents <- S.readFile path
  result <- runExceptT $ transpileSource fileContents
  case result of
    Left err -> throwError $ userError $ show err
    Right newContents -> writeFile newPath newContents

-- Transpile a file in place.
transpileFile' :: FilePath -> IO FilePath
transpileFile' path = do
  let newPath = axelPathToHaskellPath path
  transpileFile path newPath
  pure newPath

evalFile :: FilePath -> IO ()
evalFile path =
  withTempDirectory $ \tempDirectoryPath -> do
    let astDefinitionPath = tempDirectoryPath </> "Axel.hs"
    readResource Res.astDefinition >>= writeFile astDefinitionPath
    let newPath = directory .~ tempDirectoryPath $ axelPathToHaskellPath path
    transpileFile path newPath
    evalResult <- runExceptT $ runWithGHC newPath
    either (throwError . userError . show) putStr evalResult
