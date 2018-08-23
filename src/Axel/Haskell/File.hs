{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Axel.Haskell.File where

import Axel.AST (ToHaskell(toHaskell))
import Axel.Error (Error)
import Axel.Macros (exhaustivelyExpandMacros, stripMacroDefinitions)
import Axel.Monad.FileSystem (MonadFileSystem)
import qualified Axel.Monad.FileSystem as FS
  ( MonadFileSystem(readFile, withTempDirectory, writeFile)
  )
import Axel.Monad.Haskell.GHC (MonadGHC(ghcInterpret))
import Axel.Monad.Output (MonadOutput(outputStr))
import Axel.Monad.Resource (MonadResource(readResource))
import qualified Axel.Monad.Resource as Res (astDefinition)
import Axel.Normalize (normalizeStatement)
import Axel.Parse (Expression(Symbol), parseSource)
import Axel.Utils.Recursion (Recursive(bottomUpFmap))

import Control.Lens.Operators ((.~))
import Control.Monad.Except (MonadError)

import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import qualified Data.Text as T (isSuffixOf, pack)

import System.FilePath ((</>), stripExtension)
import System.FilePath.Lens (directory)

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
     (MonadError Error m, MonadFileSystem m, MonadGHC m, MonadResource m)
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

transpileFile ::
     (MonadError Error m, MonadFileSystem m, MonadGHC m, MonadResource m)
  => FilePath
  -> FilePath
  -> m ()
transpileFile path newPath = do
  fileContents <- FS.readFile path
  newContents <- transpileSource fileContents
  FS.writeFile newPath newContents

-- Transpile a file in place.
transpileFile' ::
     (MonadError Error m, MonadFileSystem m, MonadGHC m, MonadResource m)
  => FilePath
  -> m FilePath
transpileFile' path = do
  let newPath = axelPathToHaskellPath path
  transpileFile path newPath
  pure newPath

evalFile ::
     ( MonadError Error m
     , MonadFileSystem m
     , MonadGHC m
     , MonadResource m
     , MonadOutput m
     )
  => FilePath
  -> m ()
evalFile path =
  FS.withTempDirectory $ \tempDirectoryPath -> do
    let astDefinitionPath = tempDirectoryPath </> "Axel.hs"
    readResource Res.astDefinition >>= FS.writeFile astDefinitionPath
    let newPath = directory .~ tempDirectoryPath $ axelPathToHaskellPath path
    transpileFile path newPath
    output <- ghcInterpret newPath
    outputStr output
