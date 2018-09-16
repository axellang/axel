{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Axel.Haskell.File where

import Prelude hiding (putStr, putStrLn)

import Axel.AST (ToHaskell(toHaskell))
import Axel.Error (Error(EvalError), mapError)
import Axel.Haskell.GHC (ghcInterpret)
import Axel.Haskell.Prettify (prettifyHaskell)
import Axel.Macros (exhaustivelyExpandMacros, stripMacroDefinitions)
import Axel.Monad.Console (MonadConsole(putStr), putStrLn)
import Axel.Monad.FileSystem (MonadFileSystem)
import qualified Axel.Monad.FileSystem as FS
  ( MonadFileSystem(readFile, writeFile)
  , withTemporaryDirectory
  )
import Axel.Monad.Process (MonadProcess)
import Axel.Monad.Resource (MonadResource, readResource)
import qualified Axel.Monad.Resource as Res (astDefinition)
import Axel.Normalize (normalizeStatement)
import Axel.Parse (Expression(Symbol), parseSource)
import Axel.Utils.Recursion (Recursive(bottomUpFmap))

import Control.Lens.Operators ((.~))
import Control.Monad.Except (MonadError)

import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import qualified Data.Text as T (isSuffixOf, pack)

import System.FilePath ((</>), stripExtension, takeFileName)
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
     (MonadError Error m, MonadFileSystem m, MonadProcess m, MonadResource m)
  => String
  -> m String
transpileSource source =
  prettifyHaskell . toHaskell . stripMacroDefinitions <$>
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
     (MonadError Error m, MonadFileSystem m, MonadProcess m, MonadResource m)
  => FilePath
  -> FilePath
  -> m ()
transpileFile path newPath = do
  fileContents <- FS.readFile path
  newContents <- transpileSource fileContents
  FS.writeFile newPath newContents

-- | Transpile a file in place.
transpileFile' ::
     (MonadError Error m, MonadFileSystem m, MonadProcess m, MonadResource m)
  => FilePath
  -> m FilePath
transpileFile' path = do
  let newPath = axelPathToHaskellPath path
  transpileFile path newPath
  pure newPath

evalFile ::
     ( MonadConsole m
     , MonadError Error m
     , MonadFileSystem m
     , MonadProcess m
     , MonadResource m
     )
  => FilePath
  -> m ()
evalFile path = do
  putStrLn ("Building " <> takeFileName path <> "...")
  FS.withTemporaryDirectory $ \tempDirectoryPath -> do
    let astDefinitionPath = tempDirectoryPath </> "Axel.hs"
    readResource Res.astDefinition >>= FS.writeFile astDefinitionPath
    let newPath = directory .~ tempDirectoryPath $ axelPathToHaskellPath path
    transpileFile path newPath
    putStrLn ("Running " <> takeFileName path <> "...")
    output <- ghcInterpret newPath `mapError` EvalError
    putStr output
