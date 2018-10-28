{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Axel.Haskell.File where

import Prelude hiding (putStr, putStrLn)

import Axel.AST (ToHaskell(toHaskell))
import Axel.Eff.Console (putStrLn)
import qualified Axel.Eff.Console as Effs (Console)
import qualified Axel.Eff.FileSystem as Effs (FileSystem)
import qualified Axel.Eff.FileSystem as FS
  ( readFile
  , withTemporaryDirectory
  , writeFile
  )
import Axel.Eff.Process (StreamSpecification(InheritStreams))
import qualified Axel.Eff.Process as Effs (Process)
import Axel.Eff.Resource (readResource)
import qualified Axel.Eff.Resource as Effs (Resource)
import qualified Axel.Eff.Resource as Res (astDefinition)
import Axel.Error (Error)
import Axel.Haskell.Prettify (prettifyHaskell)
import Axel.Haskell.Stack (interpretFile)
import Axel.Macros (exhaustivelyExpandMacros)
import Axel.Normalize (normalizeStatement)
import Axel.Parse (Expression(Symbol), parseSource)
import Axel.Utils.Recursion (Recursive(bottomUpFmap))

import Control.Lens.Operators ((.~))
import Control.Monad (void)
import Control.Monad.Freer (Eff, Members)
import qualified Control.Monad.Freer.Error as Effs (Error)

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
     (Members '[ Effs.Console, Effs.Error Error, Effs.FileSystem, Effs.Process, Effs.Resource] effs)
  => String
  -> Eff effs String
transpileSource source =
  prettifyHaskell . toHaskell <$>
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
     (Members '[ Effs.Console, Effs.Error Error, Effs.FileSystem, Effs.Process, Effs.Resource] effs)
  => FilePath
  -> FilePath
  -> Eff effs ()
transpileFile path newPath = do
  fileContents <- FS.readFile path
  newContents <- transpileSource fileContents
  putStrLn "Writing file..."
  FS.writeFile newPath newContents

-- | Transpile a file in place.
transpileFile' ::
     (Members '[ Effs.Console, Effs.Error Error, Effs.FileSystem, Effs.Process, Effs.Resource] effs)
  => FilePath
  -> Eff effs FilePath
transpileFile' path = do
  let newPath = axelPathToHaskellPath path
  transpileFile path newPath
  pure newPath

evalFile ::
     (Members '[ Effs.Console, Effs.Error Error, Effs.FileSystem, Effs.Process, Effs.Resource] effs)
  => FilePath
  -> Eff effs ()
evalFile path = do
  putStrLn ("Building " <> takeFileName path <> "...")
  FS.withTemporaryDirectory $ \tempDirectoryPath -> do
    let astDefinitionPath = tempDirectoryPath </> "Axel.hs"
    readResource Res.astDefinition >>= FS.writeFile astDefinitionPath
    let newPath = directory .~ tempDirectoryPath $ axelPathToHaskellPath path
    transpileFile path newPath
    putStrLn ("Running " <> takeFileName path <> "...")
    void $ interpretFile @'InheritStreams newPath
