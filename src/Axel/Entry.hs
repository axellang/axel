{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Axel.Entry where

import Control.Lens.Operators ((.~), (^.))
import Control.Monad (when)
import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)

import Axel.AST (ToHaskell(toHaskell))
import Axel.Error (Error)
import Axel.Eval (execInterpreter)
import Axel.Macros
  ( exhaustivelyExpandMacros
  , getAstDefinition
  , stripMacroDefinitions
  )
import Axel.Normalize (normalizeStatement)
import Axel.Parse (Expression(Symbol), parseSource)
import Axel.Utils.Recursion (Recursive(bottomUpFmap))

import System.Directory
  ( createDirectory
  , doesDirectoryExist
  , removeDirectoryRecursive
  )
import System.FilePath ((</>))
import System.FilePath.Lens (directory, extension)

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

transpileSource :: (MonadError Error m, MonadIO m) => String -> m String
transpileSource source =
  toHaskell . stripMacroDefinitions <$>
  (parseSource source >>= exhaustivelyExpandMacros . convertList . convertUnit >>=
   normalizeStatement)

-- TODO Switch this to `(MonadError Error m, MonadIO m)` and do the error check in `evalFile`.
transpileFile :: FilePath -> FilePath -> IO ()
transpileFile path newPath =
  readFile path >>= runExceptT . transpileSource >>= \case
    Left err -> print err
    Right newContents -> writeFile newPath newContents

evalFile :: FilePath -> IO ()
evalFile path = do
  let newPath = directory .~ tempDirectoryPath $ extension .~ "hs" $ path
  let astDefinitionPath = (newPath ^. directory) </> "Axel.hs"
  ensureCleanTempDirectory
  getAstDefinition >>= writeFile astDefinitionPath
  transpileFile path newPath
  evalResult <- runExceptT $ execInterpreter newPath
  case evalResult of
    Left err -> print err
    Right res -> putStrLn res
  where
    tempDirectoryPath = (path ^. directory) </> "axelTemp"
    ensureCleanTempDirectory = do
      tempDirectoryExists <- doesDirectoryExist tempDirectoryPath
      when tempDirectoryExists $ removeDirectoryRecursive tempDirectoryPath
      createDirectory tempDirectoryPath
