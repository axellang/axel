{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Lihsp.Entry where

import Control.Lens.Operators ((.~))
import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)

import Lihsp.AST (ToHaskell(toHaskell))
import Lihsp.Error (Error)
import Lihsp.Eval (evalSource)
import Lihsp.Macros (exhaustivelyExpandMacros, stripMacroDefinitions)
import Lihsp.Normalize (normalizeStatement)
import Lihsp.Parse (Expression(Symbol), parseSource)
import Lihsp.Utils.Recursion (Recursive(bottomUpFmap))

import System.FilePath.Lens (extension)

convertUnit :: Expression -> Expression
convertUnit =
  bottomUpFmap $ \case
    Symbol "Unit" -> Symbol "()"
    Symbol "unit" -> Symbol "()"
    x -> x

transpileSource :: (MonadError Error m, MonadIO m) => String -> m String
transpileSource source =
  toHaskell . stripMacroDefinitions <$>
  (parseSource source >>= exhaustivelyExpandMacros >>=
   normalizeStatement . convertUnit)

transpileFile :: FilePath -> FilePath -> IO ()
transpileFile path newPath =
  readFile path >>= runExceptT . transpileSource >>= \case
    Left err -> print err
    Right newContents -> writeFile newPath newContents

evalFile :: FilePath -> IO ()
evalFile path = do
  let newPath = extension .~ ".hs" $ path
  transpileFile path newPath
  newContents <- readFile newPath
  evalResult <- runExceptT $ evalSource newContents
  case evalResult of
    Left err -> print err
    Right res -> putStrLn res
