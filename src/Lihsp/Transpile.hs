{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Lihsp.Transpile where

import Control.Lens.Operators ((.~))
import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)

import Lihsp.AST (ToHaskell(toHaskell))
import Lihsp.Error (Error)
import Lihsp.Macros (exhaustivelyExpandMacros, stripMacroDefinitions)
import Lihsp.Normalize (normalizeStatement)
import Lihsp.Parse (parseSource)

import System.FilePath.Lens (extension)

transpileSource :: (MonadError Error m, MonadIO m) => String -> m String
transpileSource source =
  toHaskell . stripMacroDefinitions <$>
  (parseSource source >>= exhaustivelyExpandMacros >>= normalizeStatement)

transpileFile :: FilePath -> IO ()
transpileFile path =
  readFile path >>= runExceptT . transpileSource >>= \case
    Left err -> print err
    Right newContents -> writeFile newPath newContents
  where
    newPath = extension .~ ".hs" $ path
