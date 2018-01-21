{-# LANGUAGE FlexibleContexts #-}

module Lihsp.Transpile where

import Control.Monad.Except (MonadError)

import Data.Semigroup ((<>))

import Lihsp.AST (ToHaskell(toHaskell))
import Lihsp.Error (Error)
import Lihsp.Normalize (normalizeStatement)
import Lihsp.Parse (runSingle)

transpileProgram :: (MonadError Error m) => String -> m String
transpileProgram source =
  toHaskell <$> (runSingle source >>= normalizeStatement)

transpileFile :: FilePath -> IO ()
transpileFile path = do
  contents <- readFile path
  either print (writeFile $ path <> ".hs") (transpileProgram contents)
