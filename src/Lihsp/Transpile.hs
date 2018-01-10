{-# LANGUAGE FlexibleContexts #-}

module Lihsp.Transpile where

import Control.Monad.Except (MonadError)

import Data.Semigroup ((<>))

import Lihsp.AST (ToHaskell(toHaskell))
import Lihsp.Error (Error)
import Lihsp.Normalize (normalizeProgram)
import Lihsp.Parse (parseProgram)
import Lihsp.Utils.Display (Delimiter(Newlines), delimit)

transpileProgram :: (MonadError Error m) => String -> m String
transpileProgram source =
  delimit Newlines . map toHaskell <$>
  (parseProgram source >>= normalizeProgram)

transpileFile :: FilePath -> IO ()
transpileFile path = do
  contents <- readFile path
  either print (writeFile $ path <> ".hs") (transpileProgram contents)
