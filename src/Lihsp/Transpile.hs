{-# LANGUAGE FlexibleContexts #-}

module Lihsp.Transpile where

import Control.Monad ((>=>))
import Control.Monad.Except (MonadError)

import Data.Semigroup ((<>))

import Lihsp.Error (Error)
import Lihsp.Normalize (normalizeProgram)
import Lihsp.Parse (parseProgram)
import Lihsp.Utils.Display (Delimiter(Newlines), delimit)

transpileProgram :: (MonadError Error m) => String -> m String
transpileProgram =
  parseProgram >=> normalizeProgram >=> return . delimit Newlines . map show

transpileFile :: FilePath -> IO ()
transpileFile path = do
  contents <- readFile $ path <> ".lihsp"
  let newContents = transpileProgram contents
  either print (writeFile $ path <> ".hs") newContents
