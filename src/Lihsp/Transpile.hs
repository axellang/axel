{-# LANGUAGE FlexibleContexts #-}

module Lihsp.Transpile where

import Control.Monad ((>=>))
import Control.Monad.Except (MonadError)

import Lihsp.Error (Error)
import Lihsp.Normalize (normalizeProgram)
import Lihsp.Parse (parseProgram)
import Lihsp.Utils (Delimiter(Newlines), delimit)

transpile :: (MonadError Error m) => String -> m String
transpile =
  parseProgram >=> normalizeProgram >=> return . delimit Newlines . map show
