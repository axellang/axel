{-# LANGUAGE FlexibleContexts #-}

module Axel.Test.MockUtils where

import Control.Monad.Except
import Control.Monad.State.Lazy

throwInterpretError ::
     (MonadError String m, MonadState s m, Show s) => String -> String -> m a
throwInterpretError actionName message = do
  errorMsg <-
    gets $ \ctxt ->
      "\n----------\nACTION\t" <> actionName <> "\n\nMESSAGE\t" <> message <>
      "\n\nSTATE\t" <>
      show ctxt <>
      "\n----------\n"
  throwError errorMsg
