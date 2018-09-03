{-# OPTIONS_GHC "-fno-warn-incomplete-patterns" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Axel.Test.MockUtils where

import Control.Monad.Except
import Control.Monad.State.Lazy

import GHC.Exts (IsString(fromString))

import Language.Haskell.TH.Quote

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

-- Adapted from http://hackage.haskell.org/package/string-quote-0.0.1/docs/src/Data-String-Quote.html#s.
s :: QuasiQuoter
s =
  QuasiQuoter
    ((\a -> [|fromString a|]) . filter (/= '\r'))
    (error "Cannot use s as a pattern")
    (error "Cannot use s as a type")
    (error "Cannot use s as a dec")

unwrapRight :: Either b a -> a
unwrapRight (Right x) = x
