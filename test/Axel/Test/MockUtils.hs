{-# OPTIONS_GHC "-fno-warn-incomplete-patterns" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Axel.Test.MockUtils where

import Control.Monad.Freer
import Control.Monad.Freer.Error as Effs
import Control.Monad.Freer.State as Effs

import GHC.Exts (IsString(fromString))

import Language.Haskell.TH.Quote

throwInterpretError ::
     forall s effs a. (Members '[ Effs.Error String, Effs.State s] effs, Show s)
  => String
  -> String
  -> Eff effs a
throwInterpretError actionName message = do
  errorMsg <-
    gets @s $ \ctxt ->
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
