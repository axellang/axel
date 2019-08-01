{-# OPTIONS_GHC "-fno-warn-incomplete-patterns" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TestUtils where

import Control.Exception
import Control.Monad.Freer
import Control.Monad.Freer.Error as Effs
import Control.Monad.Freer.State as Effs

import Test.Tasty.HUnit as HUnit

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

unwrapRight :: (Show b) => Either b a -> a
unwrapRight (Right x) = x
unwrapRight (Left x) = error $ show x

assertEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
assertEqual msg expected actual =
  catch (HUnit.assertEqual "" expected actual) $ \(HUnitFailure maybeSrcLoc errorMsg) ->
    errorWithoutStackTrace $
    "assertEquals FAILURE\n\nmessage: " <> msg <> "\n\n" <> errorMsg <>
    (case maybeSrcLoc of
       Just srcLoc -> "\n\nat: " <> show srcLoc
       Nothing -> "")
