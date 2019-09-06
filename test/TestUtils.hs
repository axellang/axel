{-# OPTIONS_GHC "-fno-warn-incomplete-patterns" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TestUtils where

import Axel.Eff.Error
import Axel.Parse
import Axel.Sourcemap as SM

import Control.Exception

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem
import qualified Polysemy.State as Sem

import Data.Functor.Identity (Identity)

import Hedgehog hiding (MonadGen)
import qualified Hedgehog

import Test.Tasty.HUnit as HUnit

throwInterpretError ::
     forall s effs a.
     (Sem.Members '[ Sem.Error String, Sem.State s] effs, Show s)
  => String
  -> String
  -> Sem.Sem effs a
throwInterpretError actionName message = do
  errorMsg <-
    Sem.gets $ \ctxt ->
      "\n----------\nACTION\t" <>
      actionName <>
      "\n\nMESSAGE\t" <>
      message <> "\n\nSTATE\t" <> show ctxt <> "\n----------\n"
  Sem.throw errorMsg

unwrapRight :: (Show b) => Either b a -> a
unwrapRight (Right x) = x
unwrapRight (Left x) = error $ show x

assertEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
assertEqual msg expected actual =
  catch (HUnit.assertEqual "" expected actual) $ \(HUnitFailure maybeSrcLoc errorMsg) ->
    errorWithoutStackTrace $
    "assertEquals FAILURE\n\nmessage: " <>
    msg <>
    "\n\n" <>
    errorMsg <>
    (case maybeSrcLoc of
       Just srcLoc -> "\n\nat: " <> show srcLoc
       Nothing -> "")

-- | Will error at runtime if a parse error occurs.
-- | If multiple expressions are able to be parsed, only the first will be returned.
unsafeParseSingle :: Maybe FilePath -> String -> SM.Expression
unsafeParseSingle filePath =
  head . Sem.run . unsafeRunError @Axel.Eff.Error.Error . parseMultiple filePath

-- NOTE Workaround until https://github.com/hedgehogqa/haskell-hedgehog/commit/de401e949526951fdff87ef02fc75f13e8e22dfe
--      is publicly released.
-- TODO When hedgehogqa/haskell-hedgehog#303's changes are published, remove
--      the `GenBase m ~ Identity` constraint (currently, it's only required
--      because of `Gen.unicode`).
-- | Use instead of `MonadGen` from `hedgehog`.
type MonadGen m = (Hedgehog.MonadGen m, GenBase m ~ Identity)
