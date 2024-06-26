{-# OPTIONS_GHC "-fno-warn-incomplete-patterns" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module TestUtils where

import Axel.Prelude

import Axel.Eff ((:>>))
import Axel.Eff.Error
import Axel.Parse
import Axel.Sourcemap as SM
import Axel.Utils.Text

import Control.Exception

import Data.Functor.Identity (Identity)
import qualified Data.Text as T

import qualified Effectful as Eff
import qualified Effectful.Error.Static as Eff
import qualified Effectful.State.Static.Local as Eff

import Hedgehog hiding (MonadGen)
import qualified Hedgehog

import Test.Hspec
import Test.Tasty.HUnit as HUnit

throwInterpretError ::
     forall s effs a. ('[ Eff.Error Text, Eff.State s] :>> effs, Show s)
  => Text
  -> Text
  -> Eff.Eff effs a
throwInterpretError actionName message = do
  errorMsg <-
    Eff.gets $ \ctxt ->
      "\n----------\nACTION\t" <>
      actionName <>
      "\n\nMESSAGE\t" <>
      message <> "\n\nSTATE\t" <> showText ctxt <> "\n----------\n"
  Eff.throwError errorMsg

unwrapRight :: Renderer e -> Either e a -> a
unwrapRight _ (Right x) = x
unwrapRight errorRenderer (Left x) = error $ errorRenderer x

assertEqual :: (Eq a, Show a) => Text -> a -> a -> Assertion
assertEqual msg expected actual =
  catch (HUnit.assertEqual "" expected actual) $ \(HUnitFailure maybeSrcLoc errorMsg) ->
    errorWithoutStackTrace $
    "assertEquals FAILURE\n\nmessage: " <>
    msg <>
    "\n\n" <>
    T.pack errorMsg <>
    (case maybeSrcLoc of
       Just srcLoc -> "\n\nat: " <> showText srcLoc
       Nothing -> "")

-- | Will error at runtime if a parse error occurs.
--  If multiple expressions are able to be parsed, only the first will be returned.
unsafeParseSingle :: Maybe FilePath -> Text -> SM.Expression
unsafeParseSingle filePath =
  head . Eff.runPureEff . unsafeRunError renderError . parseMultiple filePath

-- | A convenience wrapper for `MonadGen` from `hedgehog`.
--   We need the `GenBase m ~ Identity` constraint in order to use `Gen.filter`,
--   so it's convenient to always have it available.
type MonadGen m = (Hedgehog.MonadGen m, GenBase m ~ Identity)

failSpec :: Text -> Expectation
failSpec = expectationFailure . T.unpack
