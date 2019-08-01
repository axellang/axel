module Axel.Test.MacrosSpec where

import Axel.Macros
import Axel.Parse
import Axel.Parse.AST
import qualified Axel.Sourcemap as SM
import Axel.Utils.Zipper

import Data.Generics.Uniplate.Zipper

import Test.Tasty.Hspec

import TestUtils

{-# ANN module "HLint: ignore Redundant do" #-}

spec_Macros :: SpecWith ()
spec_Macros = do
  describe "isStatementFocused" $ do
    it "returns if a zipper is focused on a statement's expression" $ do
      let z =
            zipper $
            unsafeParseSingle Nothing "(begin (testStatement1) testStatement2)"
      let testFocusers ::
               Bool
            -> [Zipper SM.Expression SM.Expression -> Zipper SM.Expression SM.Expression]
            -> Expectation
          testFocusers expected =
            mapM_
              (\focus ->
                 assertEqual
                   ("Failure at " <> toAxel (hole $ focus z) <>
                    "\n\n\t Context:\thole (focus z)\t\t== " <>
                    toAxel (hole $ focus z) <>
                    "\n\t\t\thole <$> up (focus z)\t== " <>
                    show (toAxel . hole <$> up (focus z)) <>
                    "\n\t\t\tfromZipper z\t\t== " <>
                    toAxel (fromZipper z))
                   expected
                   (isStatementFocused $ focus z))
      testFocusers
        True
        [unsafeRight . unsafeDown, unsafeRight . unsafeRight . unsafeDown]
      testFocusers False [id, unsafeDown, unsafeDown . unsafeRight . unsafeDown]
