module Axel.Test.SourcemapSpec where

import Axel.Sourcemap

import Control.Monad

import Test.Tasty.Hspec

{-# ANN module "HLint: ignore Redundant do" #-}

spec_Sourcemap :: SpecWith ()
spec_Sourcemap = do
  describe "findOriginalPosition" $ -- TODO Convert these into property tests
   do
    let (output, line', column') `was` expected =
          findOriginalPosition output (SourcePosition line' column') `shouldBe`
          expected
    context "output is empty" $ do
      it "fails" $ do
        ([], 1, 1) `was` (Nothing :: Maybe ())
        ([("", ()), ("", ())], 1, 1) `was` Nothing
    context "output has multiple lines" $ do
      it "can map each column on each line" $ do
        let output =
              [ ("some", 1)
              , ("  ", 2)
              , ("string\n", 3)
              , ("foo", 4)
              , ("\n", 5)
              , ("foo\nquux\n", 6)
              ]
        forM_ [1 .. 4] $ \i -> (output, 1, i) `was` Just (1 :: Int)
        forM_ [5 .. 6] $ \i -> (output, 1, i) `was` Just (2 :: Int)
        forM_ [7 .. 12] $ \i -> (output, 1, i) `was` Just (3 :: Int)
        (output, 1, 13) `was` Nothing
        forM_ [1 .. 3] $ \i -> (output, 2, i) `was` Just (4 :: Int)
        forM_ [4 .. 4] $ \i -> (output, 2, i) `was` Nothing
        (output, 2, 5) `was` Nothing
        forM_ [1 .. 3] $ \i -> (output, 3, i) `was` Just (6 :: Int)
        (output, 3, 4) `was` Nothing
        forM_ [1 .. 4] $ \i -> (output, 4, i) `was` Just (6 :: Int)
        (output, 4, 5) `was` Nothing
        (output, 5, 1) `was` Nothing
