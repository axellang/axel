{- HLINT ignore "Redundant do" -}
module Axel.Test.Utils.ListSpec where

import Axel.Prelude

import Axel.Utils.List

import Test.Tasty.Hspec

spec_List :: SpecWith ()
spec_List = do
  describe "filterMapOut" $ do
    it "correctly partitions its input" $ do
      let f :: Int -> Maybe Text
          f n =
            if n `mod` 2 == 0
              then Just $ showText n
              else Nothing
       in filterMapOut f [1, 2, 3, 4, 6] `shouldBe` ([1, 3], ["2", "4", "6"])
