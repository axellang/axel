{- HLINT ignore "Redundant do" -}
module Axel.Test.Utils.MaybeSpec where

import Axel.Prelude

import Axel.Utils.Maybe
import Axel.Utils.Zipper

import Data.Char
import Data.Data
import Data.Generics.Uniplate.Data ()
import Data.Generics.Uniplate.Zipper
import qualified Data.Text as T

import Test.Hspec

data MockAST
  = SExp [MockAST]
  | Symbol Text
  deriving (Data, Eq, Show)

spec_Maybe :: Spec
spec_Maybe = do
  describe "foldUntilNothing" $ do
    it "works with basic function to be folded" $ do
      let modify :: Int -> Int
          modify = (+ 2)
      let move n =
            if n < 5
              then Just $ n - 1
              else Nothing
      foldUntilNothing move modify 0 `shouldBe` 5
    it "works as expected with AST-traversing zippers" $ do
      let modify z =
            replaceHole
              (case hole z of
                 SExp xs -> SExp xs
                 Symbol x -> Symbol $ T.map toUpper x)
              z
       in (fromZipper .
           foldUntilNothing right modify .
           unsafeDown . unsafeRight . unsafeDown . zipper)
            (SExp
               [Symbol "unchanged", SExp [Symbol "both", Symbol "capitalized"]]) `shouldBe`
          SExp [Symbol "unchanged", SExp [Symbol "BOTH", Symbol "CAPITALIZED"]]
