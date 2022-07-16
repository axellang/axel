{- HLINT ignore "Redundant do" -}
module Axel.Test.Parse.ASTSpec where

import Axel.Prelude

import Axel.Parse.AST
import Axel.Utils.Recursion

import Test.Hspec

spec_AST :: Spec
spec_AST = do
  describe "RecursiveZipper instance" $ do
    it
      "correctly implements zipperBottomUpTraverse (by testing derived methods)" $ do
      let ast :: Expression Int
          ast =
            SExpression
              1
              [ Symbol 2 "foo"
              , SExpression
                  3
                  [ LiteralInt 4 1
                  , LiteralChar 5 'a'
                  , SExpression 6 [LiteralString 7 "bar"]
                  , SExpression 8 []
                  ]
              ]
      let f (LiteralChar ann x) = LiteralChar (succ ann) x
          f (LiteralFloat ann x) = LiteralFloat (succ ann) x
          f (LiteralInt ann x) = LiteralInt (succ ann) x
          f (LiteralString ann x) = LiteralString (succ ann) x
          f (SExpression ann xs) = SExpression (succ ann) xs
          f (Symbol ann x) = Symbol (succ ann) x
      bottomUpFmap f ast `shouldBe` (succ <$> ast)
    it "correctly implements zipperTopDownTraverse (by testing derived methods)" $ do
      let ast :: Expression Int
          ast =
            SExpression
              1
              [ Symbol 2 "foo"
              , SExpression
                  3
                  [ LiteralInt 4 1
                  , LiteralChar 5 'a'
                  , SExpression 6 [LiteralString 7 "bar"]
                  , SExpression 8 []
                  ]
              ]
      let f (LiteralChar ann x) = LiteralChar (succ ann) x
          f (LiteralInt ann x) = LiteralInt (succ ann) x
          f (LiteralString ann x) = LiteralString (succ ann) x
          f (SExpression ann xs) = SExpression (succ ann) xs
          f (Symbol ann x) = Symbol (succ ann) x
      topDownFmap f ast `shouldBe` (succ <$> ast)
