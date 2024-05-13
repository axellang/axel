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
      let f (LiteralChar ann x) = LiteralChar (ann + 1) x
          f (LiteralFloat ann x) = LiteralFloat (ann + 1) x
          f (LiteralInt ann x) = LiteralInt (ann + 1) x
          f (LiteralString ann x) = LiteralString (ann + 1) x
          f (SExpression ann xs) = SExpression (ann + 1) xs
          f (Symbol ann x) = Symbol (ann + 1) x
      bottomUpFmap f ast `shouldBe` ((+ 1) <$> ast)
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
      let f (LiteralChar ann x) = LiteralChar (ann + 1) x
          f (LiteralInt ann x) = LiteralInt (ann + 1) x
          f (LiteralString ann x) = LiteralString (ann + 1) x
          f (SExpression ann xs) = SExpression (ann + 1) xs
          f (Symbol ann x) = Symbol (ann + 1) x
          f _ = error "Impossible!"
      topDownFmap f ast `shouldBe` ((+ 1) <$> ast)
