{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Axel.Test.ParseSpec where

import Axel.Error
import Axel.Parse
import Axel.Utils.String

import Control.Monad.Freer as Eff
import qualified Control.Monad.Freer.Error as Effs

import Test.Tasty.Hspec

{-# ANN module "HLint: ignore Redundant do" #-}

spec_Parse :: SpecWith ()
spec_Parse = do
  describe "parseSingle" $ do
    it "can parse a character literal" $ do
      let result = LiteralChar 'a'
      case Eff.run . Effs.runError @Error $ parseSingle "#\\a" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can parse an integer literal" $ do
      let result = LiteralInt 123
      case Eff.run . Effs.runError @Error $ parseSingle "123" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can parse a list literal" $ do
      let result = SExpression [Symbol "list", LiteralInt 1, LiteralChar 'a']
      case Eff.run . Effs.runError @Error $ parseSingle "[1 #\\a]" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can parse a string literal" $ do
      let result = LiteralString "a \"b"
      case Eff.run . Effs.runError @Error $ parseSingle "\"a \\\"b\"" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can parse a quasiquoted expression" $ do
      let result =
            SExpression
              [Symbol "quasiquote", SExpression [Symbol "foo", Symbol "bar"]]
      case Eff.run . Effs.runError @Error $ parseSingle "`(foo bar)" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can parse an s-expression" $ do
      let result = SExpression [Symbol "foo", Symbol "bar"]
      case Eff.run . Effs.runError @Error $ parseSingle "(foo bar)" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can parse a splice-unquoted expression" $ do
      let result =
            SExpression
              [ Symbol "unquoteSplicing"
              , SExpression [Symbol "foo", Symbol "bar"]
              ]
      case Eff.run . Effs.runError @Error $ parseSingle "~@(foo bar)" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can parse a symbol" $ do
      let result = Symbol "abc123'''"
      case Eff.run . Effs.runError @Error $ parseSingle "abc123'''" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can parse a macro name" $ do
      let result = Symbol ",;foo"
      case Eff.run . Effs.runError @Error $ parseSingle ",;foo" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can parse an unquoted expression" $ do
      let result =
            SExpression
              [Symbol "unquote", SExpression [Symbol "foo", Symbol "bar"]]
      case Eff.run . Effs.runError @Error $ parseSingle "~(foo bar)" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can quote a character character" $ do
      let result = SExpression [Symbol "AST.LiteralChar", LiteralChar 'a']
      case Eff.run . Effs.runError @Error $ parseSingle "'#\\a" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can quote an integer literal" $ do
      let result = SExpression [Symbol "AST.LiteralInt", LiteralInt 123]
      case Eff.run . Effs.runError @Error $ parseSingle "'123" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can quote a list literal" $ do
      let result =
            SExpression
              [ Symbol "AST.SExpression"
              , SExpression
                  [ Symbol "list"
                  , SExpression [Symbol "AST.Symbol", LiteralString "list"]
                  , SExpression [Symbol "AST.LiteralInt", LiteralInt 1]
                  , SExpression [Symbol "AST.LiteralInt", LiteralInt 2]
                  ]
              ]
      case Eff.run . Effs.runError @Error $ parseSingle "'[1 2]" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can quote a string literal" $ do
      let result = SExpression [Symbol "AST.LiteralString", LiteralString "foo"]
      case Eff.run . Effs.runError @Error $ parseSingle "'\"foo\"" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can quote an s-expression" $ do
      let result =
            SExpression
              [ Symbol "AST.SExpression"
              , SExpression
                  [ Symbol "list"
                  , SExpression [Symbol "AST.LiteralInt", LiteralInt 1]
                  , SExpression [Symbol "AST.LiteralInt", LiteralInt 2]
                  ]
              ]
      case Eff.run . Effs.runError @Error $ parseSingle "'(1 2)" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can quote a symbol" $ do
      let result = SExpression [Symbol "AST.Symbol", LiteralString "foo"]
      case Eff.run . Effs.runError @Error $ parseSingle "'foo" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can quote a quoted expression" $ do
      let result =
            SExpression
              [ Symbol "AST.SExpression"
              , SExpression
                  [ Symbol "list"
                  , SExpression [Symbol "AST.Symbol", LiteralString "quote"]
                  , SExpression [Symbol "AST.Symbol", LiteralString "foo"]
                  ]
              ]
      case Eff.run . Effs.runError @Error $ parseSingle "''foo" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
  describe "parseMultiple" $ do
    it "can parse multiple expressions" $ do
      let input =
            [s|
(foo 1 2 3)

(bar
 x
 y
 z)
|]
      let result =
            [ SExpression
                [Symbol "foo", LiteralInt 1, LiteralInt 2, LiteralInt 3]
            , SExpression [Symbol "bar", Symbol "x", Symbol "y", Symbol "z"]
            ]
      case Eff.run . Effs.runError @Error $ parseMultiple input of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
  describe "parseSource" $ do
    it "can parse a source file" $ do
      let input =
            [s|
(foo 1 2 3) -- This is a comment
-- Another comment! (bar x y z)
|]
      let result =
            SExpression
              [ Symbol "begin"
              , SExpression
                  [Symbol "foo", LiteralInt 1, LiteralInt 2, LiteralInt 3]
              ]
      case Eff.run . Effs.runError @Error $ parseSource input of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
