{-# LANGUAGE QuasiQuotes #-}

module Axel.Test.ParseSpec where

import Axel.Parse
import Axel.Test.MockUtils

import Test.Tasty.Hspec

spec_Parse :: SpecWith ()
spec_Parse = do
  describe "parseSingle" $ do
    it "can parse a literal character" $ do
      let result = LiteralChar 'a'
      case parseSingle "{a}" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can parse a literal integer" $ do
      let result = LiteralInt 123
      case parseSingle "123" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can parse a literal list" $ do
      let result = SExpression [Symbol "list", LiteralInt 1, LiteralChar 'a']
      case parseSingle "[1 {a}]" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can parse a literal string" $ do
      let result = LiteralString "a b"
      case parseSingle "\"a b\"" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can parse a quasiquoted expression" $ do
      let result =
            SExpression
              [Symbol "quasiquote", SExpression [Symbol "foo", Symbol "bar"]]
      case parseSingle "`(foo bar)" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can parse a quoted expression" $ do
      let result =
            SExpression
              [Symbol "quote", SExpression [Symbol "foo", Symbol "bar"]]
      case parseSingle "'(foo bar)" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can parse an s-expression" $ do
      let result = SExpression [Symbol "foo", Symbol "bar"]
      case parseSingle "(foo bar)" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can parse a splice-unquoted expression" $ do
      let result =
            SExpression
              [ Symbol "unquoteSplicing"
              , SExpression [Symbol "foo", Symbol "bar"]
              ]
      case parseSingle "~@(foo bar)" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can parse a symbol" $ do
      let result = Symbol "abc123'''"
      case parseSingle "abc123'''" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can parse an unquoted expression" $ do
      let result =
            SExpression
              [Symbol "unquote", SExpression [Symbol "foo", Symbol "bar"]]
      case parseSingle "~(foo bar)" of
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
      case parseMultiple input of
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
      case parseSource input of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
