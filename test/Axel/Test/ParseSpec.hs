{-# LANGUAGE QuasiQuotes #-}

module Axel.Test.ParseSpec where

import Axel.Parse
import Axel.Test.MockUtils

import Test.Tasty.Hspec

spec_Parse :: SpecWith ()
spec_Parse = do
  describe "parseSingle" $ do
    it "can parse a character literal" $ do
      let result = LiteralChar 'a'
      case parseSingle "{a}" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can parse an integer literal" $ do
      let result = LiteralInt 123
      case parseSingle "123" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can parse a list literal" $ do
      let result = SExpression [Symbol "list", LiteralInt 1, LiteralChar 'a']
      case parseSingle "[1 {a}]" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can parse a string literal" $ do
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
    it "can quote a character character" $ do
      let result = SExpression [Symbol "AST.LiteralChar", LiteralChar 'a']
      case parseSingle "'{a}" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can quote an integer literal" $ do
      let result = SExpression [Symbol "AST.LiteralInt", LiteralInt 123]
      case parseSingle "'123" of
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
      case parseSingle "'[1 2]" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can quote a string literal" $ do
      let result = SExpression [Symbol "AST.LiteralString", LiteralString "foo"]
      case parseSingle "'\"foo\"" of
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
      case parseSingle "'(1 2)" of
        Left err -> expectationFailure $ show err
        Right x -> x `shouldBe` result
    it "can quote a symbol" $ do
      let result = SExpression [Symbol "AST.Symbol", LiteralString "foo"]
      case parseSingle "'foo" of
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
      case parseSingle "''foo" of
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
