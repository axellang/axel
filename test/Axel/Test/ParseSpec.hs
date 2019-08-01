{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Axel.Test.ParseSpec where

import Axel.Error as Error
import Axel.Parse
import Axel.Parse.AST
import Axel.Utils.String

import Control.Monad.Freer as Eff
import qualified Control.Monad.Freer.Error as Effs

import Test.Tasty.Hspec

{-# ANN module "HLint: ignore Redundant do" #-}

parseSingle :: String -> Expression ()
parseSingle = (() <$) . unsafeParseSingle Nothing

spec_Parse :: SpecWith ()
spec_Parse = do
  describe "parseSingle" $ do
    it "can parse a character literal" $ do
      let result = LiteralChar () 'a'
      parseSingle "#\\a" `shouldBe` result
    it "can parse an integer literal" $ do
      let result = LiteralInt () 123
      parseSingle "123" `shouldBe` result
    it "can parse a list literal" $ do
      let result =
            SExpression
              ()
              [Symbol () "list", LiteralInt () 1, LiteralChar () 'a']
      parseSingle "[1 #\\a]" `shouldBe` result
    it "can parse a string literal" $ do
      let result = LiteralString () "a \"b"
      parseSingle "\"a \\\"b\"" `shouldBe` result
    it "can parse a quasiquoted expression" $ do
      let result =
            SExpression
              ()
              [ Symbol () "quasiquote"
              , SExpression () [Symbol () "foo", Symbol () "bar"]
              ]
      parseSingle "`(foo bar)" `shouldBe` result
    it "can parse an s-expression" $ do
      let result = SExpression () [Symbol () "foo", Symbol () "bar"]
      parseSingle "(foo bar)" `shouldBe` result
    it "can parse a splice-unquoted expression" $ do
      let result =
            SExpression
              ()
              [ Symbol () "unquoteSplicing"
              , SExpression () [Symbol () "foo", Symbol () "bar"]
              ]
      parseSingle "~@(foo bar)" `shouldBe` result
    it "can parse a symbol" $ do
      let result = Symbol () "abc123'''"
      parseSingle "abc123'''" `shouldBe` result
    it "can parse a macro name" $ do
      let result = Symbol () ",;foo"
      parseSingle ",;foo" `shouldBe` result
    it "can parse an unquoted expression" $ do
      let result =
            SExpression
              ()
              [ Symbol () "unquote"
              , SExpression () [Symbol () "foo", Symbol () "bar"]
              ]
      parseSingle "~(foo bar)" `shouldBe` result
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
                ()
                [ Symbol () "foo"
                , LiteralInt () 1
                , LiteralInt () 2
                , LiteralInt () 3
                ]
            , SExpression
                ()
                [Symbol () "bar", Symbol () "x", Symbol () "y", Symbol () "z"]
            ]
      case Eff.run . Effs.runError @Error.Error $ parseMultiple Nothing input of
        Left err -> expectationFailure $ show err
        Right x -> map (() <$) x `shouldBe` result
  describe "parseSource" $ do
    it "can parse a source file" $ do
      let input =
            [s|
(foo 1 2 3) -- This is a comment
-- Another comment! (bar x y z)
|]
      let result =
            SExpression
              ()
              [ Symbol () "begin"
              , SExpression
                  ()
                  [ Symbol () "foo"
                  , LiteralInt () 1
                  , LiteralInt () 2
                  , LiteralInt () 3
                  ]
              ]
      case Eff.run . Effs.runError @Error.Error $ parseSource Nothing input of
        Left err -> expectationFailure $ show err
        Right x -> () <$ x `shouldBe` result
