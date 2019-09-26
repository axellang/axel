{- HLINT ignore "Redundant do" -}
{-# LANGUAGE QuasiQuotes #-}

module Axel.Test.ParseSpec where

import Axel.Prelude

import Axel.Eff.Error
import Axel.Parse
import Axel.Parse.AST
import Axel.Utils.Text

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem

import Test.Tasty.Hspec

import TestUtils

parseSingle :: Text -> Expression ()
parseSingle = (() <$) . unsafeParseSingle Nothing

spec_Parse :: SpecWith ()
spec_Parse = do
  describe "parseSingle" $ do
    it "can parse a character literal" $ do
      let result = LiteralChar () 'a'
      parseSingle "#\\a" `shouldBe` result
    it "can parse an integer literal" $ do
      parseSingle "-123" `shouldBe` LiteralInt () (-123)
      parseSingle "456" `shouldBe` LiteralInt () 456
    it "can parse a float literal" $ do
      parseSingle "-12.3" `shouldBe` LiteralFloat () (-12.3)
      parseSingle "4.56" `shouldBe` LiteralFloat () 4.56
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
      case Sem.run . Sem.runError $ parseMultiple Nothing input of
        Left err -> failSpec $ renderError err
        Right x -> map (() <$) x `shouldBe` result
  describe "parseSource" $ do
    it "can parse a source file" $ do
      let input =
            [s|
(foo 1 2 3) -- This is a comment
-- Another comment! (bar x y z)
(end of line comment) --
(butThis-->IsASingleSymbol)
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
              , SExpression
                  ()
                  [ Symbol () "end"
                  , Symbol () "of"
                  , Symbol () "line"
                  , Symbol () "comment"
                  ]
              , SExpression () [Symbol () "butThis-->IsASingleSymbol"]
              ]
      case Sem.run . Sem.runError $ parseSource Nothing input of
        Left err -> failSpec $ renderError err
        Right x -> () <$ x `shouldBe` result
