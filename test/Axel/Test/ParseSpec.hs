{- HLINT ignore "Redundant do" -}
{-# LANGUAGE QuasiQuotes #-}

module Axel.Test.ParseSpec where

import Axel.Prelude

import Axel.Eff.Error
import Axel.Parse
import Axel.Parse.AST
import Axel.Utils.Text

import Control.Monad

import qualified Effectful as Eff
import qualified Effectful.Error.Static as Eff

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Hspec

import TestUtils

parseSingle :: Text -> Expression ()
parseSingle = (() <$) . unsafeParseSingle Nothing

spec_Parse :: Spec
spec_Parse = do
  describe "parseSingle" $ do
    it "can parse a character literal" $ do
      parseSingle "#\\a" `shouldBe` LiteralChar () 'a'
      parseSingle "#\\\\x1000" `shouldBe` LiteralChar () '\x1000'
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
      let result = LiteralString () "a \x1000 \"b"
      parseSingle "\"a \x1000 \\\"b\"" `shouldBe` result
    it
      "can parse string literals with escaped double quotes at the boundaries (regression: #79)" $ do
      let result = LiteralString () "a \x1000 \""
      parseSingle "\"a \x1000 \\\"\"" `shouldBe` result
      let result = LiteralString () "a \x1000 \"\""
      parseSingle "\"a \x1000 \\\"\\\"\"" `shouldBe` result
      let result = LiteralString () "\""
      parseSingle "\"\\\"\"" `shouldBe` result
      let result = LiteralString () "\"\""
      parseSingle "\"\\\"\\\"\"" `shouldBe` result
      let result = LiteralString () "\"\" foo"
      parseSingle "\"\\\"\\\" foo\"" `shouldBe` result
      let result = LiteralString () "\"\"\""
      parseSingle "\"\\\"\\\"\\\"\"" `shouldBe` result
    it
      "can parse a string literal with a double quote at the end (regression: #79)" $ do
      let result = LiteralString () "a \x1000 \""
      parseSingle "\"a \x1000 \\\"\"" `shouldBe` result
    it "can parse a string literal with escaped unprintables" $ do
      let result = LiteralString () "\0"
      parseSingle "\"\\0\"" `shouldBe` result
      let result = LiteralString () "\NUL"
      parseSingle "\"\\NUL\"" `shouldBe` result
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
    it
      "can parse a value-level name that starts with a Haskell-restricted character" $ do
      let result =
            Symbol () "aXEL_VALUE_aXEL_SYMBOL_COMMA_aXEL_SYMBOL_SEMICOLON_foo"
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
      case Eff.runPureEff . Eff.runErrorNoCallStack $
           parseMultiple Nothing input of
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
              , SExpression
                  ()
                  [ Symbol
                      ()
                      "butThisaXEL_SYMBOL_HYPHEN_aXEL_SYMBOL_HYPHEN_aXEL_SYMBOL_GREATERTHAN_IsASingleSymbol"
                  ]
              ]
      case Eff.runPureEff . Eff.runErrorNoCallStack $ parseSource Nothing input of
        Left err -> failSpec $ renderError err
        Right x -> void x `shouldBe` result

hprop_can_parse_string_literals :: Property
hprop_can_parse_string_literals =
  property $ do
    string <- forAll $ Gen.string (Range.linear 0 5) Gen.unicode
    let result = parseSingle $ showText string
    result === LiteralString () string
