module Axel.Test.Parse.ASTGen where

import Axel.Prelude

import qualified Axel.Parse.AST as AST
import qualified Axel.Sourcemap as SM

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import TestUtils

genExpression :: (MonadGen m) => m SM.Expression
genExpression =
  Gen.recursive
    Gen.choice
    [ AST.LiteralChar Nothing <$> Gen.unicode
    , AST.LiteralFloat Nothing <$>
      Gen.float (Range.exponentialFloat (-10000) 10000)
    , AST.LiteralInt Nothing <$> Gen.int Range.constantBounded
    , AST.LiteralString Nothing <$> Gen.string (Range.linear 0 5) Gen.unicode
    , AST.Symbol Nothing <$> Gen.string (Range.linear 0 5) Gen.alpha
    ]
    [AST.SExpression Nothing <$> Gen.list (Range.linear 0 3) genExpression]
