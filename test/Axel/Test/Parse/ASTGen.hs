module Axel.Test.Parse.ASTGen where

import qualified Axel.Parse.AST as AST

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genExpression :: (MonadGen m) => m AST.Expression
genExpression =
  Gen.recursive
    Gen.choice
    [ AST.LiteralChar <$> Gen.unicode
    , AST.LiteralInt <$> Gen.int Range.constantBounded
    , AST.LiteralString <$> Gen.string (Range.linear 0 5) Gen.unicode
    , AST.Symbol <$> Gen.string (Range.linear 0 5) Gen.alpha
    ]
    [AST.SExpression <$> Gen.list (Range.linear 0 3) genExpression]
