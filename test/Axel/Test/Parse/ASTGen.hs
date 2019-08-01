module Axel.Test.Parse.ASTGen where

import qualified Axel.Parse.AST as AST
import qualified Axel.Sourcemap as SM

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genExpression :: (MonadGen m) => m SM.Expression
genExpression =
  Gen.recursive
    Gen.choice
    [ AST.LiteralChar Nothing <$> Gen.unicode
    , AST.LiteralInt Nothing <$> Gen.int Range.constantBounded
    , AST.LiteralString Nothing <$> Gen.string (Range.linear 0 5) Gen.unicode
    , AST.Symbol Nothing <$> Gen.string (Range.linear 0 5) Gen.alpha
    ]
    [AST.SExpression Nothing <$> Gen.list (Range.linear 0 3) genExpression]
