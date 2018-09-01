module Axel.Test.DenormalizeSpec where

import Axel.Denormalize
import Axel.Normalize
import qualified Axel.Test.ASTGen as ASTGen
import Axel.Test.MockUtils

import Hedgehog

hprop_normalizeExpression_is_the_inverse_of_denormalizeExpression :: Property
hprop_normalizeExpression_is_the_inverse_of_denormalizeExpression =
  property $ do
    expr <- forAll ASTGen.genExpression
    expr === unwrapRight (normalizeExpression (denormalizeExpression expr))

hprop_normalizeStatement_is_the_inverse_of_denormalizeStatement :: Property
hprop_normalizeStatement_is_the_inverse_of_denormalizeStatement =
  property $ do
    stmt <- forAll ASTGen.genStatement
    stmt === unwrapRight (normalizeStatement (denormalizeStatement stmt))
