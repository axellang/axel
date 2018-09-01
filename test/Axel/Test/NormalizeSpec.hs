module Axel.Test.NormalizeSpec where

import Axel.Denormalize
import Axel.Normalize
import Axel.Test.MockUtils
import Axel.Test.Parse.ASTGen

import Hedgehog

hprop_denormalizeExpression_is_the_inverse_of_normalizeExpression :: Property
hprop_denormalizeExpression_is_the_inverse_of_normalizeExpression =
  property $ do
    expression <- forAll genExpression
    expression ===
      denormalizeExpression (unwrapRight (normalizeExpression expression))
