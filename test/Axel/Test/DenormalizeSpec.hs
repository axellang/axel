module Axel.Test.DenormalizeSpec where

import Axel.Denormalize
import Axel.Normalize
import Axel.Test.ASTGen
import Axel.Test.MockUtils

import Hedgehog

hprop_normalizeExpression_is_the_inverse_of_denormalizeExpression :: Property
hprop_normalizeExpression_is_the_inverse_of_denormalizeExpression =
  property $ do
    expression <- forAll genExpression
    expression ===
      unwrapRight (normalizeExpression (denormalizeExpression expression))
