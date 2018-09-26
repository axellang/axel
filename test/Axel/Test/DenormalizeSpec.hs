{-# LANGUAGE TypeApplications #-}

module Axel.Test.DenormalizeSpec where

import Axel.Denormalize
import Axel.Error
import Axel.Normalize
import qualified Axel.Test.ASTGen as ASTGen
import Axel.Test.MockUtils

import Control.Monad.Freer as Eff
import Control.Monad.Freer.Error (runError)

import Hedgehog

hprop_normalizeExpression_is_the_inverse_of_denormalizeExpression :: Property
hprop_normalizeExpression_is_the_inverse_of_denormalizeExpression =
  property $ do
    expr <- forAll ASTGen.genExpression
    expr ===
      unwrapRight
        (Eff.run . runError @Error $
         normalizeExpression (denormalizeExpression expr))

hprop_normalizeStatement_is_the_inverse_of_denormalizeStatement :: Property
hprop_normalizeStatement_is_the_inverse_of_denormalizeStatement =
  property $ do
    stmt <- forAll ASTGen.genStatement
    stmt ===
      unwrapRight
        (Eff.run . runError @Error $
         normalizeStatement (denormalizeStatement stmt))
