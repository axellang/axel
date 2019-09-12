{-# LANGUAGE TypeApplications #-}

module Axel.Test.NormalizeSpec where

import Axel.Denormalize
import Axel.Eff.Error as Error
import Axel.Normalize
import Axel.Sourcemap as SM
import qualified Axel.Test.Parse.ASTGen as Parse.ASTGen

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem
import qualified Polysemy.Reader as Sem

import Hedgehog

import TestUtils

hprop_denormalizeExpression_is_the_inverse_of_normalizeExpression :: Property
hprop_denormalizeExpression_is_the_inverse_of_normalizeExpression =
  property $ do
    expr <- forAll Parse.ASTGen.genExpression
    expr ===
      denormalizeExpression
        (unwrapRight renderError $
         Sem.run .
         Sem.runError @Error.Error .
         Sem.runReader "" . Sem.runReader ([] :: [SM.Expression]) $
         normalizeExpression expr)
