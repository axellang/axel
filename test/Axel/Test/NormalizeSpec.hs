module Axel.Test.NormalizeSpec where

import Axel.Prelude

import Axel.Denormalize
import Axel.Eff.Error as Error
import Axel.Normalize
import Axel.Sourcemap as SM
import qualified Axel.Test.Parse.ASTGen as Parse.ASTGen

import qualified Effectful as Eff
import qualified Effectful.Error.Static as Eff
import qualified Effectful.Reader.Static as Eff

import Hedgehog

import TestUtils

hprop_denormalizeExpression_is_the_inverse_of_normalizeExpression :: Property
hprop_denormalizeExpression_is_the_inverse_of_normalizeExpression =
  property $ do
    expr <- forAll Parse.ASTGen.genExpression
    expr ===
      denormalizeExpression
        (unwrapRight renderError $
         Eff.runPureEff .
         Eff.runErrorNoCallStack @Error.Error .
         Eff.runReader (FilePath "") . Eff.runReader ([] :: [SM.Expression]) $
         normalizeExpression expr)
