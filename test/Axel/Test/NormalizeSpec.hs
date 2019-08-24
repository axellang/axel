{-# LANGUAGE TypeApplications #-}

module Axel.Test.NormalizeSpec where

import Axel.Denormalize
import Axel.Eff.Error as Error
import Axel.Normalize
import Axel.Sourcemap as SM
import qualified Axel.Test.Parse.ASTGen as Parse.ASTGen

import Control.Monad.Freer as Eff
import qualified Control.Monad.Freer.Error as Effs
import qualified Control.Monad.Freer.Reader as Effs

import Hedgehog

import TestUtils

hprop_denormalizeExpression_is_the_inverse_of_normalizeExpression :: Property
hprop_denormalizeExpression_is_the_inverse_of_normalizeExpression =
  property $ do
    expr <- forAll Parse.ASTGen.genExpression
    expr ===
      denormalizeExpression
        (unwrapRight $
         Eff.run .
         Effs.runError @Error.Error .
         Effs.runReader "" . Effs.runReader ([] :: [SM.Expression]) $
         normalizeExpression expr)
