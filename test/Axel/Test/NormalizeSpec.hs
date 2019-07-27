{-# LANGUAGE TypeApplications #-}

module Axel.Test.NormalizeSpec where

import Axel.Denormalize
import Axel.Normalize
import Axel.Sourcemap as SM
import Axel.Test.MockUtils
import qualified Axel.Test.Parse.ASTGen as Parse.ASTGen

import Control.Monad.Freer as Eff
import qualified Control.Monad.Freer.Error as Effs
import qualified Control.Monad.Freer.Reader as Effs

import Hedgehog

hprop_denormalizeExpression_is_the_inverse_of_normalizeExpression :: Property
hprop_denormalizeExpression_is_the_inverse_of_normalizeExpression =
  property $ do
    expr <- forAll Parse.ASTGen.genExpression
    expr ===
      denormalizeExpression
        (unwrapRight $
         Eff.run . Effs.runError @SM.Error . Effs.runReader "" $
         normalizeExpression expr)
