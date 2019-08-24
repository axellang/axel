{-# LANGUAGE TypeApplications #-}

module Axel.Test.DenormalizeSpec where

import Axel.Denormalize
import Axel.Eff.Error as Error
import Axel.Normalize
import Axel.Sourcemap as SM
import qualified Axel.Test.ASTGen as ASTGen

import Control.Monad.Freer as Eff
import Control.Monad.Freer.Error (runError)
import Control.Monad.Freer.Reader (runReader)

import Hedgehog

import TestUtils

(=$=) :: (Functor f, Eq (f ()), Show (f ()), MonadTest m) => f a -> f b -> m ()
a =$= b = (() <$ a) === (() <$ b)

hprop_normalizeExpression_is_the_inverse_of_denormalizeExpression :: Property
hprop_normalizeExpression_is_the_inverse_of_denormalizeExpression =
  property $ do
    expr <- forAll ASTGen.genExpression
    expr =$=
      unwrapRight
        (Eff.run $
         runError @Error.Error $
         runReader "" $
         runReader ([] :: [SM.Expression]) $
         normalizeExpression (denormalizeExpression expr))

hprop_normalizeStatement_is_the_inverse_of_denormalizeStatement :: Property
hprop_normalizeStatement_is_the_inverse_of_denormalizeStatement =
  property $ do
    stmt <- forAll ASTGen.genStatement
    stmt =$=
      unwrapRight
        (Eff.run $
         runError @Error.Error $
         runReader "" $
         runReader ([] :: [SM.Expression]) $
         normalizeStatement (denormalizeStatement stmt))
