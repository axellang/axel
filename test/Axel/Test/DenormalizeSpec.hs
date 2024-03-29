module Axel.Test.DenormalizeSpec where

import Axel.Prelude

import Axel.Denormalize
import Axel.Eff.Error as Error
import Axel.Normalize
import Axel.Sourcemap as SM
import qualified Axel.Test.ASTGen as ASTGen

import Control.Monad

import qualified Effectful as Eff
import qualified Effectful.Error.Static as Eff
import qualified Effectful.Reader.Static as Eff

import Hedgehog

import TestUtils

(=$=) :: (Functor f, Eq (f ()), Show (f ()), MonadTest m) => f a -> f b -> m ()
a =$= b = void a === void b

hprop_normalizeExpression_is_the_inverse_of_denormalizeExpression :: Property
hprop_normalizeExpression_is_the_inverse_of_denormalizeExpression =
  property $ do
    expr <- forAll ASTGen.genExpression
    expr =$=
      unwrapRight
        renderError
        (Eff.runPureEff $
         Eff.runErrorNoCallStack @Error.Error $
         Eff.runReader (FilePath "") $
         Eff.runReader ([] :: [SM.Expression]) $
         normalizeExpression (denormalizeExpression expr))

hprop_normalizeStatement_is_the_inverse_of_denormalizeStatement :: Property
hprop_normalizeStatement_is_the_inverse_of_denormalizeStatement =
  property $ do
    stmt <- forAll ASTGen.genStatement
    stmt =$=
      unwrapRight
        renderError
        (Eff.runPureEff $
         Eff.runErrorNoCallStack @Error.Error $
         Eff.runReader (FilePath "") $
         Eff.runReader ([] :: [SM.Expression]) $
         normalizeStatement (denormalizeStatement stmt))
