module Axel.Test.DenormalizeSpec where

import Axel.Prelude

import Axel.Denormalize
import Axel.Eff.Error as Error
import Axel.Normalize
import Axel.Sourcemap as SM
import qualified Axel.Test.ASTGen as ASTGen

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem
import qualified Polysemy.Reader as Sem

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
        renderError
        (Sem.run $
         Sem.runError @Error.Error $
         Sem.runReader (FilePath "") $
         Sem.runReader ([] :: [SM.Expression]) $
         normalizeExpression (denormalizeExpression expr))

hprop_normalizeStatement_is_the_inverse_of_denormalizeStatement :: Property
hprop_normalizeStatement_is_the_inverse_of_denormalizeStatement =
  property $ do
    stmt <- forAll ASTGen.genStatement
    stmt =$=
      unwrapRight
        renderError
        (Sem.run $
         Sem.runError @Error.Error $
         Sem.runReader (FilePath "") $
         Sem.runReader ([] :: [SM.Expression]) $
         normalizeStatement (denormalizeStatement stmt))
