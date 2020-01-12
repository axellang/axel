{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Axel.Eff where

import Data.Kind (Constraint, Type)
import Data.Type.Equality ((:~:)(Refl))

import qualified Polysemy as Sem

import Unsafe.Coerce (unsafeCoerce)

type Callback effs fn a
   = forall openEffs. (Sem.Members effs openEffs) =>
                        fn (Sem.Sem openEffs a)

type Eff = (Type -> Type) -> Type -> Type

-- See https://www.reddit.com/r/haskell/comments/emj06h/force_ghc_to_accept_constraint/fdq3ztq.
type ConstraintProof (a :: Constraint) = a :~: (() :: Constraint)

-- | Use to fix errors where, given `Sem.Members subEffs effs`, GHC can't infer
--   `Sem.Members subEffs (headEffs ': effs)` even though that follows trivially.
prfMembersUnderCons ::
     forall (newEff :: Eff) (subEffs :: [Eff]) (effs :: [Eff]).
     (Sem.Members subEffs effs)
  => ConstraintProof (Sem.Members subEffs (newEff ': effs))
prfMembersUnderCons = unsafeCoerce Refl

-- | Use to fix errors where, given `(Sem.Members subEffs effs, Sem.Members subSubEffs subEffs)`,
--   GHC can't infer `Sem.Members subSubEffs effs` even though that follows trivially.
prfMembersTransitive ::
     forall (subSubEffs :: [Eff]) (subEffs :: [Eff]) (effs :: [Eff]).
     (Sem.Members subSubEffs subEffs, Sem.Members subEffs effs)
  => ConstraintProof (Sem.Members subSubEffs effs)
prfMembersTransitive = unsafeCoerce Refl
