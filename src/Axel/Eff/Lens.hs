-- NOTE Inspired by `fused-effects-lens`.
module Axel.Eff.Lens where

import Axel.Prelude

import Control.Lens
  ( ASetter
  , ASetter'
  , Const(Const)
  , Const
  , Getting
  , LensLike'
  , (<>~)
  , getConst
  , over
  , set
  )
import qualified Control.Lens.Getter as Lens (view, views)

import Data.Profunctor.Unsafe (Profunctor(( #. )))

import Effectful ((:>))
import qualified Effectful as Eff
import qualified Effectful.Reader.Static as Eff
import qualified Effectful.State.Static.Local as Eff

{-# INLINE view #-}
view :: (Eff.Reader s :> effs) => Getting a s a -> Eff.Eff effs a
view l = Eff.asks $ getConst #. l Const

{-# INLINE views #-}
views ::
     (Eff.Reader s :> effs)
  => LensLike' (Const r) s a
  -> (a -> r)
  -> Eff.Eff effs r
views l f = Eff.asks $ getConst #. l (Const #. f)

{-# INLINE use #-}
use :: (Eff.State s :> effs) => Getting a s a -> Eff.Eff effs a
use = Eff.gets . Lens.view

{-# INLINE uses #-}
uses ::
     (Eff.State s :> effs)
  => LensLike' (Const r) s a
  -> (a -> r)
  -> Eff.Eff effs r
uses l f = Eff.gets $ Lens.views l f

{-# INLINE assign #-}
assign :: (Eff.State s :> effs) => ASetter s s a b -> b -> Eff.Eff effs ()
assign l b = Eff.modify $ set l b

infixr 4 .=

{-# INLINE (.=) #-}
(.=) :: (Eff.State s :> effs) => ASetter s s a b -> b -> Eff.Eff effs ()
(.=) = assign

{-# INLINE modifying #-}
modifying ::
     (Eff.State s :> effs) => ASetter s s a b -> (a -> b) -> Eff.Eff effs ()
modifying l f = Eff.modify $ over l f

infixr 4 %=

{-# INLINE (%=) #-}
(%=) :: (Eff.State s :> effs) => ASetter s s a b -> (a -> b) -> Eff.Eff effs ()
(%=) = modifying

{-# INLINE (<>=) #-}
(<>=) :: (Eff.State s :> effs, Monoid a) => ASetter' s a -> a -> Eff.Eff effs ()
l <>= a = Eff.modify $ l <>~ a
