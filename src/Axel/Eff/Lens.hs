{-# LANGUAGE GADTs #-}

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

import qualified Polysemy as Sem
import qualified Polysemy.Reader as Sem
import qualified Polysemy.State as Sem

{-# INLINE view #-}
view :: (Sem.Member (Sem.Reader s) effs) => Getting a s a -> Sem.Sem effs a
view l = Sem.asks $ getConst #. l Const

{-# INLINE views #-}
views ::
     (Sem.Member (Sem.Reader s) effs)
  => LensLike' (Const r) s a
  -> (a -> r)
  -> Sem.Sem effs r
views l f = Sem.asks $ getConst #. l (Const #. f)

{-# INLINE use #-}
use :: (Sem.Member (Sem.State s) effs) => Getting a s a -> Sem.Sem effs a
use = Sem.gets . Lens.view

{-# INLINE uses #-}
uses ::
     (Sem.Member (Sem.State s) effs)
  => LensLike' (Const r) s a
  -> (a -> r)
  -> Sem.Sem effs r
uses l f = Sem.gets $ Lens.views l f

{-# INLINE assign #-}
assign ::
     (Sem.Member (Sem.State s) effs) => ASetter s s a b -> b -> Sem.Sem effs ()
assign l b = Sem.modify $ set l b

infixr 4 .=

{-# INLINE (.=) #-}
(.=) ::
     (Sem.Member (Sem.State s) effs) => ASetter s s a b -> b -> Sem.Sem effs ()
(.=) = assign

{-# INLINE modifying #-}
modifying ::
     (Sem.Member (Sem.State s) effs)
  => ASetter s s a b
  -> (a -> b)
  -> Sem.Sem effs ()
modifying l f = Sem.modify $ over l f

infixr 4 %=

{-# INLINE (%=) #-}
(%=) ::
     (Sem.Member (Sem.State s) effs)
  => ASetter s s a b
  -> (a -> b)
  -> Sem.Sem effs ()
(%=) = modifying

{-# INLINE (<>=) #-}
(<>=) ::
     (Sem.Member (Sem.State s) effs, Monoid a)
  => ASetter' s a
  -> a
  -> Sem.Sem effs ()
l <>= a = Sem.modify $ l <>~ a
