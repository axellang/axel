{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

-- NOTE Inspired by `fused-effects-lens`.
module Axel.Eff.Lens where

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
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Reader (asks)
import qualified Control.Monad.Freer.Reader as Effs (Reader)
import qualified Control.Monad.Freer.State as Effs (State)
import Control.Monad.Freer.State (gets, modify)

import Data.Profunctor.Unsafe (Profunctor(( #. )))

{-# INLINE view #-}
view :: (Member (Effs.Reader s) effs) => Getting a s a -> Eff effs a
view l = asks $ getConst #. l Const

{-# INLINE views #-}
views ::
     (Member (Effs.Reader s) effs)
  => LensLike' (Const r) s a
  -> (a -> r)
  -> Eff effs r
views l f = asks $ getConst #. l (Const #. f)

{-# INLINE use #-}
use :: (Member (Effs.State s) effs) => Getting a s a -> Eff effs a
use = gets . Lens.view

{-# INLINE uses #-}
uses ::
     (Member (Effs.State s) effs)
  => LensLike' (Const r) s a
  -> (a -> r)
  -> Eff effs r
uses l f = gets $ Lens.views l f

{-# INLINE assign #-}
assign :: (Member (Effs.State s) effs) => ASetter s s a b -> b -> Eff effs ()
assign l b = modify $ set l b

infixr 4 .=

{-# INLINE (.=) #-}
(.=) :: (Member (Effs.State s) effs) => ASetter s s a b -> b -> Eff effs ()
(.=) = assign

{-# INLINE modifying #-}
modifying ::
     (Member (Effs.State s) effs) => ASetter s s a b -> (a -> b) -> Eff effs ()
modifying l f = modify $ over l f

infixr 4 %=

{-# INLINE (%=) #-}
(%=) ::
     (Member (Effs.State s) effs) => ASetter s s a b -> (a -> b) -> Eff effs ()
(%=) = modifying

{-# INLINE (<>=) #-}
(<>=) ::
     (Member (Effs.State s) effs, Monoid a) => ASetter' s a -> a -> Eff effs ()
l <>= a = modify $ l <>~ a
