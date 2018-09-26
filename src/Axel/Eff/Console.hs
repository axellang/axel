{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Eff.Console where

import Prelude hiding (putStr)
import qualified Prelude

import Control.Monad.Freer
  ( type (~>)
  , Eff
  , LastMember
  , Member
  , interpretM
  , send
  )

data Console r where
  PutStr :: String -> Console ()

putStr :: (Member Console effs) => String -> Eff effs ()
putStr = send . PutStr

runEff :: (LastMember IO effs) => Eff (Console ': effs) ~> Eff effs
runEff =
  interpretM
    (\case
       PutStr str -> Prelude.putStr str)

putStrLn :: (Member Console effs) => String -> Eff effs ()
putStrLn str = putStr (str <> "\n")
