{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Eff.Ghci where

import Control.Monad (void)
import Control.Monad.Freer (type (~>), Eff, LastMember, Member, interpretM)
import Control.Monad.Freer.TH (makeEffect)

import Language.Haskell.Ghcid (startGhci, stopGhci)
import qualified Language.Haskell.Ghcid as Ghci (Ghci, exec)

data Ghci r where
  Exec :: Ghci.Ghci -> String -> Ghci [String]
  Start :: Ghci Ghci.Ghci
  Stop :: Ghci.Ghci -> Ghci ()

makeEffect ''Ghci

runGhci :: (LastMember IO effs) => Eff (Ghci ': effs) ~> Eff effs
runGhci =
  interpretM $ \case
    Exec ghci command -> Ghci.exec ghci command
    Start -> fst <$> startGhci "ghci" Nothing mempty
    Stop ghci -> stopGhci ghci

enableJsonErrors :: (Member Ghci effs) => Ghci.Ghci -> Eff effs ()
enableJsonErrors ghci = void $ exec ghci ":set -ddump-json"
