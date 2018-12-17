{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Eff.Ghci where

import Control.Monad.Freer (type (~>), Eff, LastMember, interpretM)
import Control.Monad.Freer.TH (makeEffect)

import Language.Haskell.Ghcid (startGhci, stopGhci)
import qualified Language.Haskell.Ghcid as Ghci (Ghci, exec)

data Ghci r where
  Exec :: Ghci.Ghci -> String -> Ghci [String]
  Start :: Ghci Ghci.Ghci
  Stop :: Ghci.Ghci -> Ghci ()

makeEffect ''Ghci

runEff :: (LastMember IO effs) => Eff (Ghci ': effs) ~> Eff effs
runEff =
  interpretM $ \case
      Exec ghci command ->
        Ghci.exec ghci command
      Start ->
        fst <$> startGhci "ghci" Nothing mempty
      Stop ghci ->
        stopGhci ghci
