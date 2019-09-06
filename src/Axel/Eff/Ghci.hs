{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Eff.Ghci where

import Control.Monad (void)

import Polysemy (Embed, Member, Sem, embed, interpret, makeSem)

import Language.Haskell.Ghcid (startGhci, stopGhci)
import qualified Language.Haskell.Ghcid as Ghci (Ghci, exec)

data Ghci m a where
  Exec :: Ghci.Ghci -> String -> Ghci m [String]
  Start :: Ghci m Ghci.Ghci
  Stop :: Ghci.Ghci -> Ghci m ()

makeSem ''Ghci

runGhci :: (Member (Embed IO) effs) => Sem (Ghci ': effs) a -> Sem effs a
runGhci =
  interpret $ \case
    Exec ghci command -> embed $ Ghci.exec ghci command
    Start -> embed $ fst <$> startGhci "ghci" Nothing mempty
    Stop ghci -> embed $ stopGhci ghci

enableJsonErrors :: (Member Ghci effs) => Ghci.Ghci -> Sem effs ()
enableJsonErrors ghci = void $ exec ghci ":set -ddump-json"
