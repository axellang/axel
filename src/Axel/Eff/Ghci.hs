{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Eff.Ghci where

import Control.Monad.Freer (type (~>), Eff, LastMember, interpretM)
import Control.Monad.Freer.TH (makeEffect)

import Data.IORef (newIORef, readIORef, writeIORef)

import Language.Haskell.Ghcid (startGhci, stopGhci)
import qualified Language.Haskell.Ghcid as Ghci (Ghci, exec)

data Ghci r where
  Exec :: String -> Ghci [String]
  Start :: Ghci ()
  Stop :: Ghci ()

makeEffect ''Ghci

runEff :: (LastMember IO effs) => Eff (Ghci ': effs) ~> Eff effs
runEff =
  interpretM $ \x -> do
    ghciRef <- newIORef (Nothing :: Maybe Ghci.Ghci)
    case x of
      Exec command -> do
        Just ghci <- readIORef ghciRef
        Ghci.exec ghci command
      Start -> do
        (ghci, _) <- startGhci "ghci" Nothing mempty
        writeIORef ghciRef $ Just ghci
      Stop -> do
        Just ghci <- readIORef ghciRef
        stopGhci ghci
        writeIORef ghciRef Nothing
