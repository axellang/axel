{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Eff.Console where

import Prelude hiding (putStr, putStrLn)
import qualified Prelude

import Control.Monad.Freer (type (~>), Eff, LastMember, Member, interpretM)
import Control.Monad.Freer.TH (makeEffect)

import qualified System.Console.ANSI as ANSI (getTerminalSize)

data Console r where
  GetTerminalSize :: Console (Maybe (Int, Int))
  PutStr :: String -> Console ()

makeEffect ''Console

runConsole :: (LastMember IO effs) => Eff (Console ': effs) ~> Eff effs
runConsole =
  interpretM $ \case
    GetTerminalSize -> ANSI.getTerminalSize
    PutStr str -> Prelude.putStr str

putStrLn :: (Member Console effs) => String -> Eff effs ()
putStrLn = putStr . (<> "\n")

putHorizontalLine :: (Member Console effs) => Eff effs ()
putHorizontalLine = do
  maybeTerminalSize <- getTerminalSize
  case maybeTerminalSize of
    Just (_, width) -> putStrLn $ replicate width '\x2500'
    Nothing -> pure ()
