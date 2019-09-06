{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Eff.Console where

import Prelude hiding (putStr, putStrLn)
import qualified Prelude

import qualified Polysemy as Sem

import qualified System.Console.ANSI as ANSI (getTerminalSize)

data Console m a where
  GetTerminalSize :: Console m (Maybe (Int, Int))
  PutStr :: String -> Console m ()

Sem.makeSem ''Console

runConsole ::
     (Sem.Member (Sem.Embed IO) effs)
  => Sem.Sem (Console ': effs) a
  -> Sem.Sem effs a
runConsole =
  Sem.interpret $ \case
    GetTerminalSize -> Sem.embed ANSI.getTerminalSize
    PutStr str -> Sem.embed $ Prelude.putStr str

putStrLn :: (Sem.Member Console effs) => String -> Sem.Sem effs ()
putStrLn = putStr . (<> "\n")

putHorizontalLine :: (Sem.Member Console effs) => Sem.Sem effs ()
putHorizontalLine = do
  maybeTerminalSize <- getTerminalSize
  case maybeTerminalSize of
    Just (_, width) -> putStrLn $ replicate width '\x2500'
    Nothing -> pure ()
