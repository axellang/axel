{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Axel.Eff.Console where

import Axel.Prelude

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Polysemy as Sem

import qualified System.Console.ANSI as ANSI (getTerminalSize)

data Console m a where
  GetTerminalSize :: Console m (Maybe (Int, Int))
  PutStr :: Text -> Console m ()

Sem.makeSem ''Console

runConsole ::
     (Sem.Member (Sem.Embed IO) effs)
  => Sem.Sem (Console ': effs) a
  -> Sem.Sem effs a
runConsole =
  Sem.interpret $ \case
    GetTerminalSize -> Sem.embed ANSI.getTerminalSize
    PutStr str -> Sem.embed $ T.putStr str

putStrLn :: (Sem.Member Console effs) => Text -> Sem.Sem effs ()
putStrLn = putStr . (<> "\n")

putHorizontalLine :: (Sem.Member Console effs) => Sem.Sem effs ()
putHorizontalLine = do
  maybeTerminalSize <- getTerminalSize
  case maybeTerminalSize of
    Just (_, width) -> putStrLn $ T.replicate width "\x2500"
    Nothing -> pure ()
