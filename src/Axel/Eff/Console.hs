{-# LANGUAGE TemplateHaskell #-}

module Axel.Eff.Console where

import Axel.Prelude

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Effectful ((:>))
import qualified Effectful as Eff
import qualified Effectful.Dispatch.Dynamic as Eff
import qualified Effectful.TH as Eff

import qualified System.Console.ANSI as ANSI (getTerminalSize)

data Console :: Eff.Effect where
  GetTerminalSize :: Console m (Maybe (Int, Int))
  PutStr :: Text -> Console m ()

Eff.makeEffect ''Console

runConsole :: (Eff.IOE :> effs) => Eff.Eff (Console ': effs) a -> Eff.Eff effs a
runConsole =
  Eff.interpret $ \_ ->
    \case
      GetTerminalSize -> Eff.liftIO ANSI.getTerminalSize
      PutStr str -> Eff.liftIO $ T.putStr str

putStrLn :: (Console :> effs) => Text -> Eff.Eff effs ()
putStrLn = putStr . (<> "\n")

putHorizontalLine :: (Console :> effs) => Eff.Eff effs ()
putHorizontalLine = do
  maybeTerminalSize <- getTerminalSize
  case maybeTerminalSize of
    Just (_, width) -> putStrLn $ T.replicate width "\x2500"
    Nothing -> pure ()
