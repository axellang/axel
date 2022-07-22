{-# LANGUAGE TemplateHaskell #-}

module Axel.Eff.Log where

import Axel.Prelude

import Axel.Eff.Console (Console, putStr)
import Axel.Eff.FileSystem (FileSystem, appendFile, writeFile)

import Effectful ((:>))
import qualified Effectful as Eff
import qualified Effectful.Dispatch.Dynamic as Eff
import qualified Effectful.TH as Eff

data Log :: Eff.Effect where
  LogStr :: Text -> Log m ()

Eff.makeEffect ''Log

runLogAsConsole ::
     (Console :> effs) => Eff.Eff (Log ': effs) a -> Eff.Eff effs a
runLogAsConsole =
  Eff.interpret $ \_ ->
    \case
      LogStr str -> putStr str

runLogAsFileSystem ::
     (FileSystem :> effs)
  => FilePath
  -> Eff.Eff (Log ': effs) a
  -> Eff.Eff effs a
runLogAsFileSystem logFilePath action = do
  writeFile logFilePath ""
  Eff.interpret
    (\_ ->
       \case
         LogStr str -> appendFile logFilePath str)
    action

ignoreLog :: Eff.Eff (Log ': effs) a -> Eff.Eff effs a
ignoreLog =
  Eff.interpret $ \_ ->
    \case
      LogStr _ -> pure ()

logStrLn :: (Log :> effs) => Text -> Eff.Eff effs ()
logStrLn str = logStr (str <> "\n")
