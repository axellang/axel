{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Eff.Log where

import Prelude hiding (appendFile, putStr, writeFile)

import Axel.Eff.Console (Console, putStr)
import Axel.Eff.FileSystem (FileSystem, appendFile, writeFile)

import qualified Polysemy as Sem

data Log m a where
  LogStr :: String -> Log m ()

Sem.makeSem ''Log

runLogAsConsole ::
     (Sem.Member Console effs) => Sem.Sem (Log ': effs) a -> Sem.Sem effs a
runLogAsConsole =
  Sem.interpret $ \case
    LogStr str -> putStr str

runLogAsFileSystem ::
     (Sem.Member FileSystem effs)
  => FilePath
  -> Sem.Sem (Log ': effs) a
  -> Sem.Sem effs a
runLogAsFileSystem logFilePath action = do
  writeFile logFilePath ""
  Sem.interpret
    (\case
       LogStr str -> appendFile logFilePath str)
    action

ignoreLog :: Sem.Sem (Log ': effs) a -> Sem.Sem effs a
ignoreLog =
  Sem.interpret $ \case
    LogStr _ -> pure ()

logStrLn :: (Sem.Member Log effs) => String -> Sem.Sem effs ()
logStrLn str = logStr (str <> "\n")
