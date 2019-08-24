{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Eff.Log where

import Prelude hiding (appendFile, putStr, writeFile)

import Axel.Eff.Console (Console, putStr)
import Axel.Eff.FileSystem (FileSystem, appendFile, writeFile)

import Control.Monad.Freer (type (~>), Eff, Member, interpret)
import Control.Monad.Freer.TH (makeEffect)

data Log r where
  LogStr :: String -> Log ()

makeEffect ''Log

runLogAsConsole :: (Member Console effs) => Eff (Log ': effs) ~> Eff effs
runLogAsConsole =
  interpret $ \case
    LogStr str -> putStr str

runLogAsFileSystem ::
     (Member FileSystem effs) => FilePath -> Eff (Log ': effs) a -> Eff effs a
runLogAsFileSystem logFilePath action = do
  writeFile logFilePath ""
  interpret
    (\case
       LogStr str -> appendFile logFilePath str)
    action

ignoreLog :: Eff (Log ': effs) ~> Eff effs
ignoreLog =
  interpret $ \case
    LogStr _ -> pure ()

logStrLn :: (Member Log effs) => String -> Eff effs ()
logStrLn str = logStr (str <> "\n")
