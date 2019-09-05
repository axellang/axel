{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Test.Eff.AppMock where

import qualified Axel.Eff.Console as Effs
import Axel.Eff.Error as Error
import qualified Axel.Eff.FileSystem as Effs
import qualified Axel.Eff.Ghci as Effs
import qualified Axel.Eff.Log as Effs
import qualified Axel.Eff.Process as Effs
import qualified Axel.Eff.Resource as Effs
import Axel.Test.Eff.ConsoleMock
import Axel.Test.Eff.FileSystemMock
import Axel.Test.Eff.GhciMock
import Axel.Test.Eff.ProcessMock
import Axel.Test.Eff.ResourceMock

import Control.Monad.Freer as Effs
import Control.Monad.Freer.Error as Effs

type AppEffs
   = '[ Effs.Log, Effs.Console, Effs.Error Error.Error, Effs.Ghci, Effs.Process, Effs.FileSystem, Effs.Resource, Effs.Error String]

runApp ::
     (processEffs ~ '[ Effs.FileSystem, Effs.Resource, Effs.Error String])
  => ConsoleState
  -> FileSystemState
  -> GhciState
  -> ProcessState processEffs
  -> Eff AppEffs a
  -> ( (((a, ConsoleState), GhciState), ProcessState processEffs)
     , FileSystemState)
runApp origConsoleState origFSState origGhciState origProcState =
  Effs.run .
  unsafeRunError .
  runResource .
  runFileSystem origFSState .
  runProcess origProcState .
  runGhci origGhciState .
  unsafeRunError . runConsole origConsoleState . Effs.ignoreLog

evalApp ::
     (processEffs ~ '[ Effs.FileSystem, Effs.Resource, Effs.Error String])
  => ConsoleState
  -> FileSystemState
  -> GhciState
  -> ProcessState processEffs
  -> Eff AppEffs a
  -> a
evalApp origConsoleState origFSState origGhciState origProcState =
  fst .
  fst .
  fst . fst . runApp origConsoleState origFSState origGhciState origProcState

evalApp' :: Eff AppEffs a -> a
evalApp' = evalApp origConsoleState origFSState origGhciState origProcState
  where
    origConsoleState = mkConsoleState
    origFSState = mkFileSystemState []
    origGhciState = mkGhciState []
    origProcState = mkProcessState [] []
