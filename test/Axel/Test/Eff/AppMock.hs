{-# LANGUAGE GADTs #-}

module Axel.Test.Eff.AppMock where

import Axel.Prelude

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

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem

type AppEffs
   = '[ Effs.Log, Effs.Console, Sem.Error Error.Error, Effs.Ghci, Effs.Process, Effs.FileSystem, Effs.Resource, Sem.Error Text]

runApp ::
     (processEffs ~ '[ Effs.FileSystem, Effs.Resource, Sem.Error Text])
  => ConsoleState
  -> FileSystemState
  -> GhciState
  -> ProcessState processEffs
  -> Sem.Sem AppEffs a
  -> ( FileSystemState
     , (ProcessState processEffs, (GhciState, (ConsoleState, a))))
runApp origConsoleState origFSState origGhciState origProcState =
  Sem.run .
  unsafeRunError id .
  runResource .
  runFileSystem origFSState .
  runProcess origProcState .
  runGhci origGhciState .
  unsafeRunError renderError . runConsole origConsoleState . Effs.ignoreLog

evalApp ::
     (processEffs ~ '[ Effs.FileSystem, Effs.Resource, Sem.Error Text])
  => ConsoleState
  -> FileSystemState
  -> GhciState
  -> ProcessState processEffs
  -> Sem.Sem AppEffs a
  -> a
evalApp origConsoleState origFSState origGhciState origProcState =
  snd .
  snd .
  snd . snd . runApp origConsoleState origFSState origGhciState origProcState

evalApp' :: Sem.Sem AppEffs a -> a
evalApp' = evalApp origConsoleState origFSState origGhciState origProcState
  where
    origConsoleState = mkConsoleState
    origFSState = mkFileSystemState []
    origGhciState = mkGhciState []
    origProcState = mkProcessState [] []
