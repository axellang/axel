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

import qualified Effectful as Eff
import qualified Effectful.Error.Static as Eff

type AppEffs
   = '[ Effs.Log, Effs.Console, Eff.Error Error.Error, Effs.Ghci, Effs.Process, Effs.FileSystem, Effs.Resource, Eff.Error Text]

runApp ::
     (processEffs ~ '[ Effs.FileSystem, Effs.Resource, Eff.Error Text])
  => ConsoleState
  -> FileSystemState
  -> GhciState
  -> ProcessState processEffs
  -> Eff.Eff AppEffs a
  -> ( (((a, ConsoleState), GhciState), ProcessState processEffs)
     , FileSystemState)
runApp origConsoleState origFSState origGhciState origProcState =
  Eff.runPureEff .
  unsafeRunError id .
  runResource .
  runFileSystem origFSState .
  runProcess origProcState .
  runGhci origGhciState .
  unsafeRunError renderError . runConsole origConsoleState . Effs.ignoreLog

evalApp ::
     (processEffs ~ '[ Effs.FileSystem, Effs.Resource, Eff.Error Text])
  => ConsoleState
  -> FileSystemState
  -> GhciState
  -> ProcessState processEffs
  -> Eff.Eff AppEffs a
  -> a
evalApp origConsoleState origFSState origGhciState origProcState =
  fst .
  fst .
  fst . fst . runApp origConsoleState origFSState origGhciState origProcState

evalApp' :: Eff.Eff AppEffs a -> a
evalApp' = evalApp origConsoleState origFSState origGhciState origProcState
  where
    origConsoleState = mkConsoleState
    origFSState = mkFileSystemState []
    origGhciState = mkGhciState []
    origProcState = mkProcessState [] []
