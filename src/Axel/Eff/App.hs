module Axel.Eff.App where

import Axel.Prelude

import Axel.Eff.Console (Console, runConsole)
import Axel.Eff.Error (Error, renderError, unsafeRunError)
import Axel.Eff.FileSystem (FileSystem, runFileSystem)
import Axel.Eff.Ghci (Ghci, runGhci)
import Axel.Eff.Log (Log, runLogAsFileSystem)
import Axel.Eff.Process (Process, runProcess)
import Axel.Eff.Random (Random, runRandom)
import Axel.Eff.Resource (Resource, runResource)
import Axel.Eff.Time (Time, runTime)

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem

type AppEffs
   = '[ Sem.Error Error, Log, Console, FileSystem, Ghci, Process, Resource, Random, Time, Sem.Embed IO]

runApp :: Sem.Sem AppEffs a -> IO a
runApp =
  Sem.runM .
  runTime .
  runRandom .
  runResource .
  runProcess .
  runGhci .
  runFileSystem .
  runConsole .
  runLogAsFileSystem (FilePath "axelCompilation.log") .
  unsafeRunError renderError
