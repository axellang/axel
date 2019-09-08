{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Eff.App where

import Axel.Eff.Console (Console, runConsole)
import Axel.Eff.Error (Error, unsafeRunError)
import Axel.Eff.FileSystem (FileSystem, runFileSystem)
import Axel.Eff.Ghci (Ghci, runGhci)
import Axel.Eff.Log (Log, runLogAsFileSystem)
import Axel.Eff.Process (Process, runProcess)
import Axel.Eff.Random (Random, runRandom)
import Axel.Eff.Resource (Resource, runResource)

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem

type AppEffs
   = '[ Log, Console, Sem.Error Error, FileSystem, Ghci, Process, Resource, Random, Sem.Embed IO]

runApp :: Sem.Sem AppEffs a -> IO a
runApp =
  Sem.runM .
  runRandom .
  runResource .
  runProcess .
  runGhci .
  runFileSystem .
  unsafeRunError . runConsole . runLogAsFileSystem "axelCompilation.log"
