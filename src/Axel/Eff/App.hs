{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Eff.App where

import Axel.Eff.Console (Console, runConsole)
import Axel.Eff.Error (Error, unsafeRunError)
import Axel.Eff.FileSystem (FileSystem, runFileSystem)
import Axel.Eff.Ghci (Ghci, runGhci)
import Axel.Eff.Log (Log, runLogAsFileSystem)
import Axel.Eff.Process (Process, runProcess)
import Axel.Eff.Resource (Resource, runResource)

import Control.Monad.Freer (type (~>), Eff, runM)
import qualified Control.Monad.Freer.Error as Effs (Error)

type AppEffs
   = Eff '[ Log, Console, Effs.Error Error, FileSystem, Ghci, Process, Resource, IO]

runApp :: AppEffs ~> IO
runApp =
  runM .
  runResource .
  runProcess .
  runGhci .
  runFileSystem .
  unsafeRunError . runConsole . runLogAsFileSystem "axelCompilation.log"
