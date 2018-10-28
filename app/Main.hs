{-# LANGUAGE DataKinds #-}

module Main where

import Prelude hiding (putStrLn)

import Axel.Eff.Console (putStrLn)
import qualified Axel.Eff.Console as Effs (Console)
import qualified Axel.Eff.Console as Console (runEff)
import qualified Axel.Eff.FileSystem as FS (runEff)
import qualified Axel.Eff.FileSystem as Effs (FileSystem)
import qualified Axel.Eff.Process as Proc (runEff)
import qualified Axel.Eff.Process as Effs (Process)
import qualified Axel.Eff.Resource as Res (runEff)
import qualified Axel.Eff.Resource as Effs (Resource)
import Axel.Error (Error)
import qualified Axel.Error as Error (runEff)
import Axel.Haskell.File (transpileFile')
import Axel.Haskell.Project (buildProject, runProject)
import Axel.Haskell.Stack (axelStackageVersion)
import Axel.Parse.Args (Command(File, Project, Version), commandParser)

import Control.Monad (void)
import Control.Monad.Freer (Eff)
import qualified Control.Monad.Freer as Effs (runM)
import qualified Control.Monad.Freer.Error as Effs (Error)

import Options.Applicative ((<**>), execParser, helper, info, progDesc)

type AppEffs
   = Eff '[ Effs.Console, Effs.Error Error, Effs.FileSystem, Effs.Process, Effs.Resource, IO]

runApp :: AppEffs a -> IO a
runApp =
  Effs.runM .
  Res.runEff . Proc.runEff . FS.runEff . Error.runEff . Console.runEff

app :: Command -> AppEffs ()
app (File filePath) = void $ transpileFile' filePath
app Project = buildProject >> runProject
app Version = putStrLn $ "Axel version " <> axelStackageVersion

main :: IO ()
main = do
  modeCommand <-
    execParser $
    info (commandParser <**> helper) (progDesc "The command to run.")
  runApp $ app modeCommand
