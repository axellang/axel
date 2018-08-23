module Main where

import qualified Axel.Error as Error (toIO)
import Axel.Haskell.File (evalFile)
import Axel.Haskell.Project (buildProject, runProject)
import Axel.Monad.FileSystem (setCurrentDirectory)
import Axel.Parse.Args (ModeCommand(File, Project), modeCommandParser)

import Control.Monad.IO.Class (liftIO)

import Options.Applicative (execParser, info, progDesc)

import System.Environment (getArgs)

main :: IO ()
main = do
  modeCommand <-
    execParser $ info modeCommandParser (progDesc "The command to run.")
  Error.toIO $
    case modeCommand of
      File filePath -> evalFile filePath
      Project -> do
        [projectPath] <- liftIO getArgs
        setCurrentDirectory projectPath
        buildProject
        runProject
