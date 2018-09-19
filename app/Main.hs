{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Prelude hiding (putStrLn)

import Axel.Error (Error)
import qualified Axel.Error as Error (toIO)
import Axel.Haskell.File (evalFile)
import Axel.Haskell.Project (buildProject, runProject)
import Axel.Haskell.Stack (axelStackageVersion)
import Axel.Monad.Console (putStrLn)
import Axel.Parse.Args (Command(File, Project, Version), commandParser)

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)

import Options.Applicative ((<**>), execParser, helper, info, progDesc)

newtype AppM a = AppM
  { runAppM :: ExceptT Error IO a
  } deriving (Functor, Applicative, Monad, MonadError Error, MonadIO)

runAppM' :: AppM a -> IO a
runAppM' = Error.toIO . runAppM

app :: Command -> AppM ()
app (File filePath) = evalFile filePath
app Project = buildProject >> runProject
app Version = putStrLn $ "Axel version " <> axelStackageVersion

main :: IO ()
main = do
  modeCommand <-
    execParser $
    info (commandParser <**> helper) (progDesc "The command to run.")
  runAppM' $ app modeCommand
