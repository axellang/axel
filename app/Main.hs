{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Axel.Error (Error)
import qualified Axel.Error as Error (toIO)
import Axel.Haskell.File (evalFile)
import Axel.Haskell.Project (buildProject, runProject)
import Axel.Parse.Args (ModeCommand(File, Project), modeCommandParser)

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)

import Options.Applicative (execParser, info, progDesc)

newtype AppM a = AppM
  { runAppM :: ExceptT Error IO a
  } deriving (Functor, Applicative, Monad, MonadError Error, MonadIO)

runAppM' :: AppM a -> IO a
runAppM' = Error.toIO . runAppM

app :: ModeCommand -> AppM ()
app (File filePath) = evalFile filePath
app Project = buildProject >> runProject

main :: IO ()
main = do
  modeCommand <-
    execParser $ info modeCommandParser (progDesc "The command to run.")
  runAppM' $ app modeCommand
