{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Axel.Error (Error)
import qualified Axel.Error as Error (toIO)
import Axel.Haskell.File (evalFile)
import Axel.Haskell.Project (buildProject, runProject)
import Axel.Monad.Console (MonadConsole)
import Axel.Monad.Process (MonadProcess)
import Axel.Monad.Resource (MonadResource)
import Axel.Parse.Args (ModeCommand(File, Project), modeCommandParser)

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)

import Options.Applicative (execParser, info, progDesc)

newtype AppT a = AppT
  { runAppT :: ExceptT Error IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadConsole
             , MonadError Error
             , MonadIO
             , MonadProcess
             , MonadResource
             )

app :: ModeCommand -> AppT ()
app (File filePath) = evalFile filePath
app Project = buildProject >> runProject

main :: IO ()
main = do
  modeCommand <-
    execParser $ info modeCommandParser (progDesc "The command to run.")
  Error.toIO $ runAppT $ app modeCommand
