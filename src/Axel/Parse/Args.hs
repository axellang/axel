module Axel.Parse.Args where

import Data.Semigroup ((<>))

import Options.Applicative
  ( Parser
  , argument
  , command
  , info
  , metavar
  , progDesc
  , str
  , subparser
  )

data ModeCommand
  = File FilePath
  | Project

modeCommandParser :: Parser ModeCommand
modeCommandParser = subparser $ projectCommand <> fileCommand
  where
    fileCommand =
      command
        "file"
        (info (File <$> argument str (metavar "FILE")) $
         progDesc "Build and run a single file")
    projectCommand =
      command
        "project"
        (info (pure Project) $ progDesc "Build and run the project")
