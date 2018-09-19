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

data Command
  = File FilePath
  | Project
  | Version

commandParser :: Parser Command
commandParser = subparser $ projectCommand <> fileCommand <> versionCommand
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
    versionCommand =
      command
        "version"
        (info (pure Version) $
         progDesc "Display the version of the Axel compiler")
