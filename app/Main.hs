{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Axel.Eff.App (AppEffs, runApp)
import Axel.Eff.Console (putStrLn)
import Axel.Haskell.File (convertFile', transpileFile')
import Axel.Haskell.Project (buildProject, runProject)
import Axel.Haskell.Stack (axelStackageVersion)
import Axel.Macros (ModuleInfo)
import Axel.Parse.Args (Command(Convert, File, Project, Version), commandParser)
import Control.Monad (void)
import Control.Monad.Freer.State (evalState)
import qualified Data.Map as Map (empty)
import Options.Applicative ((<**>), execParser, helper, info, progDesc)
import Prelude hiding (putStrLn)

app (Convert filePath) = (void (convertFile' filePath))
app (File filePath) =
  (void ((evalState @ModuleInfo Map.empty) (transpileFile' filePath)))
app (Project) = ((>>) buildProject runProject)
app (Version) = (putStrLn ((<>) "Axel version " axelStackageVersion))

app :: (((->) Command) (AppEffs ()))
main = do
  modeCommand <-
    execParser $
    info (commandParser <**> helper) (progDesc "The command to run.")
  runApp $ app modeCommand

main :: (IO ())
