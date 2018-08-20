module Main where

import Axel.Haskell.Project (buildProject, runProject)

import System.Directory (setCurrentDirectory)
import System.Environment (getArgs)

main :: IO ()
main = do
  [projectPath] <- getArgs
  setCurrentDirectory projectPath
  buildProject
  runProject
