module Main where

import Axel.Entry (evalFile)

import System.Environment (getArgs)

main :: IO ()
main = do
  [sourceFilePath] <- getArgs
  evalFile sourceFilePath
