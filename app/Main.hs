module Main where

import Lihsp.Entry (evalFile)

import System.Environment (getArgs)

main :: IO ()
main = do
  [sourceFilePath] <- getArgs
  evalFile sourceFilePath
