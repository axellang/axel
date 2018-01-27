module Main where

import Lihsp.Transpile (transpileFile)

import System.Environment (getArgs)

main :: IO ()
main = do
  [sourceFilePath] <- getArgs
  transpileFile sourceFilePath
