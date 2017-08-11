module Lihsp.IO where

import Data.Semigroup ((<>))

import Lihsp.Transpile (transpile)

transpileFile :: FilePath -> IO ()
transpileFile path = do
  contents <- readFile $ path <> ".lihsp"
  let newContents = transpile contents
  either print (writeFile $ path <> ".hs") newContents
