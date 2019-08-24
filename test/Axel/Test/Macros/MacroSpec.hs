{-# LANGUAGE TypeApplications #-}

module Axel.Test.Macros.MacroSpec where

import Axel.Eff.App
import Axel.Eff.FileSystem as FS
import Axel.Haskell.File
import Axel.Macros
import Axel.Sourcemap as SM

import Control.Monad.Freer.State (evalState)

import Data.ByteString.Lazy.Char8 as C

import qualified Data.Map as M

import System.FilePath

import Test.Tasty
import Test.Tasty.Golden

test_macroExpansion_golden :: IO TestTree
test_macroExpansion_golden = do
  axelFiles <- findByExtension [".axel_golden"] "test/Axel/Test/Macros"
  pure $
    testGroup "macro expansion golden tests" $ do
      axelFile <- axelFiles
      let hsFile = replaceExtension axelFile ".hs_golden"
      pure $
        goldenVsString
          (takeBaseName axelFile)
          hsFile
          (C.pack . SM.raw <$>
           (runApp . evalState (M.empty :: ModuleInfo))
             (FS.readFile axelFile >>= transpileSource (takeBaseName axelFile)))
