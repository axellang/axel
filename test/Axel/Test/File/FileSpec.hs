{-# LANGUAGE TypeApplications #-}

module Axel.Test.File.FileSpec where

import Axel.Eff.Console as Console
import Axel.Eff.FileSystem as FS
import Axel.Eff.Ghci as Ghci
import Axel.Eff.Log as Log
import Axel.Eff.Process as Proc
import Axel.Eff.Resource as Res
import Axel.Error as Error
import Axel.Haskell.File
import Axel.Macros
import Axel.Sourcemap as SM

import Control.Monad.Freer as Effs
import Control.Monad.Freer.State (evalState)

import Data.ByteString.Lazy.Char8 as C

import qualified Data.Map as M

import System.FilePath

import Test.Tasty
import Test.Tasty.Golden

test_transpileSource_golden :: IO TestTree
test_transpileSource_golden = do
  axelFiles <- findByExtension [".axel_golden"] "test/Axel/Test/File"
  pure $
    testGroup "`transpileSource` golden tests" $ do
      axelFile <- axelFiles
      let hsFile = replaceExtension axelFile ".hs_golden"
      pure $
        goldenVsString
          (takeBaseName axelFile)
          hsFile
          (C.pack . SM.raw <$>
           (Effs.runM .
            Console.runEff .
            Log.runEffAsConsole .
            Ghci.runEff .
            evalState (M.empty :: ModuleInfo) .
            Res.runEff .
            Proc.runEff .
            FS.runEff . Error.unsafeRunEff @Error.Error . Console.runEff)
             (FS.readFile axelFile >>= transpileSource (takeBaseName axelFile)))
