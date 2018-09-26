{-# LANGUAGE TypeApplications #-}

module Axel.Test.File.FileSpec where

import Axel.Eff.Console as Console
import Axel.Eff.FileSystem as FS
import Axel.Eff.Process as Proc
import Axel.Eff.Resource as Res
import Axel.Error as Error
import Axel.Haskell.File

import Control.Monad.Freer as Effs

import Data.ByteString.Lazy.Char8 as C

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
          (C.pack <$>
           (Effs.runM .
            Res.runEff .
            Proc.runEff . FS.runEff . Error.runEff @Error . Console.runEff)
             (FS.readFile axelFile >>= transpileSource))
