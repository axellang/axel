module Axel.Test.File.FileSpec where

import Axel.Error as Error
import Axel.Haskell.File
import Axel.Monad.FileSystem as FS

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
          (C.pack <$> Error.toIO (FS.readFile axelFile >>= transpileSource))
