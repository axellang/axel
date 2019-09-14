module Axel.Test.Eff.FileSystemSpec where

import Axel.Prelude

import qualified Axel.Eff.FileSystem as FS
import qualified Axel.Test.Eff.FileSystemMock as Mock
import Axel.Utils.FilePath

import Control.Lens

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem

import Test.Tasty.Hspec

import TestUtils

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

spec_FileSystem :: SpecWith ()
spec_FileSystem = do
  describe "getDirectoryContentsRec" $ do
    it "gets the files inside a directory and all its subdirectories" $ do
      let action = FS.getDirectoryContentsRec (FilePath "dir1")
      let origState =
            Mock.mkFileSystemState
              [ Mock.Directory
                  (FilePath "dir1")
                  [ Mock.File (FilePath "file1") ""
                  , Mock.Directory
                      (FilePath "dir2")
                      [ Mock.File (FilePath "file2") ""
                      , Mock.Directory (FilePath "dir3") []
                      ]
                  ]
              ]
      let expected = [FilePath "dir1/file1", FilePath "dir1/dir2/file2"]
      case Sem.run . Sem.runError . Mock.runFileSystem origState $ action of
        Left err -> failSpec err
        Right result -> result `shouldBe` (origState, expected)
  describe "withCurrentDirectory" $ do
    it "changes the directory and resets it afterwards" $ do
      let action = do
            FS.withCurrentDirectory (FilePath "inside/subdir") $
              FS.writeFile (FilePath "insideDir") "insideDirContents"
            FS.writeFile (FilePath "outsideDir") "outsideDirContents"
      let origState =
            Mock.mkFileSystemState
              [ Mock.Directory
                  (FilePath "inside")
                  [ Mock.Directory
                      (FilePath "subdir")
                      [Mock.File (FilePath "bar") "barContents"]
                  , Mock.File (FilePath "foo") "fooContents"
                  ]
              ]
      let expected =
            origState &
            (Mock.fsRoot . at (FilePath "inside/subdir/insideDir") ?~
             Mock.File (FilePath "insideDir") "insideDirContents") &
            (Mock.fsRoot . at (FilePath "outsideDir") ?~
             Mock.File (FilePath "outsideDir") "outsideDirContents")
      case Sem.run . Sem.runError . Mock.runFileSystem origState $ action of
        Left err -> failSpec err
        Right result -> result `shouldBe` (expected, ())
  describe "withTemporaryDirectory" $ do
    it
      "creates a temporary directory and resets the current directory afterwards" $ do
      let action = do
            FS.withTemporaryDirectory $ \tempDir ->
              FS.writeFile
                (tempDir </> FilePath "insideTemp0")
                "insideTemp0Contents"
            FS.withTemporaryDirectory $ \tempDir ->
              FS.writeFile
                (tempDir </> FilePath "insideTemp1")
                "insideTemp1Contents"
      let origState = Mock.mkFileSystemState []
      let expected =
            origState &
            (Mock.fsRoot . at (FilePath "tmp") ?~
             Mock.Directory (FilePath "tmp") []) &
            (Mock.fsRoot . at (FilePath "tmp/0") ?~
             Mock.Directory
               (FilePath "0")
               [Mock.File (FilePath "insideTemp0") "insideTemp0Contents"]) &
            (Mock.fsRoot . at (FilePath "tmp/1") ?~
             Mock.Directory
               (FilePath "1")
               [Mock.File (FilePath "insideTemp1") "insideTemp1Contents"]) &
            (Mock.fsTempCounter .~ 2)
      case Sem.run . Sem.runError . Mock.runFileSystem origState $ action of
        Left err -> failSpec err
        Right result -> result `shouldBe` (expected, ())
