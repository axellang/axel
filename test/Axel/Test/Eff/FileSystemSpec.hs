module Axel.Test.Eff.FileSystemSpec where

import qualified Axel.Eff.FileSystem as FS
import qualified Axel.Test.Eff.FileSystemMock as Mock

import Control.Lens

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem

import System.FilePath

import Test.Tasty.Hspec

{-# ANN module "HLint: ignore Redundant do" #-}

spec_FileSystem :: SpecWith ()
spec_FileSystem = do
  describe "getDirectoryContentsRec" $ do
    it "gets the files inside a directory and all its subdirectories" $ do
      let action = FS.getDirectoryContentsRec "dir1"
      let origState =
            Mock.mkFileSystemState
              [ Mock.Directory
                  "dir1"
                  [ Mock.File "file1" ""
                  , Mock.Directory
                      "dir2"
                      [Mock.File "file2" "", Mock.Directory "dir3" []]
                  ]
              ]
      let expected = ["dir1/file1", "dir1/dir2/file2"]
      case Sem.run . Sem.runError . Mock.runFileSystem origState $ action of
        Left err -> expectationFailure err
        Right result -> result `shouldBe` (origState, expected)
  describe "withCurrentDirectory" $ do
    it "changes the directory and resets it afterwards" $ do
      let action = do
            FS.withCurrentDirectory "inside/subdir" $
              FS.writeFile "insideDir" "insideDirContents"
            FS.writeFile "outsideDir" "outsideDirContents"
      let origState =
            Mock.mkFileSystemState
              [ Mock.Directory
                  "inside"
                  [ Mock.Directory "subdir" [Mock.File "bar" "barContents"]
                  , Mock.File "foo" "fooContents"
                  ]
              ]
      let expected =
            origState &
            (Mock.fsRoot . at "inside/subdir/insideDir" ?~
             Mock.File "insideDir" "insideDirContents") &
            (Mock.fsRoot . at "outsideDir" ?~
             Mock.File "outsideDir" "outsideDirContents")
      case Sem.run . Sem.runError . Mock.runFileSystem origState $ action of
        Left err -> expectationFailure err
        Right result -> result `shouldBe` (expected, ())
  describe "withTemporaryDirectory" $ do
    it
      "creates a temporary directory and resets the current directory afterwards" $ do
      let action = do
            FS.withTemporaryDirectory $ \tempDir ->
              FS.writeFile (tempDir </> "insideTemp0") "insideTemp0Contents"
            FS.withTemporaryDirectory $ \tempDir ->
              FS.writeFile (tempDir </> "insideTemp1") "insideTemp1Contents"
      let origState = Mock.mkFileSystemState []
      let expected =
            origState & (Mock.fsRoot . at "tmp" ?~ Mock.Directory "tmp" []) &
            (Mock.fsRoot . at "tmp/0" ?~
             Mock.Directory "0" [Mock.File "insideTemp0" "insideTemp0Contents"]) &
            (Mock.fsRoot . at "tmp/1" ?~
             Mock.Directory "1" [Mock.File "insideTemp1" "insideTemp1Contents"]) &
            (Mock.fsTempCounter .~ 2)
      case Sem.run . Sem.runError . Mock.runFileSystem origState $ action of
        Left err -> expectationFailure err
        Right result -> result `shouldBe` (expected, ())
