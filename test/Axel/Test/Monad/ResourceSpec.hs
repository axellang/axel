{-# LANGUAGE FlexibleContexts #-}

module Axel.Test.Monad.ResourceSpec where

import qualified Axel.Monad.Resource as Res
import qualified Axel.Test.Monad.FileSystemMock as Mock
import qualified Axel.Test.Monad.ResourceMock as Mock

import Test.Tasty.Hspec

{-# ANN module "HLint: ignore Redundant do" #-}

spec_Resource :: SpecWith ()
spec_Resource =
  describe "readResource" $ do
    it "reads a resource's contents from the file system" $ do
      let action = Res.readResource (Res.ResourceId "resGroup1/res1")
      let origFSState =
            Mock.mkFSState
              [ Mock.Directory
                  "resources"
                  [Mock.Directory "resGroup1" [Mock.File "res1" "res1Contents"]]
              ]
      let expected = "res1Contents"
      case Mock.runFileSystem origFSState $ Mock.runResourceT action of
        Left err -> expectationFailure err
        Right result -> result `shouldBe` (expected, origFSState)
