{-# LANGUAGE FlexibleContexts #-}

module Monad.ResourceSpec where

import qualified Axel.Monad.Resource as Res

import qualified Monad.FileSystemMock as Mock
import qualified Monad.ResourceMock as Mock

import Test.Tasty.Hspec

spec_Resource :: SpecWith ()
spec_Resource =
  describe "Resource" $ do
    describe "readResource" $ do
      it "reads a resource's contents from the file system" $ do
        let action =
              Res.readResource (Res.ResourceId "resGroup1/res1") :: Mock.ResourceT Mock.FileSystem FilePath
            expected = "res1Contents"
            origFSState =
              Mock.mkFSState
                [ Mock.Directory
                    "resources"
                    [ Mock.Directory
                        "resGroup1"
                        [Mock.File "res1" "res1Contents"]
                    ]
                ]
        case Mock.runFileSystem origFSState $ Mock.runResourceT action of
          Left err -> error err
          Right result -> result `shouldBe` (expected, origFSState)
