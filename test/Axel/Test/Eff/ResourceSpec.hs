{- HLINT ignore "Redundant do" -}
module Axel.Test.Eff.ResourceSpec where

import Axel.Prelude

import Axel.Eff.Resource as Res
import Axel.Test.Eff.FileSystemMock as Mock
import Axel.Test.Eff.ResourceMock as Mock

import qualified Effectful as Eff
import qualified Effectful.Error.Static as Eff

import Test.Hspec

import TestUtils

spec_Resource :: Spec
spec_Resource =
  describe "readResource" $ do
    it "reads a resource's contents from the file system" $ do
      let action = Res.readResource (Res.ResourceId "resGroup1/res1")
      let origFSState =
            Mock.mkFileSystemState
              [ Mock.Directory
                  (FilePath "dataFiles")
                  [ Mock.Directory
                      (FilePath "resources")
                      [ Mock.Directory
                          (FilePath "resGroup1")
                          [Mock.File (FilePath "res1") "res1Contents"]
                      ]
                  ]
              ]
      let expected = "res1Contents"
      case Eff.runPureEff .
           Eff.runErrorNoCallStack .
           Mock.runFileSystem origFSState . Mock.runResource $
           action of
        Left err -> failSpec err
        Right result -> result `shouldBe` (expected, origFSState)
