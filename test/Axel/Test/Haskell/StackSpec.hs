{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Axel.Test.Haskell.StackSpec where

import Axel.Haskell.Stack as Stack
import Axel.Monad.FileSystem as FS
import Axel.Test.Monad.FileSystemMock as Mock
import Axel.Test.Monad.ProcessMock as Mock

import Control.Lens

import System.Exit

import Test.Tasty.Hspec

{-# ANN module "HLint: ignore Redundant do" #-}

spec_Stack :: SpecWith ()
spec_Stack = do
  describe "addStackDependency" $ do
    it "adds a Stackage dependency to a Stack project" $ do
      let action = Stack.addStackDependency "foo-1.2.3.4" "project/path"
      let origFSState =
            Mock.mkFSState
              [ Mock.Directory
                  "project"
                  [ Mock.Directory
                      "path"
                      [Mock.File "package.yaml" "dependencies:\n- asdf-5.6.7"]
                  ]
              ]
      let expectedFSState =
            origFSState & Mock.fsRoot . at "project/path/package.yaml" ?~
            Mock.File
              "package.yaml"
              "dependencies:\n- foo-1.2.3.4\n- asdf-5.6.7\n"
      case Mock.runFileSystem origFSState action of
        Left err -> expectationFailure err
        Right ((), result) -> result `shouldBe` expectedFSState
  describe "buildStackProject" $ do
    it "builds a Stack project" $ do
      let action = Stack.buildStackProject "project/path"
      let origFSState =
            Mock.mkFSState [Mock.Directory "project" [Mock.Directory "path" []]]
      let origProcState =
            Mock.mkProcessState
              []
              [ProcessResultT ((ExitSuccess, Just ("", "")), pure ())]
      let expectation ((), (procState, fsState)) = do
            procState ^. Mock.procExecutionLog `shouldBe`
              [("stack", ["build"], Just "")]
            fsState ^. Mock.fsCurrentDirectory `shouldBe` "/"
      case Mock.runProcess (origProcState, origFSState) action of
        Left err -> expectationFailure err
        Right result -> expectation result
  describe "createStackProject" $ do
    it "creates a new Stack project" $ do
      let action = Stack.createStackProject "newProject"
      let origFSState = Mock.mkFSState []
      let origProcState =
            Mock.mkProcessState
              []
              [ ProcessResultT
                  ( (ExitSuccess, Just ("", ""))
                  , FS.createDirectoryIfMissing False "newProject")
              , ProcessResultT ((ExitSuccess, Just ("", "")), pure ())
              ]
      let expectation ((), (procState, fsState)) = do
            procState ^. Mock.procExecutionLog `shouldBe`
              [ ("stack", ["new", "newProject", "new-template"], Just "")
              , ( "stack"
                , ["config", "set", "resolver", stackageResolverWithAxel]
                , Just "")
              ]
            fsState ^. Mock.fsCurrentDirectory `shouldBe` "/"
            fsState ^. Mock.fsRoot . at "newProject" . _Just . Mock.fsPath `shouldBe`
              "newProject"
      case Mock.runProcess (origProcState, origFSState) action of
        Left err -> expectationFailure err
        Right result -> expectation result
