{-# LANGUAGE FlexibleContexts #-}

module Haskell.StackSpec where

import Axel.Haskell.Stack as Stack

import Control.Lens
import Control.Monad.Except

import qualified Monad.FileSystemMock as Mock
import qualified Monad.ProcessMock as Mock

import System.Exit

import Test.Tasty.Hspec

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
      let origProcState = Mock.mkProcessState [] [(ExitSuccess, Just ("", ""))]
      let expectedPred ((), (procState, fsState)) =
            (procState ^. Mock.procExecutionLog ==
             [("stack", ["build"], Just "")]) &&
            (fsState ^. Mock.fsCurrentDirectory == "/")
      case Mock.runFileSystem origFSState $ runExceptT $
           Mock.runProcessT origProcState action of
        Left err -> expectationFailure err
        Right (x, fsState) ->
          case x of
            Left err -> expectationFailure err
            Right (x', procState) ->
              (x', (procState, fsState)) `shouldSatisfy` expectedPred
