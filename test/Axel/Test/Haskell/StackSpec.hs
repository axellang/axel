{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Test.Haskell.StackSpec where

import Axel.Eff.FileSystem as FS
import Axel.Eff.Process
import Axel.Error
import Axel.Haskell.Stack as Stack
import Axel.Test.Monad.ConsoleMock as Mock
import Axel.Test.Monad.FileSystemMock as Mock
import Axel.Test.Monad.ProcessMock as Mock

import Control.Lens
import Control.Monad.Freer as Eff
import qualified Control.Monad.Freer.Error as Effs

import System.Exit

import Test.Tasty.Hspec

{-# ANN module "HLint: ignore Redundant do" #-}

spec_Stack :: SpecWith ()
spec_Stack = do
  describe "addStackDependency" $ do
    it "adds a Stackage dependency to a Stack project" $ do
      let action = Stack.addStackDependency "foo-1.2.3.4" "project/path"
      let origFSState =
            Mock.mkFileSystemState
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
      case Eff.run . Effs.runError . Mock.runFileSystem origFSState $ action of
        Left err -> expectationFailure err
        Right ((), result) -> result `shouldBe` expectedFSState
  describe "buildStackProject" $ do
    it "builds a Stack project" $ do
      let action = Stack.buildStackProject "project/foo"
      let origConsoleState = Mock.mkConsoleState
      let origFSState =
            Mock.mkFileSystemState
              [Mock.Directory "project" [Mock.Directory "foo" []]]
      let origProcState =
            Mock.mkProcessState
              []
              [ProcessResult ((ExitSuccess, Just ("", "")), pure ())]
      let expectation result (consoleState, fsState, procState) = do
            result `shouldBe` ()
            consoleState ^. Mock.consoleOutput `shouldBe` "Building foo...\n"
            procState ^. Mock.procExecutionLog `shouldBe`
              [("stack", ["build"], Just "")]
            fsState ^. Mock.fsCurrentDirectory `shouldBe` "/"
      case Eff.run . Effs.runError @Error . Effs.runError @String .
           Mock.runFileSystem origFSState .
           Mock.runProcess origProcState .
           Mock.runConsole origConsoleState $
           action of
        Left err -> expectationFailure $ show err
        Right (Left err) -> expectationFailure err
        Right (Right (((x, consoleState), procState), fsState)) ->
          expectation x (consoleState, fsState, procState)
  describe "createStackProject" $ do
    it "creates a new Stack project" $ do
      let action = Stack.createStackProject "newProject"
      let origFSState = Mock.mkFileSystemState []
      let origProcState =
            Mock.mkProcessState
              []
              [ ProcessResult
                  ( (ExitSuccess, Just ("", ""))
                  , FS.createDirectoryIfMissing False "newProject")
              , ProcessResult ((ExitSuccess, Just ("", "")), pure ())
              ]
      let expectation result (fsState, procState) = do
            result `shouldBe` ()
            procState ^. Mock.procExecutionLog `shouldBe`
              [ ("stack", ["new", "newProject", "new-template"], Just "")
              , ( "stack"
                , ["config", "set", "resolver", stackageResolverWithAxel]
                , Just "")
              ]
            fsState ^. Mock.fsCurrentDirectory `shouldBe` "/"
            fsState ^. Mock.fsRoot . at "newProject" . _Just . Mock.fsPath `shouldBe`
              "newProject"
      case Eff.run . Effs.runError @Error . Effs.runError @String .
           Mock.runFileSystem origFSState .
           Mock.runProcess origProcState $
           action of
        Left err -> expectationFailure $ show err
        Right (Left err) -> expectationFailure err
        Right (Right ((x, procState), fsState)) ->
          expectation x (fsState, procState)
  describe "runStackProject" $ do
    it "runs a Stack project" $ do
      let action = Stack.runStackProject "project/foo"
      let origConsoleState = Mock.mkConsoleState
      let origFSState =
            Mock.mkFileSystemState
              [Mock.Directory "project" [Mock.Directory "foo" []]]
      let origProcState =
            Mock.mkProcessState
              []
              [ ProcessResult
                  ( ( ExitSuccess
                    , Just ("", "foo:lib\nfoo:exe:foo-exe\nfoo:test:foo-test"))
                  , pure ())
              , ProcessResult ((ExitSuccess, Nothing), pure ())
              ]
      let expectation result (consoleState, fsState, procState) = do
            result `shouldBe` ()
            procState ^. Mock.procExecutionLog `shouldBe`
              [ ("stack", ["ide", "targets"], Just "")
              , ("stack", ["exec", "foo-exe"], Nothing)
              ]
            fsState ^. Mock.fsCurrentDirectory `shouldBe` "/"
            consoleState ^. Mock.consoleOutput `shouldBe` "Running foo-exe...\n"
      case Eff.run . Effs.runError @Error . Effs.runError @String .
           Mock.runFileSystem origFSState .
           Mock.runProcess origProcState .
           Mock.runConsole origConsoleState $
           action of
        Left err -> expectationFailure $ show err
        Right (Left err) -> expectationFailure err
        Right (Right (((x, consoleState), procState), fsState)) ->
          expectation x (consoleState, fsState, procState)
  describe "compileFile" $ do
    it "compiles a file with GHC" $ do
      let action = Stack.compileFile @'CreateStreams "projectFoo/app/Main.hs" ""
      let origFSState = Mock.mkFileSystemState []
      let origProcState =
            Mock.mkProcessState
              []
              [ProcessResult ((ExitSuccess, Just ("testStdout", "")), pure ())]
      let expectation stdout (procState, _) = do
            stdout `shouldBe` "testStdout"
            procState ^. Mock.procExecutionLog `shouldBe`
              [ ( "stack"
                , [ "ghc"
                  , "--resolver"
                  , Stack.stackageResolverWithAxel
                  , "--package"
                  , Stack.axelStackageId
                  , "--"
                  , "projectFoo/app/Main.hs"
                  ]
                , Just "")
              ]
      case Eff.run . Effs.runError @String . Mock.runFileSystem origFSState .
           Mock.runProcess origProcState $
           action of
        Left err -> expectationFailure err
        Right (((_, stdout, _), procState), fsState) ->
          expectation stdout (procState, fsState)
  describe "interpretFile" $ do
    it "interprets a file with GHC" $ do
      let action =
            Stack.interpretFile @'CreateStreams "projectFoo/app/Main.hs" ""
      let origFSState = Mock.mkFileSystemState []
      let origProcState =
            Mock.mkProcessState
              []
              [ProcessResult ((ExitSuccess, Just ("testStdout", "")), pure ())]
      let expectation stdout (procState, _) = do
            stdout `shouldBe` "testStdout"
            procState ^. Mock.procExecutionLog `shouldBe`
              [ ( "stack"
                , [ "runghc"
                  , "--resolver"
                  , Stack.stackageResolverWithAxel
                  , "--package"
                  , Stack.axelStackageId
                  , "--"
                  , "projectFoo/app/Main.hs"
                  ]
                , Just "")
              ]
      case Eff.run . Effs.runError @String . Mock.runFileSystem origFSState .
           Mock.runProcess origProcState $
           action of
        Left err -> expectationFailure err
        Right (((_, stdout, _), procState), fsState) ->
          expectation stdout (procState, fsState)
