{- HLINT ignore "Redundant do" -}
{-# LANGUAGE PartialTypeSignatures #-}

module Axel.Test.Haskell.CabalSpec where

import Axel.Prelude

import Test.Hspec

-- TODO Flesh out `ProcessMock.runProcess` with the new `Process` actions,
--      so that we can re-enable this test suite.
spec_Cabal :: Spec
spec_Cabal = pure ()
{-

import Axel.Eff.Error
import Axel.Eff.FileSystem as FS
import Axel.Eff.Process
import Axel.Haskell.Cabal as Cabal
import Axel.Test.Eff.ConsoleMock as Mock
import Axel.Test.Eff.FileSystemMock as Mock
import Axel.Test.Eff.ProcessMock as Mock

import Control.Lens

import Data.Functor
import qualified Data.Map as M

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem

import System.Exit

import TestUtils

spec_Cabal :: Spec
spec_Cabal = do
  describe "addDependency" $ do
    it "adds a package dependency to a project" $ do
      let action =
            Cabal.addDependency "foo-1.2.3.4" (FilePath "project/path")
      let origFSState =
            Mock.mkFileSystemState
              [ Mock.Directory
                  (FilePath "project")
                  [ Mock.Directory
                      (FilePath "path")
                      [ Mock.File
                          (FilePath "package.yaml")
                          "dependencies:\n- asdf-5.6.7"
                      ]
                  ]
              ]
      let expectedFSState =
            origFSState & Mock.fsRoot .
            at (FilePath "project/path/package.yaml") ?~
            Mock.File
              (FilePath "package.yaml")
              "dependencies:\n- foo-1.2.3.4\n- asdf-5.6.7\n"
      case Sem.run . Eff.runErrorNoCallStack . Mock.runFileSystem origFSState $ action of
        Left err -> failSpec err
        Right (result, ()) -> result `shouldBe` expectedFSState
  describe "buildProject" $ do
    it "builds a project" $ do
      let action = Cabal.buildProject M.empty (FilePath "project/foo")
      let origConsoleState = Mock.mkConsoleState
      let origFSState =
            Mock.mkFileSystemState
              [ Mock.Directory
                  (FilePath "project")
                  [Mock.Directory (FilePath "foo") []]
              ]
      let origProcState =
            Mock.mkProcessState
              []
              [ProcessResult ((ExitSuccess, Just ("", "")), pure ())]
      let expectation result (consoleState, fsState, procState) = do
            result `shouldBe` ()
            consoleState ^. Mock.consoleOutput `shouldBe` "Building foo...\n"
            procState ^. Mock.procExecutionLog `shouldBe`
              [("stack build --ghc-options='-ddump-json'", Just "")]
            fsState ^. Mock.fsCurrentDirectory `shouldBe` FilePath "/"
      case Sem.run . Eff.runErrorNoCallStack @Error . Eff.runErrorNoCallStack @Text .
           Mock.runFileSystem origFSState .
           Mock.runProcess origProcState .
           Mock.runConsole origConsoleState $
           action of
        Left err -> failSpec $ renderError err
        Right (Left err) -> failSpec err
        Right (Right (fsState, (procState, (consoleState, x)))) ->
          expectation x (consoleState, fsState, procState)
  describe "createProject" $ do
    it "creates a new project" $ do
      let action = Cabal.createProject "newProject"
      let origFSState = Mock.mkFileSystemState []
      let origProcState =
            Mock.mkProcessState
              []
              [ ProcessResult
                  ( (ExitSuccess, Just ("", ""))
                  , FS.createDirectoryIfMissing False (FilePath "newProject"))
              , ProcessResult ((ExitSuccess, Just ("", "")), pure ())
              ]
      let expectation result (fsState, procState) = do
            result `shouldBe` ()
            procState ^. Mock.procExecutionLog `shouldBe`
              [ ("stack new newProject new-template", Just "")
              , ( "stack config set resolver " <> stackageResolverWithAxel
                , Just "")
              ]
            fsState ^. Mock.fsCurrentDirectory `shouldBe` FilePath "/"
            fsState ^. Mock.fsRoot . at (FilePath "newProject") . _Just .
              Mock.fsPath `shouldBe`
              FilePath "newProject"
      case Sem.run . Eff.runErrorNoCallStack @Error . Eff.runErrorNoCallStack @Text .
           Mock.runFileSystem origFSState .
           Mock.runProcess origProcState $
           action of
        Left err -> failSpec $ renderError err
        Right (Left err) -> failSpec err
        Right (Right (fsState, (procState, x))) ->
          expectation x (fsState, procState)
  describe "runProject" $ do
    it "runs a project" $ do
      let action = Cabal.runProject (FilePath "project/foo")
      let origConsoleState = Mock.mkConsoleState
      let origFSState =
            Mock.mkFileSystemState
              [ Mock.Directory
                  (FilePath "project")
                  [Mock.Directory (FilePath "foo") []]
              ]
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
              [("stack ide targets", Just ""), ("stack exec foo-exe", Nothing)]
            fsState ^. Mock.fsCurrentDirectory `shouldBe` FilePath "/"
            consoleState ^. Mock.consoleOutput `shouldBe` "Running foo-exe...\n"
      case Sem.run . Eff.runErrorNoCallStack @Error . Eff.runErrorNoCallStack @Text .
           Mock.runFileSystem origFSState .
           Mock.runProcess origProcState .
           Mock.runConsole origConsoleState $
           action of
        Left err -> failSpec $ renderError err
        Right (Left err) -> failSpec err
        Right (Right (fsState, (procState, (consoleState, x)))) ->
          expectation x (consoleState, fsState, procState)
-}
