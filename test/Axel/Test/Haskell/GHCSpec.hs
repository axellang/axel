{-# LANGUAGE FlexibleContexts #-}

module Axel.Test.Haskell.GHCSpec where

import Axel.Haskell.GHC as GHC
import Axel.Haskell.Stack as Stack
import qualified Axel.Test.Monad.FileSystemMock as Mock
import qualified Axel.Test.Monad.ProcessMock as Mock

import Control.Lens
import Control.Monad.Except

import System.Exit

import Test.Tasty.Hspec

{-# ANN module "HLint: ignore Redundant do" #-}

spec_GHC :: SpecWith ()
spec_GHC = do
  describe "ghcCompile" $ do
    it "compiles a file with GHC" $ do
      let action = GHC.ghcCompile "projectFoo/app/Main.hs"
      let origFSState = Mock.mkFSState []
      let origProcState =
            Mock.mkProcessState
              []
              [ Mock.ProcessResultT
                  ((ExitSuccess, Just ("testStdout", "")), pure ())
              ]
      let expectedPred (stdout, (procState, _)) =
            stdout == "testStdout" &&
            (procState ^. Mock.procExecutionLog ==
             [ ( "stack"
               , [ "--resolver"
                 , Stack.stackageResolverWithAxel
                 , "ghc"
                 , "--"
                 , "-v0"
                 , "-ddump-json"
                 , "projectFoo/app/Main.hs"
                 ]
               , Just "")
             ])
      case Mock.runProcess (origProcState, origFSState) $ runExceptT action of
        Left err -> expectationFailure err
        Right (Left err, _) -> expectationFailure err
        Right (Right x, state) -> (x, state) `shouldSatisfy` expectedPred
  describe "ghcInterpret" $ do
    it "interprets a file with GHC" $ do
      let action = GHC.ghcInterpret "projectFoo/app/Main.hs"
      let origFSState = Mock.mkFSState []
      let origProcState =
            Mock.mkProcessState
              []
              [ Mock.ProcessResultT
                  ((ExitSuccess, Just ("testStdout", "")), pure ())
              ]
      let expectedPred (stdout, (procState, _)) =
            stdout == "testStdout" &&
            (procState ^. Mock.procExecutionLog ==
             [ ( "stack"
               , [ "--resolver"
                 , stackageResolverWithAxel
                 , "runghc"
                 , "--package"
                 , axelStackageId
                 , "--"
                 , "projectFoo/app/Main.hs"
                 ]
               , Just "")
             ])
      case Mock.runProcess (origProcState, origFSState) $ runExceptT action of
        Left err -> expectationFailure err
        Right (Left err, _) -> expectationFailure err
        Right (Right x, state) -> (x, state) `shouldSatisfy` expectedPred
