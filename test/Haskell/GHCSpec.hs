{-# LANGUAGE FlexibleContexts #-}

module Haskell.GHCSpec where

import Axel.Haskell.GHC as GHC
import Axel.Haskell.Stack as Stack

import Control.Lens

import qualified Monad.ProcessMock as Mock

import System.Exit

import Test.Tasty.Hspec

spec_GHC :: SpecWith ()
spec_GHC = do
  describe "ghcCompile" $ do
    it "compiles a file with GHC" $ do
      let action = GHC.ghcCompile "projectFoo/app/Main.hs"
      let origProcState =
            Mock.mkProcessState [] [(ExitSuccess, Just ("testStdout", ""))]
      let expectedPred (stdout, procState) =
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
      case Mock.runProcess origProcState action of
        Left err -> expectationFailure err
        Right result -> result `shouldSatisfy` expectedPred
  describe "ghcInterpret" $ do
    it "interprets a file with GHC" $ do
      let action = GHC.ghcInterpret "projectFoo/app/Main.hs"
      let origProcState =
            Mock.mkProcessState [] [(ExitSuccess, Just ("testStdout", ""))]
      let expectedPred (stdout, procState) =
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
      case Mock.runProcess origProcState action of
        Left err -> expectationFailure err
        Right result -> result `shouldSatisfy` expectedPred
