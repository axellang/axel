{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Monad.Haskell.Stack where

import Axel.Monad.FileSystem
import Axel.Monad.Haskell.Stack
import Axel.Monad.Output
import Axel.Monad.Process

import Control.Exception (evaluate)
import Control.Monad.Mock
import Control.Monad.Mock.TH

import Data.Function ((&))

import Test.Hspec

makeAction "FileSystemAction" [ts| MonadFileSystem |]
makeAction "OutputAction" [ts| MonadOutput |]
makeAction "ProcessAction" [ts| MonadProcess |]

spec =
  describe "addStackDependency" $
  it "adds a Stackage dependency to the current Stack project" $
  let dependency = "foo-1.2.3.4"
      oldContents = "dependencies:\n- other-dep-0.0.1"
      newContents = "dependencies:\n- other-dep-0.0.1\n- foo-1.2.3.4"
      projectPath = "path/to/project"
   in addStackDependency dependency projectPath `shouldBe`
