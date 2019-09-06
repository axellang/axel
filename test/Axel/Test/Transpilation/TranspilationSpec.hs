{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Test.Transpilation.TranspilationSpec where

import Axel.Eff.App (AppEffs)
import qualified Axel.Eff.Console as Effs
import qualified Axel.Eff.Error as Effs
import qualified Axel.Eff.FileSystem as Effs
import qualified Axel.Eff.Ghci as Effs
import qualified Axel.Eff.Log as Effs
import qualified Axel.Eff.Process as Effs
import qualified Axel.Eff.Resource as Effs
import Axel.Haskell.File
import Axel.Sourcemap as SM

import qualified Polysemy as Sem
import qualified Polysemy.State as Sem

import Data.ByteString.Lazy.Char8 as C hiding (readFile)

import qualified Data.Map as M

import System.FilePath

import Test.Tasty
import Test.Tasty.Golden

runApp :: Sem.Sem AppEffs a -> IO a
runApp =
  Sem.runM .
  Effs.runResource .
  Effs.runProcess .
  Effs.runGhci .
  Effs.runFileSystem .
  Effs.unsafeRunError @Effs.Error . Effs.runConsole . Effs.ignoreLog

test_transpilation_golden :: IO TestTree
test_transpilation_golden = do
  axelFiles <- findByExtension [".axel_golden"] "test/Axel/Test/Transpilation"
  pure $
    testGroup "transpilation golden tests" $ do
      axelFile <- axelFiles
      let hsFile = replaceExtension axelFile ".hs_golden"
      let transpiled = do
            axelSource <- readFile axelFile
            output <-
              runApp $
              Sem.evalState (M.empty :: ModuleInfo) $
              transpileSource (takeBaseName axelFile) axelSource
            let newSource = C.pack $ SM.raw output
            pure newSource
      pure $ goldenVsString (takeBaseName axelFile) hsFile transpiled
