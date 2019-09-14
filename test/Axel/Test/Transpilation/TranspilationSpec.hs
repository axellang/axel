module Axel.Test.Transpilation.TranspilationSpec where

import Axel.Prelude

import Axel.Eff.App (AppEffs)
import qualified Axel.Eff.Console as Effs
import qualified Axel.Eff.Error as Effs
import qualified Axel.Eff.FileSystem as Effs
import qualified Axel.Eff.Ghci as Effs
import qualified Axel.Eff.Ghci as Ghci
import qualified Axel.Eff.Log as Effs
import qualified Axel.Eff.Process as Effs
import qualified Axel.Eff.Random as Effs
import qualified Axel.Eff.Resource as Effs
import qualified Axel.Eff.Time as Effs
import Axel.Haskell.File
import Axel.Sourcemap as SM
import Axel.Utils.FilePath
import Axel.Utils.Text

import Control.Lens

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Polysemy as Sem
import qualified Polysemy.State as Sem

import Test.Tasty
import Test.Tasty.Golden

runApp :: Sem.Sem AppEffs a -> IO a
runApp =
  Sem.runM .
  Effs.runTime .
  Effs.runRandom .
  Effs.runResource .
  Effs.runProcess .
  Effs.runGhci .
  Effs.runFileSystem .
  Effs.unsafeRunError Effs.renderError . Effs.runConsole . Effs.ignoreLog

test_transpilation_golden :: IO TestTree
test_transpilation_golden = do
  axelFiles <-
    map (FilePath . T.pack) <$>
    findByExtension [".axel_golden"] "test/Axel/Test/Transpilation"
  pure $
    testGroup "transpilation golden tests" $ do
      axelFile <- axelFiles
      let hsFile = replaceExtension axelFile "hs_golden"
      let transpiled = do
            axelSource <- T.readFile $ T.unpack (op FilePath axelFile)
            output <-
              runApp $
              Sem.evalState (M.empty :: ModuleInfo) $
              Ghci.withGhci $ transpileSource (takeBaseName axelFile) axelSource
            let newSource = encodeUtf8Lazy $ SM.raw output
            pure newSource
      pure $
        goldenVsString
          (T.unpack . op FilePath $ takeBaseName axelFile)
          (T.unpack . op FilePath $ hsFile)
          transpiled
