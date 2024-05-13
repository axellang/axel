module Axel.Haskell.File where

import Axel.Prelude

import Axel.AST
  ( SMStatement
  , Statement(SModuleDeclaration)
  , ToHaskell(toHaskell)
  , statementsToProgram
  )
import Axel.Eff ((:>>))
import Axel.Eff.Console (putStrLn)
import qualified Axel.Eff.Console as Effs (Console)
import Axel.Eff.Error (Error)
import qualified Axel.Eff.FileSystem as Effs (FileSystem)
import qualified Axel.Eff.FileSystem as FS (readFile, writeFile)
import qualified Axel.Eff.Ghci as Effs (Ghci)
import qualified Axel.Eff.Log as Effs (Log)
import qualified Axel.Eff.Process as Effs (Process)
import qualified Axel.Eff.Resource as Effs (Resource)
import qualified Axel.Eff.Restartable as Effs (Restartable)
import Axel.Haskell.Convert (convertFile)
import Axel.Macros (handleFunctionApplication, processProgram)
import Axel.Normalize (normalizeStatement, withExprCtxt)
import Axel.Parse (parseMultiple', parseSource)
import Axel.Parse.AST (Expression(Symbol))
import Axel.Pretty (prettifyProgram)
import qualified Axel.Sourcemap as SM
  ( Expression
  , Output
  , raw
  , unwrapCompoundExpressions
  )
import Axel.Sourcemap (ModuleInfo)
import Axel.Utils.FilePath (replaceExtension)
import Axel.Utils.Recursion (bottomUpFmap)

import Control.Lens (op)
import Control.Lens.Operators ((<&>), (?~))
import Control.Lens.Tuple (_2)
import Control.Monad (forM, unless, void)

import Data.Data (Data)
import qualified Data.Map as M (adjust, fromList, lookup)
import Data.Maybe (catMaybes)
import Data.Monoid (Alt(Alt))

import qualified Effectful as Eff
import qualified Effectful.Error.Static as Eff
import qualified Effectful.Reader.Static as Eff
import qualified Effectful.State.Static.Local as Eff

import qualified Language.Haskell.Ghcid as Ghci (Ghci)

convertList :: (Data ann) => Expression ann -> Expression ann
convertList =
  bottomUpFmap $ \case
    Symbol ann "List" -> Symbol ann "[]"
    x -> x

convertUnit :: (Data ann) => Expression ann -> Expression ann
convertUnit =
  bottomUpFmap $ \case
    Symbol ann "Unit" -> Symbol ann "()"
    Symbol ann "unit" -> Symbol ann "()"
    x -> x

readModuleInfo ::
     ('[ Eff.Error Error, Effs.FileSystem] :>> effs)
  => [FilePath]
  -> Eff.Eff effs ModuleInfo
readModuleInfo axelFiles = do
  modules <-
    forM axelFiles $ \filePath -> do
      source <- FS.readFile filePath
      exprs <-
        SM.unwrapCompoundExpressions <$> parseSource (Just filePath) source
      Alt moduleDecl <-
        mconcat . map Alt <$>
        mapM
          (\expr ->
             Eff.runErrorNoCallStack
               (Eff.runReader filePath $ withExprCtxt $ normalizeStatement expr) <&> \case
               Right (SModuleDeclaration _ moduleId) ->
                 Just (filePath, (moduleId, Nothing))
               _ -> Nothing)
          exprs
      pure moduleDecl
  pure $ M.fromList $ catMaybes modules

transpileSource ::
     forall effs fileExpanderEffs funAppExpanderEffs.
     ( fileExpanderEffs ~ '[ Effs.Console, Eff.Error Error, Effs.FileSystem, Effs.Ghci, Effs.Log, Effs.Process, Effs.Resource, Eff.Reader Ghci.Ghci, Eff.State ModuleInfo]
     , funAppExpanderEffs ~ (Eff.Reader FilePath ': Effs.Restartable SM.Expression ': Eff.State [SMStatement] ': fileExpanderEffs)
     , '[ Eff.Error Error, Effs.Ghci, Eff.Reader Ghci.Ghci, Eff.State ModuleInfo] :>> effs
     , fileExpanderEffs :>> effs
     )
  => FilePath
  -> Text
  -> Eff.Eff effs SM.Output
transpileSource filePath source =
  toHaskell . statementsToProgram <$>
  (parseSource (Just filePath) source >>=
   processProgram
     @fileExpanderEffs
     @funAppExpanderEffs
     handleFunctionApplication
     (void . transpileFileInPlace)
     filePath)

convertFileInPlace ::
     ('[ Effs.Console, Effs.FileSystem, Eff.Error Error, Effs.FileSystem] :>> effs)
  => FilePath
  -> Eff.Eff effs FilePath
convertFileInPlace path = do
  let newPath = replaceExtension path "axel"
  void $ convertFile path newPath
  pure newPath

transpileFile ::
     ('[ Effs.Console, Eff.Error Error, Effs.FileSystem, Effs.Ghci, Effs.Log, Effs.Process, Effs.Resource, Eff.Reader Ghci.Ghci, Eff.State ModuleInfo] :>> effs)
  => FilePath
  -> FilePath
  -> Eff.Eff effs ()
transpileFile path newPath = do
  fileContents <- FS.readFile path
  newContents <- transpileSource path fileContents
  putStrLn $ op FilePath path <> " => " <> op FilePath newPath
  FS.writeFile newPath (SM.raw newContents)
  Eff.modify $ M.adjust (_2 ?~ newContents) path

transpileFileInPlace ::
     ('[ Effs.Console, Eff.Error Error, Effs.FileSystem, Effs.Ghci, Effs.Log, Effs.Process, Effs.Resource, Eff.Reader Ghci.Ghci, Eff.State ModuleInfo] :>> effs)
  => FilePath
  -> Eff.Eff effs FilePath
transpileFileInPlace path = do
  moduleInfo <- Eff.gets $ M.lookup path
  let alreadyCompiled =
        case moduleInfo of
          Just (_, Just _) -> True
          _ -> False
  let newPath = replaceExtension path "hs"
  unless alreadyCompiled $ transpileFile path newPath
  pure newPath

formatFileInPlace ::
     ('[ Effs.Console, Effs.FileSystem, Eff.Error Error] :>> effs)
  => FilePath
  -> Eff.Eff effs ()
formatFileInPlace path = do
  contents <- FS.readFile path
  putStrLn $ "Formatting " <> op FilePath path <> "..."
  program <- parseMultiple' id (Just path) contents
  let prettifiedContents = prettifyProgram program
  FS.writeFile path prettifiedContents
