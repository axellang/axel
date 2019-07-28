{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Axel.Macros where

import Prelude hiding (putStrLn)

import Axel.AST
  ( Identifier
  , MacroDefinition
  , MacroImport
  , Statement(SMacroDefinition, SMacroImport, STypeSignature)
  , ToHaskell(toHaskell)
  , TypeSignature
  , _SModuleDeclaration
  , _STypeSignature
  , functionDefinition
  , imports
  , moduleName
  , name
  )
import Axel.Denormalize (denormalizeStatement)
import qualified Axel.Eff.FileSystem as Effs (FileSystem)
import qualified Axel.Eff.FileSystem as FS (removeFile, writeFile)
import qualified Axel.Eff.Ghci as Effs (Ghci)
import qualified Axel.Eff.Ghci as Ghci (enableJsonErrors, exec, start, stop)
import Axel.Eff.Lens ((%=), use, view)
import qualified Axel.Eff.Process as Effs (Process)
import Axel.Eff.Resource (readResource)
import qualified Axel.Eff.Resource as Effs (Resource)
import qualified Axel.Eff.Resource as Res
  ( astDefinition
  , macroDefinitionAndEnvironmentFooter
  , macroDefinitionAndEnvironmentHeader
  , macroScaffold
  )
import Axel.Error (Error(MacroError), fatal, unsafeIgnoreError)
import Axel.Haskell.Error (processErrors)
import Axel.Haskell.Macros (hygenisizeMacroName)
import Axel.Haskell.Prettify (prettifyHaskell)
import Axel.Normalize (normalizeStatement, withExprCtxt)
import qualified Axel.Parse as Parse
  ( Expression(SExpression, Symbol)
  , parseMultiple
  , programToTopLevelExpressions
  , topLevelExpressionsToProgram
  )
import Axel.Sourcemap (Delimiter(Newlines))
import qualified Axel.Sourcemap as SM (Error, Expression, Output, raw)
import Axel.Utils.Display (delimit)
import Axel.Utils.Function (uncurry3)
import Axel.Utils.List (filterMap, head')
import Axel.Utils.Monad (concatMapM)
import Axel.Utils.Recursion (Recursive(bottomUpTraverse), exhaustM)
import Axel.Utils.String (replace)

import Control.Lens.Cons (snoc)
import Control.Lens.Extras (is)
import Control.Lens.Operators ((%~), (<&>), (^.), (^?!))
import Control.Lens.TH (makeFieldsNoPrefix)
import Control.Lens.Tuple (_2)
import Control.Monad ((>=>), forM_, when)
import Control.Monad.Freer (Eff, Member, Members)
import Control.Monad.Freer.Error (throwError)
import qualified Control.Monad.Freer.Error as Effs (Error)
import Control.Monad.Freer.Reader (ask)
import qualified Control.Monad.Freer.Reader as Effs (Reader, runReader)
import Control.Monad.Freer.State (gets)
import qualified Control.Monad.Freer.State as Effs (State, evalState)

import Data.Function ((&))
import Data.List (nub)
import Data.List.Split (splitWhen)
import Data.Map (Map)
import qualified Data.Map as M (elems, empty, filter, fromList, toList)
import Data.Maybe (fromJust, isNothing, mapMaybe)
import Data.Semigroup ((<>))

import qualified Language.Haskell.Ghcid as Ghci (Ghci)

import System.FilePath ((<.>))

type ModuleInfo = Map Identifier (FilePath, Maybe SM.Output)

getTranspiledFiles :: ModuleInfo -> Map FilePath SM.Output
getTranspiledFiles =
  M.fromList .
  filterMap
    (\case
       (_, Nothing) -> Nothing
       (filePath, Just output) -> Just (filePath, output)) .
  M.elems

data MacroExpansionEnv =
  MacroExpansionEnv
    { _stmts :: [Statement SM.Expression]
    , _macroDefs :: [MacroDefinition SM.Expression]
    }

makeFieldsNoPrefix ''MacroExpansionEnv

type FileExpander effs
   = forall openEffs. (Members effs openEffs) =>
                        FilePath -> Eff openEffs ()

expandMacros ::
     forall fileExpanderEffs effs.
     ( Members '[ Effs.Error SM.Error, Effs.FileSystem, Effs.Ghci, Effs.Process, Effs.Reader ( Ghci.Ghci
                                                                                             , FilePath), Effs.Resource, Effs.State ModuleInfo] effs
     , Members fileExpanderEffs (Effs.State MacroExpansionEnv ': effs)
     )
  => FileExpander fileExpanderEffs
  -> [SM.Expression]
  -> Eff effs [Statement SM.Expression]
expandMacros expandFile origTopLevelExprs =
  Effs.evalState (MacroExpansionEnv [] []) $ do
    mapM_ (expandProgramExpr @fileExpanderEffs expandFile) origTopLevelExprs
    hygenicMacroDefs <-
      map (SMacroDefinition . hygenisizeMacroDefinition) <$>
      use @MacroExpansionEnv macroDefs
    (<> hygenicMacroDefs) <$> use @MacroExpansionEnv stmts

isMacroImported ::
     (Member (Effs.State MacroExpansionEnv) effs) => Identifier -> Eff effs Bool
isMacroImported macroName =
  any
    (\case
       SMacroImport macroImport -> macroName `elem` macroImport ^. imports
       _ -> False) <$>
  use @MacroExpansionEnv stmts

ensureCompiledDependency ::
     forall fileExpanderEffs effs.
     (Member (Effs.State ModuleInfo) effs, Members fileExpanderEffs effs)
  => FileExpander fileExpanderEffs
  -> MacroImport SM.Expression
  -> Eff effs ()
ensureCompiledDependency expandFile macroImport = do
  moduleInfo <-
    gets
      @ModuleInfo
      (M.filter (\(moduleId', _) -> moduleId' == macroImport ^. moduleName))
  case head' $ M.toList moduleInfo of
    Just (dependencyFilePath, (_, transpiledOutput)) ->
      when (isNothing transpiledOutput) $ expandFile dependencyFilePath
    Nothing -> pure ()

-- | Fully expand an expression tree.
expandProgramExpr ::
     forall fileExpanderEffs effs.
     ( Members '[ Effs.Error SM.Error, Effs.FileSystem, Effs.Ghci, Effs.Process, Effs.Resource, Effs.State MacroExpansionEnv, Effs.State ModuleInfo, Effs.Reader ( Ghci.Ghci
                                                                                                                                                                 , FilePath)] effs
     , Members fileExpanderEffs effs
     )
  => FileExpander fileExpanderEffs
  -> SM.Expression
  -> Eff effs [SM.Expression]
expandProgramExpr expandFile expr = do
  let program = Parse.topLevelExpressionsToProgram [expr]
  newTopLevelExprs <-
    Parse.programToTopLevelExpressions <$>
    bottomUpTraverse
      (\case
         Parse.SExpression ann' xs ->
           Parse.SExpression ann' <$>
           handleFunctionApplication @fileExpanderEffs expandFile xs
         x -> pure x)
      program
  filePath <- view @(Ghci.Ghci, FilePath) _2
  forM_ newTopLevelExprs $ Effs.runReader filePath . withExprCtxt .
    normalizeStatement >=> \case
    SMacroDefinition macroDef ->
      (%=) @MacroExpansionEnv macroDefs (`snoc` macroDef)
    newStmt -> do
      (%=) @MacroExpansionEnv stmts (`snoc` newStmt)
      case newStmt of
        SMacroImport macroImport ->
          ensureCompiledDependency @fileExpanderEffs expandFile macroImport
        _ -> pure ()
  pure newTopLevelExprs

-- | If a function application is a macro call, expand it.
handleFunctionApplication ::
     forall fileExpanderEffs effs.
     ( Members '[ Effs.Error SM.Error, Effs.FileSystem, Effs.Ghci, Effs.Process, Effs.Resource, Effs.State MacroExpansionEnv, Effs.State ModuleInfo, Effs.Reader ( Ghci.Ghci
                                                                                                                                                                 , FilePath)] effs
     , Members fileExpanderEffs effs
     )
  => FileExpander fileExpanderEffs
  -> [SM.Expression]
  -> Eff effs [SM.Expression]
handleFunctionApplication expandFile =
  concatMapM
    (\x ->
       case x of
         Parse.SExpression _ (Parse.Symbol _ function:args) -> do
           isImported <- isMacroImported function
           maybeMacroDefs <-
             if isImported
               then pure $ Just []
               else lookupMacroDefinitions function <&> \case
                      [] -> Nothing
                      macroDefs' -> Just macroDefs'
           case maybeMacroDefs of
             Just _ -> do
               newExprs <- expandMacroApplication function args
               concatMapM
                 (expandProgramExpr @fileExpanderEffs expandFile)
                 newExprs
             Nothing -> pure [x]
         _ -> pure [x])

lookupMacroDefinitions ::
     (Member (Effs.State MacroExpansionEnv) effs)
  => Identifier
  -> Eff effs [MacroDefinition SM.Expression]
lookupMacroDefinitions identifier =
  filter (\macroDef -> macroDef ^. functionDefinition . name == identifier) <$>
  use @MacroExpansionEnv macroDefs

hygenisizeMacroDefinition :: MacroDefinition ann -> MacroDefinition ann
hygenisizeMacroDefinition macroDef =
  macroDef & functionDefinition . name %~ hygenisizeMacroName

generateMacroProgram ::
     (Members '[ Effs.Error SM.Error, Effs.FileSystem, Effs.Resource, Effs.State MacroExpansionEnv] effs)
  => Identifier
  -> [SM.Expression]
  -> Eff effs (String, String, String)
generateMacroProgram oldMacroName args = do
  auxEnv <- use @MacroExpansionEnv stmts
  let newMacroName = hygenisizeMacroName oldMacroName
  let insertDefName =
        let defNamePlaceholder = "%%%MACRO_NAME%%%"
         in replace defNamePlaceholder newMacroName
  let insertArgs =
        let argsPlaceholder = "%%%ARGUMENTS%%%"
         in replace argsPlaceholder (show args)
  let (preModuleEnv, postModuleEnv) =
        if any (is _SModuleDeclaration) auxEnv
          then case splitWhen (is _SModuleDeclaration) auxEnv of
                 preEnv:postEnv:_ -> (preEnv, postEnv)
                 _ -> fatal "generateMacroProgram" "0001"
          else ([], auxEnv)
  let renderStmts =
        prettifyHaskell . delimit Newlines . map (SM.raw . toHaskell)
  astDef <- readResource Res.astDefinition
  scaffold <- insertArgs <$> readResource Res.macroScaffold
  hygenicMacroDefs <-
    map hygenisizeMacroDefinition <$> use @MacroExpansionEnv macroDefs
  macroDefAndEnv <-
    do header <- readResource Res.macroDefinitionAndEnvironmentHeader
       footer <-
         insertDefName <$> readResource Res.macroDefinitionAndEnvironmentFooter
       pure $
         unlines
           [ renderStmts preModuleEnv
           , header
           , renderStmts postModuleEnv
           , renderStmts $ map SMacroDefinition hygenicMacroDefs <>
             map STypeSignature (typeMacroDefinitions hygenicMacroDefs)
           , footer
           ]
  pure (astDef, scaffold, macroDefAndEnv)

expansionPass ::
     forall fileExpanderEffs effs.
     ( Members '[ Effs.Error SM.Error, Effs.FileSystem, Effs.Ghci, Effs.Process, Effs.Reader ( Ghci.Ghci
                                                                                             , FilePath), Effs.Resource, Effs.State ModuleInfo] effs
     , Members fileExpanderEffs (Effs.State MacroExpansionEnv ': effs)
     )
  => FileExpander fileExpanderEffs
  -> SM.Expression
  -> Eff effs SM.Expression
expansionPass expandFile programExpr =
  Parse.topLevelExpressionsToProgram . map denormalizeStatement <$>
  expandMacros
    @fileExpanderEffs
    expandFile
    (Parse.programToTopLevelExpressions programExpr)

exhaustivelyExpandMacros ::
     forall fileExpanderEffs effs.
     ( Members '[ Effs.Error SM.Error, Effs.FileSystem, Effs.Ghci, Effs.Process, Effs.Resource, Effs.State ModuleInfo] effs
     , Members fileExpanderEffs (Effs.State MacroExpansionEnv ': Effs.Reader ( Ghci.Ghci
                                                                             , FilePath) ': effs)
     )
  => FilePath
  -> FileExpander fileExpanderEffs
  -> SM.Expression
  -> Eff effs SM.Expression
exhaustivelyExpandMacros filePath expandFile program = do
  ghci <- Ghci.start
  Ghci.enableJsonErrors ghci
  expandedTopLevelExprs <-
    Effs.runReader (ghci, filePath) $ Parse.programToTopLevelExpressions <$>
    exhaustM (expansionPass @fileExpanderEffs expandFile) program
  macroTypeSigs <-
    do normalizedStmts <-
         Effs.runReader filePath $ withExprCtxt $
         traverse normalizeStatement expandedTopLevelExprs
       let typeSigs =
             typeMacroDefinitions $ mapMaybe isMacroDefinition normalizedStmts
       pure $ map (denormalizeStatement . STypeSignature) typeSigs
  Ghci.stop ghci
  pure $
    Parse.topLevelExpressionsToProgram (expandedTopLevelExprs <> macroTypeSigs)
  where
    isMacroDefinition (SMacroDefinition x) = Just x
    isMacroDefinition _ = Nothing

typeMacroDefinitions :: [MacroDefinition ann] -> [TypeSignature SM.Expression]
typeMacroDefinitions macroDefs' = map mkTySig macroNames
  where
    macroNames = nub $ map (^. functionDefinition . name) macroDefs'
    mkTySigSource macroName =
      "(:: " <> macroName <>
      "(-> ([] (AST.Expression AST.SourceMetadata)) (IO ([] (AST.Expression AST.SourceMetadata)))))"
    mkTySig :: Identifier -> TypeSignature SM.Expression
    mkTySig macroName =
      let expr =
            fromJust $ head' $ unsafeIgnoreError @SM.Error $
            Parse.parseMultiple Nothing $
            mkTySigSource macroName
       in unsafeIgnoreError
            @SM.Error
            (Effs.runReader "" $ withExprCtxt $ normalizeStatement expr) ^?!
          _STypeSignature

expandMacroApplication ::
     (Members '[ Effs.Error SM.Error, Effs.FileSystem, Effs.Ghci, Effs.Process, Effs.Resource, Effs.Reader ( Ghci.Ghci
                                                                                                           , FilePath), Effs.State MacroExpansionEnv] effs)
  => Identifier
  -> [SM.Expression]
  -> Eff effs [SM.Expression]
expandMacroApplication macroName args = do
  filePath' <- view @(Ghci.Ghci, FilePath) _2
  macroProgram <- generateMacroProgram macroName args
  newSource <- uncurry3 evalMacro macroProgram
  Parse.parseMultiple (Just filePath') newSource

isMacroDefinitionStatement :: Statement ann -> Bool
isMacroDefinitionStatement (SMacroDefinition _) = True
isMacroDefinitionStatement _ = False

evalMacro ::
     (Members '[ Effs.Error SM.Error, Effs.FileSystem, Effs.Ghci, Effs.Process, Effs.Reader ( Ghci.Ghci
                                                                                            , FilePath)] effs)
  => String
  -> String
  -> String
  -> Eff effs String
evalMacro astDefinition scaffold macroDefinitionAndEnvironment = do
  setup
  (ghci, originalFilePath) <- ask
  loadResult <-
    Ghci.exec ghci $
    unwords
      [ ":l"
      , scaffoldFileName
      , astDefinitionFileName
      , macroDefinitionAndEnvironmentFileName
      ]
  if "Ok, three modules loaded." `elem` loadResult
    then do
      result <- unlines <$> Ghci.exec ghci ":main"
      cleanup
      pure result
    else throwError @SM.Error $
         MacroError
           ("While compiling '" <> originalFilePath <> "':\n\n" <>
            processErrors M.empty (unlines loadResult))
  where
    macroDefinitionAndEnvironmentFileName =
      "AutogeneratedAxelMacroDefinitionAndEnvironment.hs"
    scaffoldModuleName = "AutogeneratedAxelScaffold"
    scaffoldFileName = scaffoldModuleName <.> "hs"
    astDefinitionFileName = "AutogeneratedAxelASTDefinition.hs"
    setup = do
      FS.writeFile scaffoldFileName scaffold
      FS.writeFile astDefinitionFileName astDefinition
      FS.writeFile
        macroDefinitionAndEnvironmentFileName
        macroDefinitionAndEnvironment
    cleanup = do
      FS.removeFile astDefinitionFileName
      FS.removeFile macroDefinitionAndEnvironmentFileName
      FS.removeFile scaffoldFileName
