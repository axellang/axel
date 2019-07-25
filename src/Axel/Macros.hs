{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Macros where

import Prelude hiding (putStrLn)

import Axel.AST
  ( Identifier
  , MacroDefinition
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
import Axel.Normalize (normalizeStatement)
import qualified Axel.Parse as Parse
  ( Expression(SExpression, Symbol)
  , SourceMetadata
  , parseMultiple
  , programToTopLevelExpressions
  , topLevelExpressionsToProgram
  )
import Axel.Sourcemap (Delimiter(Newlines))
import qualified Axel.Sourcemap as SM (Error, Expression, Output, raw)
import Axel.Utils.Display (delimit)
import Axel.Utils.Function (uncurry3)
import Axel.Utils.List (filterMap, head')
import Axel.Utils.Recursion (Recursive(bottomUpTraverse), exhaustM)
import Axel.Utils.String (replace)

import Control.Lens.Cons (snoc)
import Control.Lens.Extras (is)
import Control.Lens.Operators ((%~), (^.), (^?!))
import Control.Lens.Tuple (_1, _2)
import Control.Monad (foldM, void, when)
import Control.Monad.Freer (Eff, Members)
import Control.Monad.Freer.Error (throwError)
import qualified Control.Monad.Freer.Error as Effs (Error)
import Control.Monad.Freer.State (gets)
import qualified Control.Monad.Freer.State as Effs (State)

import Data.Function ((&))
import Data.List (nub)
import Data.List.Split (splitWhen)
import Data.Map (Map)
import qualified Data.Map as M (elems, empty, filter, fromList, toList)
import Data.Maybe (isNothing, mapMaybe)
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

hygenisizeMacroDefinition :: MacroDefinition ann -> MacroDefinition ann
hygenisizeMacroDefinition macroDef =
  macroDef & functionDefinition . name %~ hygenisizeMacroName

generateMacroProgram ::
     (Members '[ Effs.Error SM.Error, Effs.FileSystem, Effs.Resource] effs)
  => Identifier
  -> [MacroDefinition SM.Expression]
  -> [Statement SM.Expression]
  -> [SM.Expression]
  -> Eff effs (String, String, String)
generateMacroProgram oldMacroName macroDefs allEnv args = do
  let newMacroName = hygenisizeMacroName oldMacroName
  let insertDefName =
        let defNamePlaceholder = "%%%MACRO_NAME%%%"
         in replace defNamePlaceholder newMacroName
  let insertArgs =
        let argsPlaceholder = "%%%ARGUMENTS%%%"
         in replace argsPlaceholder (show args)
  let (preModuleEnv, postModuleEnv) =
        if any (is _SModuleDeclaration) allEnv
          then case splitWhen (is _SModuleDeclaration) allEnv of
                 preEnv:postEnv:_ -> (preEnv, postEnv)
                 _ -> fatal "generateMacroProgram" "0001"
          else ([], allEnv)
  astDef <- readResource Res.astDefinition
  scaffold <- insertArgs <$> readResource Res.macroScaffold
  let hygenicMacroDefs = map hygenisizeMacroDefinition macroDefs
  let renderStmts =
        prettifyHaskell . delimit Newlines . map (SM.raw . toHaskell)
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
     (Members '[ Effs.Error SM.Error, Effs.FileSystem, Effs.Ghci, Effs.Process, Effs.Resource, Effs.State ModuleInfo] effs)
  => Ghci.Ghci
  -> FilePath
  -> (FilePath -> Eff effs a)
  -> SM.Expression
  -> Eff effs SM.Expression
expansionPass ghci filePath expandFile programExpr =
  Parse.topLevelExpressionsToProgram . map denormalizeStatement <$>
  expandMacros
    ghci
    filePath
    expandFile
    (Parse.programToTopLevelExpressions programExpr)

exhaustivelyExpandMacros ::
     (Members '[ Effs.Error SM.Error, Effs.FileSystem, Effs.Ghci, Effs.Process, Effs.Resource, Effs.State ModuleInfo] effs)
  => FilePath
  -> (FilePath -> Eff effs a)
  -> SM.Expression
  -> Eff effs SM.Expression
exhaustivelyExpandMacros filePath expandFile program = do
  ghci <- Ghci.start
  Ghci.enableJsonErrors ghci
  expandedTopLevelExprs <-
    Parse.programToTopLevelExpressions <$>
    exhaustM (expansionPass ghci filePath expandFile) program
  macroTypeSigs <-
    do normalizedStmts <- traverse normalizeStatement expandedTopLevelExprs
       let typeSigs =
             typeMacroDefinitions $ mapMaybe isMacroDefinition normalizedStmts
       pure $ map (denormalizeStatement . STypeSignature) typeSigs
  Ghci.stop ghci
  pure $
    Parse.topLevelExpressionsToProgram (expandedTopLevelExprs <> macroTypeSigs)
  where
    isMacroDefinition (SMacroDefinition x) = Just x
    isMacroDefinition _ = Nothing

isMacroImported :: Identifier -> [Statement ann] -> Bool
isMacroImported macroName =
  any
    (\case
       SMacroImport macroImport -> macroName `elem` macroImport ^. imports
       _ -> False)

typeMacroDefinitions :: [MacroDefinition ann] -> [TypeSignature SM.Expression]
typeMacroDefinitions macroDefs = map mkTySig macroNames
  where
    macroNames = nub $ map (^. functionDefinition . name) macroDefs
    mkTySigSource macroName =
      "(:: " <> macroName <>
      "(-> ([] (AST.Expression AST.SourceMetadata)) (IO ([] (AST.Expression AST.SourceMetadata)))))"
    mkTySig :: Identifier -> TypeSignature SM.Expression
    mkTySig macroName =
      let expr =
            head $ unsafeIgnoreError @SM.Error $ Parse.parseMultiple $
            mkTySigSource macroName
       in unsafeIgnoreError @SM.Error (normalizeStatement expr) ^?!
          _STypeSignature

expandMacros ::
     (Members '[ Effs.Error SM.Error, Effs.FileSystem, Effs.Ghci, Effs.Process, Effs.Resource, Effs.State ModuleInfo] effs)
  => Ghci.Ghci
  -> FilePath
  -> (FilePath -> Eff effs a)
  -> [SM.Expression]
  -> Eff effs [Statement SM.Expression]
expandMacros ghci filePath expandFile topLevelExprs = do
  (stmts, macroDefs) <-
    foldM
      (\acc@(stmts, macroDefs) expr -> do
         expandedExprs <- fullyExpandExpr stmts macroDefs expr
         foldM
           (\acc' expandedExpr -> do
              stmt <- normalizeStatement expandedExpr
              case stmt of
                SMacroDefinition macroDef ->
                  pure $ acc' & _2 %~ flip snoc macroDef
                _ -> do
                  case stmt of
                    SMacroImport macroImport -> do
                      moduleInfo <-
                        gets
                          @ModuleInfo
                          (M.filter
                             (\(moduleId', _) ->
                                moduleId' == macroImport ^. moduleName))
                      case head' $ M.toList moduleInfo of
                        Just (dependencyFilePath, (_, transpiledOutput)) ->
                          when (isNothing transpiledOutput) $
                          void (expandFile dependencyFilePath)
                        Nothing -> pure ()
                    _ -> pure ()
                  pure $ acc' & _1 %~ flip snoc stmt)
           acc
           expandedExprs)
      ([], [])
      topLevelExprs
  pure $ stmts <> map (SMacroDefinition . hygenisizeMacroDefinition) macroDefs
  where
    fullyExpandExpr stmts allMacroDefs expr = do
      let program = Parse.topLevelExpressionsToProgram [expr]
      expandedExpr <-
        exhaustM
          (bottomUpTraverse
             (\case
                Parse.SExpression ann' xs ->
                  Parse.SExpression ann' <$>
                  foldM
                    (\acc x ->
                       case x of
                         Parse.SExpression _ (Parse.Symbol _ function:args) ->
                           let maybeMacroDefs =
                                 if isMacroImported function stmts
                                   then Just []
                                   else case lookupMacroDefinitions
                                               function
                                               allMacroDefs of
                                          [] -> Nothing
                                          macroDefs -> Just macroDefs
                            in case maybeMacroDefs of
                                 Just macroDefs ->
                                   (acc <>) <$>
                                   expandMacroApplication
                                     ghci
                                     filePath
                                     function
                                     macroDefs
                                     stmts
                                     args
                                 Nothing -> pure $ snoc acc x
                         _ -> pure $ snoc acc x)
                    []
                    xs
                x -> pure x))
          program
      pure $ Parse.programToTopLevelExpressions expandedExpr

expandMacroApplication ::
     (Members '[ Effs.Error SM.Error, Effs.FileSystem, Effs.Ghci, Effs.Process, Effs.Resource] effs)
  => Ghci.Ghci
  -> FilePath
  -> Identifier
  -> [MacroDefinition SM.Expression]
  -> [Statement SM.Expression]
  -> [SM.Expression]
  -> Eff effs [SM.Expression]
expandMacroApplication ghci macroFilePath macroName macroDefs auxEnv args = do
  macroProgram <- generateMacroProgram macroName macroDefs auxEnv args
  newSource <-
    uncurry3 (evalMacro @Parse.SourceMetadata ghci macroFilePath) macroProgram
  Parse.parseMultiple newSource

lookupMacroDefinitions ::
     Identifier -> [MacroDefinition ann] -> [MacroDefinition ann]
lookupMacroDefinitions identifier =
  filter (\macroDef -> macroDef ^. functionDefinition . name == identifier)

isMacroDefinitionStatement :: Statement ann -> Bool
isMacroDefinitionStatement (SMacroDefinition _) = True
isMacroDefinitionStatement _ = False

evalMacro ::
     forall ann effs.
     (Members '[ Effs.Error (Error ann), Effs.FileSystem, Effs.Ghci, Effs.Process] effs)
  => Ghci.Ghci
  -> FilePath
  -> String
  -> String
  -> String
  -> Eff effs String
evalMacro ghci originalFilePath astDefinition scaffold macroDefinitionAndEnvironment = do
  setup
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
    else throwError @(Error ann) $
         MacroError
           ("While compiling '" <> originalFilePath <>
            "', the following errors were encountered:\n\n" <>
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
