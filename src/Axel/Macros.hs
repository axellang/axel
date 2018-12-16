{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Macros where

import Prelude hiding (putStrLn)

import Axel.AST
  ( Expression(EFunctionApplication, EIdentifier)
  , FunctionApplication(FunctionApplication)
  , Identifier
  , Import(ImportItem)
  , ImportSpecification(ImportOnly)
  , MacroDefinition
  , Statement(SDataDeclaration, SFunctionDefinition, SMacroDefinition,
          SMacroImport, SModuleDeclaration, SNewtypeDeclaration, SPragma,
          SQualifiedImport, SRawStatement, SRestrictedImport, STopLevel,
          STypeSignature, STypeSynonym, STypeclassDefinition,
          STypeclassInstance, SUnrestrictedImport)
  , ToHaskell(toHaskell)
  , TypeSignature(TypeSignature)
  , functionDefinition
  , imports
  , moduleName
  , name
  , restrictedImport
  )
import Axel.Denormalize (denormalizeStatement)
import Axel.Eff.Console (putStrLn)
import qualified Axel.Eff.Console as Effs (Console)
import qualified Axel.Eff.FileSystem as Effs (FileSystem)
import qualified Axel.Eff.FileSystem as FS (removeFile, writeFile)
import Axel.Eff.Process (StreamSpecification(CreateStreams), runProcess)
import qualified Axel.Eff.Process as Effs (Process)
import Axel.Eff.Resource (readResource)
import qualified Axel.Eff.Resource as Effs (Resource)
import qualified Axel.Eff.Resource as Res
  ( astDefinition
  , macroDefinitionAndEnvironmentFooter
  , macroDefinitionAndEnvironmentHeader
  , macroScaffold
  )
import Axel.Error (Error(MacroError))
import Axel.Haskell.Macros (hygenisizeMacroName)
import Axel.Haskell.Prettify (prettifyHaskell)
import Axel.Normalize (normalizeStatement)
import qualified Axel.Parse as Parse
  ( Expression(SExpression, Symbol)
  , parseMultiple
  , programToTopLevelExpressions
  , topLevelExpressionsToProgram
  )
import Axel.Parse.AST (toAxel)
import Axel.Utils.Display (Delimiter(Newlines), delimit)
import Axel.Utils.Function (uncurry3)
import Axel.Utils.Recursion (Recursive(bottomUpTraverse), exhaustM)
import Axel.Utils.String (replace)

import Debug.Trace (trace)

import Control.Lens.Cons (snoc)
import Control.Lens.Operators ((%~), (^.))
import Control.Lens.Tuple (_1, _2)
import Control.Monad (foldM, unless, void)
import Control.Monad.Freer (Eff, Members)
import Control.Monad.Freer.Error (throwError)
import qualified Control.Monad.Freer.Error as Effs (Error)
import Control.Monad.Freer.State (gets)
import qualified Control.Monad.Freer.State as Effs (State)

import Data.Function ((&))
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map (filter, toList)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Semigroup ((<>))

import System.Exit (ExitCode(ExitFailure))
import System.FilePath ((<.>))

type ModuleInfo = Map Identifier (FilePath, Bool)

hygenisizeMacroDefinition :: MacroDefinition -> MacroDefinition
hygenisizeMacroDefinition macroDef =
  macroDef & functionDefinition . name %~ hygenisizeMacroName

generateMacroProgram ::
     (Members '[ Effs.Error Error, Effs.FileSystem, Effs.Resource] effs)
  => Identifier
  -> [MacroDefinition]
  -> [Statement]
  -> [Parse.Expression]
  -> Eff effs (String, String, String)
generateMacroProgram oldMacroName macroDefs env args = do
  astDef <- readResource Res.astDefinition
  scaffold <- insertArgs <$> readResource Res.macroScaffold
  macroDefAndEnv <-
    do header <- readResource Res.macroDefinitionAndEnvironmentHeader
       footer <-
         insertDefName <$> readResource Res.macroDefinitionAndEnvironmentFooter
       pure $ unlines [header, macroDefAndEnvBody, footer]
  pure (astDef, scaffold, macroDefAndEnv)
  where
    insertDefName =
      let defNamePlaceholder = "%%%MACRO_NAME%%%"
       in replace defNamePlaceholder newMacroName
    insertArgs =
      let argsPlaceholder = "%%%ARGUMENTS%%%"
       in replace argsPlaceholder (show args)
    newMacroName = hygenisizeMacroName oldMacroName
    macroDefAndEnvBody =
      let hygenicMacroDefs = map hygenisizeMacroDefinition macroDefs
       in prettifyHaskell $ delimit Newlines $
          map toHaskell (env <> map SMacroDefinition hygenicMacroDefs)

expansionPass ::
     (Members '[ Effs.Console, Effs.Error Error, Effs.FileSystem, Effs.Process, Effs.Resource, Effs.State ModuleInfo] effs)
  => (FilePath -> Eff effs a)
  -> Parse.Expression
  -> Eff effs Parse.Expression
expansionPass expandFile programExpr =
  Parse.topLevelExpressionsToProgram . map denormalizeStatement <$>
  expandMacros expandFile (Parse.programToTopLevelExpressions programExpr)

exhaustivelyExpandMacros ::
     (Members '[ Effs.Console, Effs.Error Error, Effs.FileSystem, Effs.Process, Effs.Resource, Effs.State ModuleInfo] effs)
  => (FilePath -> Eff effs a)
  -> Parse.Expression
  -> Eff effs Parse.Expression
exhaustivelyExpandMacros expandFile program = do
  expandedTopLevelExprs <-
    Parse.programToTopLevelExpressions <$>
    exhaustM (expansionPass expandFile) program
  macroTypeSigs <-
    do normalizedStmts <- traverse normalizeStatement expandedTopLevelExprs
       let typeSigs =
             typeMacroDefinitions $ catMaybes $
             map isMacroDefinition normalizedStmts
       pure $ map (denormalizeStatement . STypeSignature) typeSigs
  pure $
    Parse.topLevelExpressionsToProgram (expandedTopLevelExprs <> macroTypeSigs)
  where
    isMacroDefinition (SMacroDefinition x) = Just x
    isMacroDefinition _ = Nothing

isStatementNonconflicting :: Statement -> Bool
isStatementNonconflicting (SDataDeclaration _) = True
isStatementNonconflicting (SFunctionDefinition _) = True
isStatementNonconflicting (SPragma _) = True
isStatementNonconflicting (SMacroDefinition _) = True
isStatementNonconflicting (SMacroImport _) = True
isStatementNonconflicting (SModuleDeclaration _) = False
isStatementNonconflicting (SNewtypeDeclaration _) = True
isStatementNonconflicting (SQualifiedImport _) = True
isStatementNonconflicting (SRawStatement _) = True
isStatementNonconflicting (SRestrictedImport _) = True
isStatementNonconflicting (STopLevel _) = False
isStatementNonconflicting (STypeclassDefinition _) = True
isStatementNonconflicting (STypeclassInstance _) = True
isStatementNonconflicting (STypeSignature _) = True
isStatementNonconflicting (STypeSynonym _) = True
isStatementNonconflicting (SUnrestrictedImport _) = True

isMacroImported :: Identifier -> [Statement] -> Bool
isMacroImported macroName env =
  any
    (\case
       SMacroImport macroImport ->
         case macroImport ^. restrictedImport . imports of
           ImportOnly importList ->
             any (== ImportItem (hygenisizeMacroName macroName)) importList
           _ -> False
       _ -> False)
    env

typeMacroDefinitions :: [MacroDefinition] -> [TypeSignature]
typeMacroDefinitions macroDefs =
  map
    (flip
       TypeSignature
       (EFunctionApplication $
        FunctionApplication
          (EIdentifier "->")
          [ EFunctionApplication $
            FunctionApplication
              (EIdentifier "[]")
              [EIdentifier "AST.Expression"]
          , EFunctionApplication $
            FunctionApplication
              (EIdentifier "IO")
              [ EFunctionApplication $
                FunctionApplication
                  (EIdentifier "[]")
                  [EIdentifier "AST.Expression"]
              ]
          ]))
    macroNames
  where
    macroNames = nub $ map (^. functionDefinition . name) macroDefs

expandMacros ::
     (Members '[ Effs.Console, Effs.Error Error, Effs.FileSystem, Effs.Process, Effs.Resource, Effs.State ModuleInfo] effs)
  => (FilePath -> Eff effs a)
  -> [Parse.Expression]
  -> Eff effs [Statement]
expandMacros expandFile topLevelExprs = do
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
                      let moduleId =
                            macroImport ^. restrictedImport . moduleName
                      moduleInfo <-
                        gets
                          @ModuleInfo
                          (Map.filter (\(moduleId', _) -> moduleId' == moduleId))
                      case listToMaybe $ Map.toList $ moduleInfo of
                        Just (dependencyFilePath, (_, isCompiled)) ->
                          unless isCompiled $ do
                            void $ expandFile dependencyFilePath
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
                Parse.SExpression xs ->
                  Parse.SExpression <$>
                  foldM
                    (\acc x ->
                       case x of
                         Parse.SExpression (Parse.Symbol function:args) ->
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
                                     function
                                     macroDefs
                                     (filter isStatementNonconflicting stmts)
                                     args
                                 Nothing -> pure $ snoc acc x
                         _ -> pure $ snoc acc x)
                    []
                    xs
                x -> pure x))
          program
      pure $ Parse.programToTopLevelExpressions expandedExpr

expandMacroApplication ::
     (Members '[ Effs.Console, Effs.Error Error, Effs.FileSystem, Effs.Process, Effs.Resource] effs)
  => Identifier
  -> [MacroDefinition]
  -> [Statement]
  -> [Parse.Expression]
  -> Eff effs [Parse.Expression]
expandMacroApplication macroName macroDefs auxEnv args = do
  macroProgram <-
    trace (toAxel $ Parse.SExpression (Parse.Symbol macroName : args)) $
    generateMacroProgram macroName macroDefs auxEnv args
  newSource <- uncurry3 evalMacro macroProgram
  Parse.parseMultiple newSource

lookupMacroDefinitions :: Identifier -> [MacroDefinition] -> [MacroDefinition]
lookupMacroDefinitions identifier =
  filter (\macroDef -> macroDef ^. functionDefinition . name == identifier)

isMacroDefinitionStatement :: Statement -> Bool
isMacroDefinitionStatement (SMacroDefinition _) = True
isMacroDefinitionStatement _ = False

evalMacro ::
     forall effs.
     (Members '[ Effs.Console, Effs.Error Error, Effs.FileSystem, Effs.Process] effs)
  => String
  -> String
  -> String
  -> Eff effs String
evalMacro astDefinition scaffold macroDefinitionAndEnvironment = do
  let macroDefinitionAndEnvironmentFileName =
        "AutogeneratedAxelMacroDefinitionAndEnvironment.hs"
  let scaffoldModuleName = "AutogeneratedAxelScaffold"
  let scaffoldFileName = scaffoldModuleName <.> "hs"
  let astDefinitionFileName = "AutogeneratedAxelASTDefinition.hs"
  FS.writeFile astDefinitionFileName astDefinition
  FS.writeFile
    macroDefinitionAndEnvironmentFileName
    macroDefinitionAndEnvironment
  FS.writeFile scaffoldFileName scaffold
  putStrLn "START"
  let args =
        [ "-main-is"
        , scaffoldModuleName
        , "-o"
        , scaffoldModuleName
        , scaffoldFileName
        , astDefinitionFileName
        ]
  runProcess @'CreateStreams @effs "ghc" args "" >>= \case
    (ExitFailure _, _, stderr) -> throwError $ MacroError ("Error:\n" <> stderr)
    _ ->
      runProcess @'CreateStreams @effs ("./" <> scaffoldModuleName) [] "" >>= \case
        (ExitFailure _, _, stderr) ->
          throwError $ MacroError ("Error:\n" <> stderr)
        (_, stdout, _) -> do
          putStrLn "END\n\n"
          FS.removeFile astDefinitionFileName
          FS.removeFile macroDefinitionAndEnvironmentFileName
          FS.removeFile scaffoldFileName
          pure stdout
