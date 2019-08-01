{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Axel.Macros where

import Prelude hiding (putStrLn)

import Axel (preludeMacros)
import Axel.AST
  ( Identifier
  , ImportSpecification(ImportAll)
  , MacroDefinition
  , MacroImport(MacroImport)
  , QualifiedImport(QualifiedImport)
  , SMStatement
  , Statement(SMacroDefinition, SMacroImport, SModuleDeclaration,
          SQualifiedImport, SRawStatement)
  , ToHaskell(toHaskell)
  , _SMacroDefinition
  , _SModuleDeclaration
  , functionDefinition
  , imports
  , moduleName
  , name
  )
import Axel.Denormalize (denormalizeStatement)
import qualified Axel.Eff.FileSystem as Effs (FileSystem)
import qualified Axel.Eff.FileSystem as FS
  ( createDirectoryIfMissing
  , withCurrentDirectory
  , writeFile
  )
import qualified Axel.Eff.Ghci as Effs (Ghci)
import qualified Axel.Eff.Ghci as Ghci (exec, start, stop)
import Axel.Eff.Lens (view)
import qualified Axel.Eff.Log as Effs (Log)
import Axel.Eff.Log (logStrLn)
import qualified Axel.Eff.Process as Effs (Process)
import Axel.Eff.Resource (readResource)
import qualified Axel.Eff.Resource as Effs (Resource)
import qualified Axel.Eff.Resource as Res (macroScaffold)
import qualified Axel.Eff.Restartable as Effs (Restartable)
import Axel.Eff.Restartable (restart, restartable)
import Axel.Error (Error(MacroError), fatal)
import Axel.Haskell.Macros (hygenisizeMacroName)
import Axel.Normalize
  ( normalizeStatement
  , unsafeNormalizeStatement
  , withExprCtxt
  )
import qualified Axel.Parse as Parse (parseMultiple)
import Axel.Parse.AST
  ( _SExpression
  , bottomUpFmapSplicing
  , quoteExpression
  , toAxel
  )
import qualified Axel.Parse.AST as Parse (Expression(SExpression, Symbol))
import Axel.Sourcemap
  ( isCompoundExpressionWrapperHead
  , quoteSourceMetadata
  , unwrapCompoundExpressions
  , wrapCompoundExpressions
  )
import qualified Axel.Sourcemap as SM (Expression, Output, raw)
import Axel.Utils.List (filterMap, filterMapOut, head')
import Axel.Utils.Recursion (bottomUpFmap, zipperTopDownTraverse)
import Axel.Utils.String (replace)
import Axel.Utils.Zipper (unsafeLeft, unsafeUp)

import Control.Lens (snoc)
import Control.Lens.Extras (is)
import Control.Lens.Operators ((%~), (^.), (^?))
import Control.Lens.Tuple (_1, _2)
import Control.Monad (guard, unless, when)
import Control.Monad.Freer (Eff, Member, Members)
import Control.Monad.Freer.Error (throwError)
import qualified Control.Monad.Freer.Error as Effs (Error)
import qualified Control.Monad.Freer.Reader as Effs (Reader, runReader)
import Control.Monad.Freer.State (get, gets, modify)
import qualified Control.Monad.Freer.State as Effs (State, evalState)

import Data.Function ((&))
import Data.Generics.Uniplate.Zipper (Zipper, fromZipper, hole, replaceHole, up)
import Data.Hashable (hash)
import Data.List (isPrefixOf, nub)
import Data.List.Split (split, whenElt)
import Data.Map (Map)
import qualified Data.Map as M (elems, filter, fromList, toList)
import Data.Maybe (isNothing)
import Data.Semigroup ((<>))

import qualified Language.Haskell.Ghcid as Ghci (Ghci)

import System.FilePath ((<.>), (</>))

type ModuleInfo = Map Identifier (FilePath, Maybe SM.Output)

getTranspiledFiles :: ModuleInfo -> Map FilePath SM.Output
getTranspiledFiles =
  M.fromList .
  filterMap
    (\case
       (_, Nothing) -> Nothing
       (filePath, Just output) -> Just (filePath, output)) .
  M.elems

type FileExpander effs
   = forall openEffs. (Members effs openEffs) =>
                        FilePath -> Eff openEffs ()

-- | Fully expand a program, and add macro definition type signatures.
processProgram ::
     forall fileExpanderEffs effs.
     ( Members '[ Effs.Error Error, Effs.FileSystem, Effs.Ghci, Effs.Log, Effs.Process, Effs.Resource, Effs.State ModuleInfo] effs
     , Members fileExpanderEffs (Effs.State [SMStatement] ': Effs.Restartable SM.Expression ': Effs.Reader ( Ghci.Ghci
                                                                                                           , FilePath) ': effs)
     )
  => FileExpander fileExpanderEffs
  -> FilePath
  -> SM.Expression
  -> Eff effs [SMStatement]
processProgram expandFile filePath program = do
  ghci <- Ghci.start
  newProgramExpr <-
    Effs.runReader (ghci, filePath) $
    expandProgramExpr @fileExpanderEffs expandFile program
  Ghci.stop ghci
  newStmts <-
    mapM
      (Effs.runReader filePath . withExprCtxt . normalizeStatement)
      (unwrapCompoundExpressions newProgramExpr)
  let addAstImport =
        insertImports [SRawStatement Nothing "import Axel.Parse.AST as AST"]
  pure $ finalizeProgram $ addAstImport newStmts

finalizeProgram :: [SMStatement] -> [SMStatement]
finalizeProgram stmts =
  let expandQuotes =
        bottomUpFmapSplicing
          (\case
             Parse.SExpression _ (Parse.Symbol _ "quote":xs) ->
               map (quoteExpression quoteSourceMetadata) xs
             x -> [x])
      convertList =
        bottomUpFmap $ \case
          Parse.Symbol ann' "List" -> Parse.Symbol ann' "[]"
          x -> x
      convertUnit =
        bottomUpFmap $ \case
          Parse.Symbol ann' "Unit" -> Parse.Symbol ann' "()"
          Parse.Symbol ann' "unit" -> Parse.Symbol ann' "()"
          x -> x
      (nonMacroDefs, macroDefs) = filterMapOut (^? _SMacroDefinition) stmts
      hygenicMacroDefs = map hygenisizeMacroDefinition macroDefs
      macroTySigs = typeMacroDefinitions hygenicMacroDefs
      toTopLevelStmts = map unsafeNormalizeStatement . unwrapCompoundExpressions
      toProgramExpr = wrapCompoundExpressions . map denormalizeStatement
   in toTopLevelStmts $ convertUnit $ convertList $ expandQuotes $ toProgramExpr $
      nonMacroDefs <>
      map SMacroDefinition hygenicMacroDefs <>
      macroTySigs

isMacroImported ::
     (Member (Effs.State [SMStatement]) effs) => Identifier -> Eff effs Bool
isMacroImported macroName = do
  let isFromPrelude = macroName `elem` preludeMacros
  isImportedDirectly <-
    any
      (\case
         SMacroImport macroImport -> macroName `elem` macroImport ^. imports
         _ -> False) <$>
    get @[SMStatement]
  pure $ isFromPrelude || isImportedDirectly

ensureCompiledDependency ::
     forall fileExpanderEffs effs.
     (Member (Effs.State ModuleInfo) effs, Members fileExpanderEffs effs)
  => FileExpander fileExpanderEffs
  -> MacroImport (Maybe SM.Expression)
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

isStatementFocused :: Zipper SM.Expression SM.Expression -> Bool
isStatementFocused zipper =
  let wholeProgramExpr = fromZipper zipper
      isCompoundExpr = Just wholeProgramExpr == (hole <$> up zipper)
      isCompoundExprWrapper =
        case hole zipper of
          Parse.Symbol _ "begin" -> True
          _ -> False
   in isCompoundExpr && not isCompoundExprWrapper

-- | Fully expand a top-level expression.
-- | Macro expansion is top-down: it proceeds top to bottom, outwards to inwards,
-- | and left to right. Whenever a macro is successfully expanded to yield new
-- | expressions in place of the macro call in question, the substitution is made
-- | and macro expansion is repeated from the beginning. As new definitions, etc.
-- | are found at the top level while the program tree is being traversed, they
-- | will be added to the environment accessible to macros during expansion.
expandProgramExpr ::
     forall fileExpanderEffs effs.
     ( Members '[ Effs.Error Error, Effs.FileSystem, Effs.Ghci, Effs.Log, Effs.Process, Effs.Resource, Effs.State ModuleInfo, Effs.Reader ( Ghci.Ghci
                                                                                                                                          , FilePath)] effs
     , Members fileExpanderEffs (Effs.State [SMStatement] ': Effs.Restartable SM.Expression ': effs)
     )
  => FileExpander fileExpanderEffs
  -> SM.Expression
  -> Eff effs SM.Expression
expandProgramExpr expandFile programExpr =
  restartable @SM.Expression programExpr $ Effs.evalState ([] :: [SMStatement]) .
  zipperTopDownTraverse
    (\zipper -> do
       when (isStatementFocused zipper) $
         -- NOTE This algorithm will exclude the last statement, but we won't
         --      have any macros that rely on it (since macros can only access
         --      what is before them in the file). Thus, this omission is okay.
         let prevTopLevelExpr = hole $ unsafeLeft zipper
          in unless (isCompoundExpressionWrapperHead prevTopLevelExpr) $
             addStatementToMacroEnvironment
               @fileExpanderEffs
               expandFile
               prevTopLevelExpr
       let expr = hole zipper
       when (is _SExpression expr) $ do
         maybeNewExprs <- handleFunctionApplication expr
         case maybeNewExprs of
           Just newExprs -> replaceExpr zipper newExprs >>= restart
           Nothing -> pure ()
       pure expr)

-- | Returns the full program expr (after the necessary substitution has been applied).
replaceExpr ::
     (Members '[ Effs.Error Error, Effs.Reader (Ghci.Ghci, FilePath)] effs)
  => Zipper SM.Expression SM.Expression
  -> [SM.Expression]
  -> Eff effs SM.Expression
replaceExpr zipper newExprs =
  let programExpr = fromZipper zipper
      oldExpr = hole zipper
      -- NOTE Using `unsafeUp` is safe since `begin` cannot be the name of a macro,
      --      and thus `zipper` will never be focused on the whole program.
      oldParentExprZ = unsafeUp zipper
      newParentExpr =
        case hole oldParentExprZ of
          Parse.SExpression ann' xs ->
            let xs' = do
                  x <- xs
                  -- TODO What if there are multiple, equivalent copies of `oldExpr`?
                  --      If they are not top-level statements, then the macro in question
                  --      must already exist and thus we would have expanded it already.
                  --      If they are top-level statements, but e.g. were auto-generated, then
                  --      `==` will call them equal. If the macro in question did not exist
                  --      when the first statement was defined, but it does by the second
                  --      statement, then our result may be incorrect.
                  if x == oldExpr
                    then newExprs
                    else pure x
             in Parse.SExpression ann' xs'
          _ -> fatal "expandProgramExpr" "0001"
      newProgramExpr = fromZipper $ replaceHole newParentExpr oldParentExprZ
   in if newProgramExpr == programExpr
        then throwLoopError oldExpr newExprs
        else pure newProgramExpr

throwLoopError ::
     (Members '[ Effs.Error Error, Effs.Reader (Ghci.Ghci, FilePath)] effs)
  => SM.Expression
  -> [SM.Expression]
  -> Eff effs a
throwLoopError oldExpr newExprs = do
  filePath <- view @(Ghci.Ghci, FilePath) _2
  throwError $
    MacroError
      filePath
      oldExpr
      ("Infinite loop detected during macro expansion!\nCheck that no macro calls expand (directly or indirectly) to themselves.\n" <>
       toAxel oldExpr <>
       " expanded into " <>
       unwords (map toAxel newExprs) <>
       ".")

addStatementToMacroEnvironment ::
     forall fileExpanderEffs effs.
     ( Members '[ Effs.Error Error, Effs.State ModuleInfo, Effs.Reader ( Ghci.Ghci
                                                                       , FilePath), Effs.State [SMStatement]] effs
     , Members fileExpanderEffs effs
     )
  => FileExpander fileExpanderEffs
  -> SM.Expression
  -> Eff effs ()
addStatementToMacroEnvironment expandFile newExpr = do
  filePath <- view @(Ghci.Ghci, FilePath) _2
  stmt <- Effs.runReader filePath $ withExprCtxt $ normalizeStatement newExpr
  case stmt of
    SMacroImport macroImport ->
      ensureCompiledDependency @fileExpanderEffs expandFile macroImport
    _ -> pure ()
  modify @[SMStatement] (`snoc` stmt)

-- | If a function application is a macro call, expand it.
handleFunctionApplication ::
     (Members '[ Effs.Error Error, Effs.FileSystem, Effs.Ghci, Effs.Log, Effs.Process, Effs.Resource, Effs.State ModuleInfo, Effs.Reader ( Ghci.Ghci
                                                                                                                                         , FilePath), Effs.State [SMStatement]] effs)
  => SM.Expression
  -> Eff effs (Maybe [SM.Expression])
handleFunctionApplication (Parse.SExpression _ (Parse.Symbol _ function:args)) = do
  shouldExpand <- isMacroCall function
  if shouldExpand
    then Just <$> expandMacroApplication function args
    else pure Nothing
handleFunctionApplication _ = pure Nothing

isMacroCall ::
     (Member (Effs.State [SMStatement]) effs) => Identifier -> Eff effs Bool
isMacroCall function = do
  localDefs <- lookupMacroDefinitions function
  let isDefinedLocally = not $ null localDefs
  isImported <- isMacroImported function
  pure $ isImported || isDefinedLocally

lookupMacroDefinitions ::
     (Member (Effs.State [SMStatement]) effs)
  => Identifier
  -> Eff effs [MacroDefinition (Maybe SM.Expression)]
lookupMacroDefinitions identifier =
  filterMap
    (\stmt -> do
       macroDef <- stmt ^? _SMacroDefinition
       guard $ identifier == (macroDef ^. functionDefinition . name)
       pure macroDef) <$>
  get @[SMStatement]

hygenisizeMacroDefinition :: MacroDefinition ann -> MacroDefinition ann
hygenisizeMacroDefinition = functionDefinition . name %~ hygenisizeMacroName

insertImports :: [SMStatement] -> [SMStatement] -> [SMStatement]
insertImports newStmts program =
  case split (whenElt $ is _SModuleDeclaration) program of
    [preEnv, moduleDeclaration, postEnv] ->
      preEnv <> moduleDeclaration <> newStmts <> postEnv
    [_] -> newStmts <> program
    _ -> fatal "insertImport" "0001"

mkMacroTypeSignature :: Identifier -> SMStatement
mkMacroTypeSignature =
  SRawStatement Nothing .
  (<> " :: [AST.Expression SM.SourceMetadata] -> IO [AST.Expression SM.SourceMetadata]")

generateMacroProgram ::
     (Members '[ Effs.Error Error, Effs.FileSystem, Effs.Resource, Effs.State [SMStatement]] effs)
  => Identifier
  -> [SM.Expression]
  -> Eff effs (String, String)
generateMacroProgram oldMacroName args = do
  let newMacroName = hygenisizeMacroName oldMacroName
  let mainFnName = "main_AXEL_AUTOGENERATED_FUNCTION_DEFINITION"
  let footer =
        [ mkMacroTypeSignature mainFnName
        , SRawStatement Nothing $ mainFnName <> " = " <> newMacroName
        ]
  let header =
        [ SMacroImport $ MacroImport Nothing "Axel" preludeMacros
        , SQualifiedImport $
          QualifiedImport Nothing "Axel.Parse.AST" "AST" (ImportAll Nothing)
        , SQualifiedImport $
          QualifiedImport Nothing "Axel.Sourcemap" "SM" (ImportAll Nothing)
        , SQualifiedImport $
          QualifiedImport
            Nothing
            "Unsafe.Coerce"
            "Unsafe.Coerce_AXEL_AUTOGENERATED_IMPORT"
            (ImportAll Nothing)
        ]
  let insertArgs =
        let argsPlaceholder = "%%%ARGUMENTS%%%"
         in replace argsPlaceholder (show args)
  let renderStmts :: [SMStatement] -> String
      renderStmts = unlines . map (SM.raw . toHaskell)
  scaffold <- insertArgs <$> readResource Res.macroScaffold
  auxEnv <- get @[SMStatement]
  -- TODO If the file being transpiled has pragmas but no explicit module declaration,
  --      they will be erroneously included *after* the module declaration.
  --      Should we just require Axel files to have module declarations, or is there a
  --      less intrusive alternate solution?
  let macroDefAndEnv =
        let moduleDecl =
              SModuleDeclaration
                Nothing
                "AutogeneratedAxelMacroDefinitionAndEnvironment"
            programStmts =
              insertImports header $ moduleDecl : (auxEnv <> footer)
         in renderStmts $ finalizeProgram programStmts
  pure (scaffold, macroDefAndEnv)

typeMacroDefinitions :: [MacroDefinition ann] -> [SMStatement]
typeMacroDefinitions = map mkMacroTypeSignature . getMacroNames
  where
    getMacroNames = nub . map (^. functionDefinition . name)

-- | Source metadata is lost.
-- | Use only for logging and such where that doesn't matter.
losslyReconstructMacroCall :: Identifier -> [SM.Expression] -> SM.Expression
losslyReconstructMacroCall macroName args =
  Parse.SExpression
    Nothing
    (Parse.Symbol Nothing macroName : map (Nothing <$) args)

expandMacroApplication ::
     (Members '[ Effs.Error Error, Effs.FileSystem, Effs.Ghci, Effs.Log, Effs.Process, Effs.Resource, Effs.Reader ( Ghci.Ghci
                                                                                                                  , FilePath), Effs.State [SMStatement]] effs)
  => Identifier
  -> [SM.Expression]
  -> Eff effs [SM.Expression]
expandMacroApplication macroName args = do
  logStrLn $ "Expanding: " <> toAxel (losslyReconstructMacroCall macroName args)
  filePath' <- view @(Ghci.Ghci, FilePath) _2
  macroProgram <- generateMacroProgram macroName args
  newSource <- uncurry (evalMacro (macroName, args)) macroProgram
  Parse.parseMultiple (Just filePath') newSource

isMacroDefinitionStatement :: Statement ann -> Bool
isMacroDefinitionStatement (SMacroDefinition _) = True
isMacroDefinitionStatement _ = False

evalMacro ::
     forall effs.
     (Members '[ Effs.Error Error, Effs.FileSystem, Effs.Ghci, Effs.Process, Effs.Reader ( Ghci.Ghci
                                                                                         , FilePath)] effs)
  => (Identifier, [SM.Expression])
  -> String
  -> String
  -> Eff effs String
evalMacro (macroName, args) scaffold macroDefinitionAndEnvironment = do
  tmpDir <- getTempDirectory
  let macroDefinitionAndEnvironmentFileName =
        tmpDir </> "AutogeneratedAxelMacroDefinitionAndEnvironment.hs"
  let scaffoldFileName = tmpDir </> scaffoldModuleName <.> "hs"
  FS.writeFile scaffoldFileName scaffold
  FS.writeFile
    macroDefinitionAndEnvironmentFileName
    macroDefinitionAndEnvironment
  ghci <- view @(Ghci.Ghci, FilePath) _1
  loadResult <-
    Ghci.exec ghci $
    unwords [":l", scaffoldFileName, macroDefinitionAndEnvironmentFileName]
  if "Ok, two modules loaded." `elem` loadResult
    then do
      result <- Ghci.exec ghci ":main"
      unlines result &
        if any ("*** Exception:" `isPrefixOf`) result
          then throwMacroError
          else pure
    else throwMacroError (unlines loadResult)
  where
    getTempDirectory = do
      let uniqueId = hash $ scaffold <> macroDefinitionAndEnvironment
      let dirName = "axelTemp" </> show uniqueId
      FS.createDirectoryIfMissing True dirName
      pure dirName
    scaffoldModuleName = "AutogeneratedAxelScaffold"
    throwMacroError :: String -> Eff effs a
    throwMacroError msg = do
      originalFilePath <- view @(Ghci.Ghci, FilePath) _2
      throwError @Error $
        MacroError
          originalFilePath
          (losslyReconstructMacroCall macroName args)
          msg
