{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Axel.Macros where

import Axel.Prelude

import Axel (isPrelude, preludeMacros)
import Axel.AST
  ( Expression(EFunctionApplication, EIdentifier, ERawExpression)
  , FunctionApplication(FunctionApplication)
  , FunctionDefinition(FunctionDefinition)
  , Identifier
  , ImportSpecification(ImportAll)
  , MacroDefinition
  , MacroImport(MacroImport)
  , QualifiedImport(QualifiedImport)
  , SMStatement
  , Statement(SFunctionDefinition, SMacroDefinition, SMacroImport,
          SMacroImport, SModuleDeclaration, SQualifiedImport,
          SQualifiedImport, SRawStatement, SRestrictedImport, STypeSignature)
  , ToHaskell(toHaskell)
  , TypeSignature(TypeSignature)
  , _SMacroDefinition
  , _SModuleDeclaration
  , functionDefinition
  , imports
  , moduleName
  , name
  , statementsToProgram
  )
import Axel.Denormalize (denormalizeStatement)
import qualified Axel.Eff as Effs
import Axel.Eff.Error (Error(MacroError), fatal)
import qualified Axel.Eff.FileSystem as Effs (FileSystem)
import qualified Axel.Eff.FileSystem as FS
import qualified Axel.Eff.Ghci as Effs (Ghci)
import qualified Axel.Eff.Ghci as Ghci
import qualified Axel.Eff.Log as Effs (Log)
import Axel.Eff.Log (logStrLn)
import qualified Axel.Eff.Process as Effs (Process)
import qualified Axel.Eff.Restartable as Effs (Restartable)
import Axel.Eff.Restartable (restart, runRestartable)
import Axel.Haskell.Error (processErrors)
import Axel.Haskell.Macros (hygenisizeMacroName)
import Axel.Normalize
  ( normalizeExpression
  , normalizeStatement
  , unsafeNormalize
  , unsafeNormalize
  , withExprCtxt
  )
import Axel.Parse (parseMultiple)
import Axel.Parse.AST (_SExpression, bottomUpFmapSplicing, toAxel)
import qualified Axel.Parse.AST as Parse
import Axel.Sourcemap
  ( ModuleInfo
  , SourceMetadata
  , isCompoundExpressionWrapperHead
  , quoteSMExpression
  , renderSourcePosition
  , unwrapCompoundExpressions
  , wrapCompoundExpressions
  )
import qualified Axel.Sourcemap as SM
import Axel.Utils.FilePath ((<.>), (</>), replaceExtension, takeFileName)
import Axel.Utils.List (filterMap, filterMapOut, head')
import Axel.Utils.Maybe (whenMaybe)
import Axel.Utils.Recursion (bottomUpFmap, zipperTopDownTraverse)
import Axel.Utils.Zipper (unsafeLeft, unsafeUp)

import Control.Lens (_1, op, snoc)
import Control.Lens.Extras (is)
import Control.Lens.Operators ((%~), (^.), (^?))
import Control.Monad (guard, unless, when)

import Data.Function (on)
import Data.Generics.Uniplate.Zipper (Zipper, fromZipper, hole, replaceHole, up)
import Data.Hashable (hash)
import Data.List (intersperse, nub)
import Data.List.Split (split, whenElt)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import Data.Semigroup ((<>))
import qualified Data.Text as T

import qualified Language.Haskell.Ghcid as Ghcid

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem
import qualified Polysemy.Reader as Sem
import qualified Polysemy.State as Sem

type FunctionApplicationExpanderArgs a = SM.Expression -> a

type FunctionApplicationExpander effs
   = Effs.Callback effs FunctionApplicationExpanderArgs (Maybe [SM.Expression])

type FileExpanderArgs a = FilePath -> a

type FileExpander effs = Effs.Callback effs FileExpanderArgs ()

-- | Fully expand a program, and add macro definition type signatures.
processProgram ::
     forall fileExpanderEffs funAppExpanderEffs effs innerEffs.
     ( innerEffs ~ (Sem.State [SMStatement] ': Effs.Restartable SM.Expression ': Sem.Reader FilePath ': effs)
     , Sem.Members '[ Sem.Error Error, Effs.Ghci, Sem.Reader Ghcid.Ghci, Sem.State ModuleInfo] effs
     , Sem.Members fileExpanderEffs innerEffs
     , Sem.Members funAppExpanderEffs innerEffs
     )
  => FunctionApplicationExpander funAppExpanderEffs
  -> FileExpander fileExpanderEffs
  -> FilePath
  -> SM.Expression
  -> Sem.Sem effs [SMStatement]
processProgram expandFunApp expandFile filePath program = do
  newProgramExpr <-
    Sem.runReader filePath $
    expandProgramExpr
      @funAppExpanderEffs
      @fileExpanderEffs
      expandFunApp
      expandFile
      program
  newStmts <-
    mapM
      (Sem.runReader filePath . withExprCtxt . normalizeStatement)
      (unwrapCompoundExpressions newProgramExpr)
  let addAstImport =
        insertImports
          [ SQualifiedImport $
            QualifiedImport Nothing "Prelude" "GHCPrelude" (ImportAll Nothing)
          , SQualifiedImport $
            QualifiedImport Nothing "Axel.Parse.AST" "AST" (ImportAll Nothing)
          ]
  pure $ finalizeProgram $ addAstImport newStmts

finalizeProgram :: [SMStatement] -> [SMStatement]
finalizeProgram stmts =
  let expandQuotes =
        bottomUpFmapSplicing
          (\case
             Parse.SExpression _ (Parse.Symbol _ "quote":xs) ->
               map quoteSMExpression xs
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
      toTopLevelStmts =
        map (unsafeNormalize normalizeStatement) . unwrapCompoundExpressions
      toProgramExpr = wrapCompoundExpressions . map denormalizeStatement
   in toTopLevelStmts $ convertUnit $ convertList $ expandQuotes $ toProgramExpr $
      nonMacroDefs <>
      map SMacroDefinition hygenicMacroDefs <>
      macroTySigs

isMacroImported ::
     (Sem.Member (Sem.State [SMStatement]) effs)
  => Identifier
  -> Sem.Sem effs Bool
isMacroImported macroName = do
  let isFromPrelude = macroName `elem` preludeMacros
  isImportedDirectly <-
    any
      (\case
         SMacroImport macroImport -> macroName `elem` macroImport ^. imports
         _ -> False) <$>
    Sem.get
  pure $ isFromPrelude || isImportedDirectly

ensureCompiledDependency ::
     forall fileExpanderEffs effs.
     (Sem.Member (Sem.State ModuleInfo) effs, Sem.Members fileExpanderEffs effs)
  => FileExpander fileExpanderEffs
  -> Identifier
  -> Sem.Sem effs ()
ensureCompiledDependency expandFile dependencyName = do
  moduleInfo <-
    Sem.gets (M.filter (\(moduleId', _) -> moduleId' == dependencyName))
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
     forall funAppExpanderEffs fileExpanderEffs effs innerEffs.
     ( innerEffs ~ (Sem.State [SMStatement] : Effs.Restartable SM.Expression ': effs)
     , Sem.Members '[ Sem.Error Error, Sem.State ModuleInfo, Sem.Reader Ghcid.Ghci, Sem.Reader FilePath] effs
     , Sem.Members funAppExpanderEffs innerEffs
     , Sem.Members fileExpanderEffs innerEffs
     )
  => FunctionApplicationExpander funAppExpanderEffs
  -> FileExpander fileExpanderEffs
  -> SM.Expression
  -> Sem.Sem effs SM.Expression
expandProgramExpr expandFunApp expandFile programExpr =
  runRestartable @SM.Expression programExpr $
  Sem.evalState ([] :: [SMStatement]) .
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
         maybeNewExprs <- expandFunApp expr
         case maybeNewExprs of
           Just newExprs -> replaceExpr zipper newExprs >>= restart
           Nothing -> pure ()
       pure expr)

-- | Returns the full program expr (after the necessary substitution has been applied).
replaceExpr ::
     (Sem.Members '[ Sem.Error Error, Sem.Reader FilePath] effs)
  => Zipper SM.Expression SM.Expression
  -> [SM.Expression]
  -> Sem.Sem effs SM.Expression
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
     (Sem.Members '[ Sem.Error Error, Sem.Reader FilePath] effs)
  => SM.Expression
  -> [SM.Expression]
  -> Sem.Sem effs a
throwLoopError oldExpr newExprs = do
  filePath <- Sem.ask
  Sem.throw $
    MacroError
      filePath
      oldExpr
      ("Infinite loop detected during macro expansion!\nCheck that no macro calls expand (directly or indirectly) to themselves.\n" <>
       toAxel oldExpr <>
       " expanded into " <>
       T.unwords (map toAxel newExprs) <>
       ".")

addStatementToMacroEnvironment ::
     forall fileExpanderEffs effs.
     ( Sem.Members '[ Sem.Error Error, Sem.State ModuleInfo, Sem.Reader FilePath, Sem.State [SMStatement]] effs
     , Sem.Members fileExpanderEffs effs
     )
  => FileExpander fileExpanderEffs
  -> SM.Expression
  -> Sem.Sem effs ()
addStatementToMacroEnvironment expandFile newExpr = do
  filePath <- Sem.ask
  stmt <- Sem.runReader filePath $ withExprCtxt $ normalizeStatement newExpr
  let maybeDependencyName =
        case stmt of
          SRestrictedImport restrictedImport ->
            Just $ restrictedImport ^. moduleName
          SQualifiedImport qualifiedImport ->
            Just $ qualifiedImport ^. moduleName
          SMacroImport macroImport -> Just $ macroImport ^. moduleName
          _ -> Nothing
  whenMaybe maybeDependencyName $
    ensureCompiledDependency @fileExpanderEffs expandFile
  Sem.modify @[SMStatement] (`snoc` stmt)

-- | If a function application is a macro call, expand it.
handleFunctionApplication ::
     (Sem.Members '[ Sem.Error Error, Effs.FileSystem, Effs.Ghci, Effs.Log, Effs.Process, Sem.State ModuleInfo, Sem.Reader Ghcid.Ghci, Sem.Reader FilePath, Sem.State [SMStatement]] effs)
  => SM.Expression
  -> Sem.Sem effs (Maybe [SM.Expression])
handleFunctionApplication fnApp@(Parse.SExpression ann (Parse.Symbol _ functionName:args)) = do
  shouldExpand <- isMacroCall $ T.pack functionName
  if shouldExpand
    then Just <$>
         withExpansionId
           fnApp
           (expandMacroApplication ann (T.pack functionName) args)
    else pure Nothing
handleFunctionApplication _ = pure Nothing

isMacroCall ::
     (Sem.Member (Sem.State [SMStatement]) effs)
  => Identifier
  -> Sem.Sem effs Bool
isMacroCall function = do
  localDefs <- lookupMacroDefinitions function
  let isDefinedLocally = not $ null localDefs
  isImported <- isMacroImported function
  pure $ isImported || isDefinedLocally

lookupMacroDefinitions ::
     (Sem.Member (Sem.State [SMStatement]) effs)
  => Identifier
  -> Sem.Sem effs [MacroDefinition (Maybe SM.Expression)]
lookupMacroDefinitions identifier =
  filterMap
    (\stmt -> do
       macroDef <- stmt ^? _SMacroDefinition
       guard $ identifier == (macroDef ^. functionDefinition . name)
       pure macroDef) <$>
  Sem.get

hygenisizeMacroDefinition :: MacroDefinition ann -> MacroDefinition ann
hygenisizeMacroDefinition = functionDefinition . name %~ hygenisizeMacroName

insertImports :: [SMStatement] -> [SMStatement] -> [SMStatement]
insertImports newStmts program =
  case split (whenElt $ is _SModuleDeclaration) program of
    [preEnv, moduleDeclaration, postEnv] ->
      preEnv <> moduleDeclaration <> newStmts <> postEnv
    [_] -> newStmts <> program
    _ -> fatal "insertImports" "0001" -- TODO Replace with error message indicating that only one module declaration is allowed!

mkMacroTypeSignature :: Identifier -> SMStatement
mkMacroTypeSignature =
  SRawStatement Nothing .
  (<> " :: [AST.Expression SM.SourceMetadata] -> IO [AST.Expression SM.SourceMetadata]")

newtype ExpansionId =
  ExpansionId Text

mkMacroDefAndEnvModuleName :: ExpansionId -> Identifier
mkMacroDefAndEnvModuleName (ExpansionId expansionId) =
  "AutogeneratedAxelMacroDefinitionAndEnvironment" <> expansionId

mkScaffoldModuleName :: ExpansionId -> Identifier
mkScaffoldModuleName (ExpansionId expansionId) =
  "AutogeneratedAxelScaffold" <> expansionId

generateMacroProgram ::
     (Sem.Members '[ Sem.Error Error, Effs.FileSystem, Sem.Reader ExpansionId, Sem.State [SMStatement]] effs)
  => FilePath
  -> Identifier
  -> [SM.Expression]
  -> Sem.Sem effs (SM.Output, SM.Output)
generateMacroProgram filePath' oldMacroName args = do
  macroDefAndEnvModuleName <- Sem.asks mkMacroDefAndEnvModuleName
  scaffoldModuleName <- Sem.asks mkScaffoldModuleName
  let newMacroName = hygenisizeMacroName oldMacroName
  let mainFnName = "main_AXEL_AUTOGENERATED_FUNCTION_DEFINITION"
  let footer =
        [ mkMacroTypeSignature mainFnName
        , SRawStatement Nothing $ mainFnName <> " = " <> newMacroName
        ]
  let (header, scaffold) =
        let mkModuleDecl = SModuleDeclaration Nothing
            mkQualImport moduleName' alias =
              SQualifiedImport $
              QualifiedImport Nothing moduleName' alias (ImportAll Nothing)
            mkMacroImport moduleName' macros =
              SMacroImport $ MacroImport Nothing moduleName' macros
            mkId = EIdentifier Nothing
            mkQualId moduleName' identifier =
              EIdentifier Nothing $ moduleName' <> "." <> identifier
            mkTySig fnName ty = STypeSignature $ TypeSignature Nothing fnName ty
            mkFnDef fnName args' body =
              SFunctionDefinition $
              FunctionDefinition Nothing fnName args' body []
            mkFnApp fn args' =
              EFunctionApplication $ FunctionApplication Nothing fn args'
            mkRawExpr = ERawExpression Nothing
            mkList xs =
              mkFnApp (mkId "[") $ intersperse (mkRawExpr ",") xs <> [mkId "]"] -- This is VERY hacky, but it'll work without too much effort for now.
         in ( [mkMacroImport "Axel" preludeMacros | not $ isPrelude filePath'] <> -- We can't import the Axel prelude if we're actually compiling it.
              [ mkQualImport "Prelude" "GHCPrelude" -- (in case `-XNoImplicitPrelude` is enabled)
              , mkQualImport "Axel.Parse.AST" "AST"
              , mkQualImport "Axel.Sourcemap" "SM"
              ]
            , [ mkModuleDecl scaffoldModuleName
              , mkQualImport "Prelude" "GHCPrelude" -- (in case `-XNoImplicitPrelude` is enabled)
              , mkQualImport macroDefAndEnvModuleName macroDefAndEnvModuleName
              , mkQualImport "Axel.Parse.AST" "AST"
              , mkQualImport "Axel.Sourcemap" "SM"
              , mkTySig "main" $ mkFnApp (mkId "GHCPrelude.IO") [mkId "()"]
              , mkFnDef "main" [] $
                mkFnApp
                  (mkId "(GHCPrelude.>>=)")
                  [ mkFnApp
                      (mkQualId
                         macroDefAndEnvModuleName
                         "main_AXEL_AUTOGENERATED_FUNCTION_DEFINITION")
                      [ mkList
                          (map
                             (unsafeNormalize normalizeExpression .
                              quoteSMExpression)
                             args)
                      ]
                  , mkFnApp
                      (mkId "(GHCPrelude..)")
                      [ mkId "GHCPrelude.putStrLn"
                      , mkFnApp
                          (mkId "(GHCPrelude..)")
                          [ mkId "GHCPrelude.unlines"
                          , mkFnApp (mkId "GHCPrelude.map") [mkId "AST.toAxel'"]
                          ]
                      ]
                  ]
              ])
  auxEnv <- Sem.get @[SMStatement]
  -- TODO If the file being transpiled has pragmas but no explicit module declaration,
  --      they will be erroneously included *after* the module declaration.
  --      Should we just require Axel files to have module declarations, or is there a
  --      less intrusive alternate solution?
  let macroDefAndEnv =
        let moduleDecl = SModuleDeclaration Nothing macroDefAndEnvModuleName
            programStmts =
              replaceModuleDecl moduleDecl $ insertImports header $ auxEnv <>
              footer
         in finalizeProgram programStmts
  pure $
    uncurry
      ((,) `on` toHaskell . statementsToProgram)
      (scaffold, macroDefAndEnv)
  where
    replaceModuleDecl newModuleDecl stmts =
      if any (is _SModuleDeclaration) stmts
        then map
               (\case
                  SModuleDeclaration _ _ -> newModuleDecl
                  x -> x)
               stmts
        else newModuleDecl : stmts

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
    (Parse.Symbol Nothing (T.unpack macroName) : map (Nothing <$) args)

withExpansionId ::
     SM.Expression
  -> Sem.Sem (Sem.Reader ExpansionId ': effs) a
  -> Sem.Sem effs a
withExpansionId originalCall x =
  let expansionId = showText $ abs $ hash originalCall -- We take the absolute value so that folder names don't start with dashes
                                                       -- (it looks weird, even though it's not technically wrong).
                                                       -- In theory, this allows for collisions, but the chances are negligibly small(?).
   in Sem.runReader (ExpansionId expansionId) x

expandMacroApplication ::
     (Sem.Members '[ Sem.Error Error, Effs.FileSystem, Effs.Ghci, Effs.Log, Effs.Process, Sem.Reader ExpansionId, Sem.Reader Ghcid.Ghci, Sem.Reader FilePath, Sem.State [SMStatement]] effs)
  => SourceMetadata
  -> Identifier
  -> [SM.Expression]
  -> Sem.Sem effs [SM.Expression]
expandMacroApplication originalAnn macroName args = do
  logStrLn $ "Expanding: " <> toAxel (losslyReconstructMacroCall macroName args)
  filePath' <- Sem.ask @FilePath
  macroProgram <- generateMacroProgram filePath' macroName args
  (tempFilePath, newSource) <-
    uncurry (evalMacro originalAnn macroName args) macroProgram
  logStrLn $ "Result: " <> newSource <> "\n\n"
  parseMultiple (Just tempFilePath) newSource

isMacroDefinitionStatement :: Statement ann -> Bool
isMacroDefinitionStatement (SMacroDefinition _) = True
isMacroDefinitionStatement _ = False

evalMacro ::
     forall effs.
     (Sem.Members '[ Sem.Error Error, Effs.FileSystem, Effs.Ghci, Effs.Process, Sem.Reader Ghcid.Ghci, Sem.Reader FilePath, Sem.Reader ExpansionId] effs)
  => SourceMetadata
  -> Identifier
  -> [SM.Expression]
  -> SM.Output
  -> SM.Output
  -> Sem.Sem effs (FilePath, Text)
evalMacro originalCallAnn macroName args scaffoldProgram macroDefAndEnvProgram = do
  macroDefAndEnvModuleName <- Sem.asks mkMacroDefAndEnvModuleName
  scaffoldModuleName <- Sem.asks mkScaffoldModuleName
  tempDir <- getTempDirectory
  let macroDefAndEnvFileName =
        tempDir </> FilePath macroDefAndEnvModuleName <.> "hs"
  let scaffoldFileName = tempDir </> FilePath scaffoldModuleName <.> "hs"
  let resultFile = tempDir </> FilePath "result.axel"
  FS.writeFile scaffoldFileName scaffold
  FS.writeFile macroDefAndEnvFileName macroDefAndEnv
  let moduleInfo =
        M.fromList $
        map
          (_1 %~ flip replaceExtension "axel")
          [ (scaffoldFileName, (scaffoldModuleName, Just scaffoldProgram))
          , ( macroDefAndEnvFileName
            , (macroDefAndEnvModuleName, Just macroDefAndEnvProgram))
          ]
  ghci <- Sem.ask @Ghcid.Ghci
  loadResult <- Ghci.addFiles ghci [scaffoldFileName, macroDefAndEnvFileName]
  if any ("Ok, " `T.isPrefixOf`) loadResult
    then do
      result <- T.unlines <$> Ghci.exec ghci (scaffoldModuleName <> ".main")
      FS.writeFile resultFile $
        generateExpansionRecord
          originalCallAnn
          macroName
          args
          result
          scaffoldFileName
          macroDefAndEnvFileName
      if any ("*** Exception:" `T.isPrefixOf`) (T.lines result)
        then throwMacroError result
        else pure (resultFile, result)
    else throwMacroError (processErrors moduleInfo $ T.unlines loadResult)
  where
    macroDefAndEnv = SM.raw macroDefAndEnvProgram
    scaffold = SM.raw scaffoldProgram
    getTempDirectory = do
      ExpansionId expansionId <- Sem.ask @ExpansionId
      let dirName = FilePath "axelTemp" </> FilePath expansionId
      FS.createDirectoryIfMissing True dirName
      pure dirName
    throwMacroError msg = do
      originalFilePath <- Sem.ask @FilePath
      Sem.throw $
        MacroError
          originalFilePath
          (losslyReconstructMacroCall macroName args)
          msg

generateExpansionRecord ::
     SourceMetadata
  -> Identifier
  -> [SM.Expression]
  -> Text
  -> FilePath
  -> FilePath
  -> Text
generateExpansionRecord originalAnn macroName args result scaffoldFilePath macroDefAndEnvFilePath =
  T.unlines
    [ result
    , "-- This file is an autogenerated record of a macro call and expansion."
    , "-- It is (likely) not a valid Axel program, so you probably don't want to run it directly."
    , ""
    , "-- The beginning of this file contains the result of the macro invocation at " <>
      locationHint <>
      ":"
    , toAxel (losslyReconstructMacroCall macroName args)
    , ""
    , "-- The macro call itself is transpiled in " <>
      op FilePath (takeFileName scaffoldFilePath) <>
      "."
    , ""
    , "-- To see the (transpiled) modules, definitions, extensions, etc. visible during the expansion, check " <>
      op FilePath (takeFileName macroDefAndEnvFilePath) <>
      "."
    ]
  where
    locationHint =
      case originalAnn of
        Just x -> renderSourcePosition x
        Nothing -> "<unknown>"
