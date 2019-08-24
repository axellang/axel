{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Normalize where

import Axel.AST
  ( CaseBlock(CaseBlock)
  , DataDeclaration(DataDeclaration)
  , Expression(ECaseBlock, EEmptySExpression, EFunctionApplication,
           EIdentifier, EIfBlock, ELambda, ELetBlock, ELiteral,
           ERawExpression, ERecordDefinition, ERecordType)
  , FunctionApplication(FunctionApplication)
  , FunctionDefinition(FunctionDefinition)
  , Identifier
  , IfBlock(IfBlock)
  , Import(ImportItem, ImportType)
  , ImportSpecification(ImportAll, ImportOnly)
  , Lambda(Lambda)
  , LetBlock(LetBlock)
  , Literal(LChar, LInt, LString)
  , MacroDefinition(MacroDefinition)
  , MacroImport(MacroImport)
  , NewtypeDeclaration(NewtypeDeclaration)
  , Pragma(Pragma)
  , QualifiedImport(QualifiedImport)
  , RecordDefinition(RecordDefinition)
  , RecordType(RecordType)
  , RestrictedImport(RestrictedImport)
  , SMStatement
  , Statement(SDataDeclaration, SFunctionDefinition, SMacroDefinition,
          SMacroImport, SModuleDeclaration, SNewtypeDeclaration, SPragma,
          SQualifiedImport, SRawStatement, SRestrictedImport, STopLevel,
          STypeSignature, STypeSynonym, STypeclassDefinition,
          STypeclassInstance, SUnrestrictedImport)
  , TopLevel(TopLevel)
  , TypeDefinition(ProperType, TypeConstructor)
  , TypeSignature(TypeSignature)
  , TypeSynonym(TypeSynonym)
  , TypeclassDefinition(TypeclassDefinition)
  , TypeclassInstance(TypeclassInstance)
  )
import Axel.Eff.Error (Error(NormalizeError), unsafeRunError)
import qualified Axel.Parse.AST as Parse
  ( Expression(LiteralChar, LiteralInt, LiteralString, SExpression,
           Symbol)
  )
import qualified Axel.Sourcemap as SM (Expression)

import Control.Monad.Freer (Eff, Member, Members)
import qualified Control.Monad.Freer as Effs (run)
import Control.Monad.Freer.Error (throwError)
import qualified Control.Monad.Freer.Error as Effs (Error)
import Control.Monad.Freer.Reader (ask, local, runReader)
import qualified Control.Monad.Freer.Reader as Effs (Reader)

type ExprCtxt = [SM.Expression]

pushCtxt ::
     (Member (Effs.Reader [SM.Expression]) effs)
  => SM.Expression
  -> Eff effs a
  -> Eff effs a
pushCtxt newCtxt = local (newCtxt :)

withExprCtxt :: Eff (Effs.Reader [SM.Expression] ': effs) a -> Eff effs a
withExprCtxt = runReader []

throwNormalizeError ::
     (Members '[ Effs.Error Error, Effs.Reader FilePath, Effs.Reader ExprCtxt] effs)
  => String
  -> Eff effs a
throwNormalizeError msg = do
  filePath <- ask @FilePath
  exprCtxt <- ask @ExprCtxt
  throwError @Error $ NormalizeError filePath msg exprCtxt

normalizeExpression ::
     (Members '[ Effs.Error Error, Effs.Reader FilePath, Effs.Reader ExprCtxt] effs)
  => SM.Expression
  -> Eff effs (Expression (Maybe SM.Expression))
normalizeExpression expr@(Parse.LiteralChar _ char) =
  pure $ ELiteral (LChar (Just expr) char)
normalizeExpression expr@(Parse.LiteralInt _ int) =
  pure $ ELiteral (LInt (Just expr) int)
normalizeExpression expr@(Parse.LiteralString _ string) =
  pure $ ELiteral (LString (Just expr) string)
normalizeExpression expr@(Parse.SExpression _ items) =
  pushCtxt expr $
  case items of
    Parse.Symbol _ "case":var:cases ->
      let normalizedCases =
            traverse
              (\case
                 Parse.SExpression _ [pat, body] ->
                   (,) <$> normalizeExpression pat <*> normalizeExpression body
                 x -> pushCtxt x $ throwNormalizeError "Invalid case!")
              cases
       in ECaseBlock <$>
          (CaseBlock (Just expr) <$> normalizeExpression var <*> normalizedCases)
    [Parse.Symbol _ "\\", Parse.SExpression _ args, body] -- TODO Fail if num args /= 2
     ->
      let normalizedArguments = traverse normalizeExpression args
       in ELambda <$>
          (Lambda (Just expr) <$> normalizedArguments <*>
           normalizeExpression body)
    [Parse.Symbol _ "if", cond, ifTrue, ifFalse] -- TODO Fail if num args /= 3
     ->
      EIfBlock <$>
      (IfBlock (Just expr) <$> normalizeExpression cond <*>
       normalizeExpression ifTrue <*>
       normalizeExpression ifFalse)
    [Parse.Symbol _ "let", Parse.SExpression _ bindings, body] -- TODO Fail if num args /= 2
     ->
      let normalizedBindings =
            traverse
              (\case
                 Parse.SExpression _ [name, value] ->
                   (,) <$> normalizeExpression name <*>
                   normalizeExpression value
                 x -> pushCtxt x $ throwNormalizeError "Invalid pattern!")
              bindings
       in ELetBlock <$>
          (LetBlock (Just expr) <$> normalizedBindings <*>
           normalizeExpression body)
    [Parse.Symbol _ "raw", rawSource] -- TODO Fail if num args /= 1
     ->
      let normalizedRawSource =
            case rawSource of
              Parse.LiteralString _ x -> pure x
              x ->
                pushCtxt x $
                throwNormalizeError
                  "`raw` takes strings representing the code to inject directly."
       in ERawExpression (Just expr) <$> normalizedRawSource
    Parse.Symbol _ "record":bindings ->
      let normalizedBindings =
            traverse
              (\x ->
                 normalizeExpression x >>= \case
                   EFunctionApplication (FunctionApplication _ (EIdentifier _ field) [val]) ->
                     pure (field, val)
                   _ ->
                     pushCtxt x $ throwNormalizeError "Invalid field binding!")
              bindings
       in ERecordDefinition <$>
          (RecordDefinition (Just expr) <$> normalizedBindings)
    Parse.Symbol _ "recordType":fields ->
      let normalizedFields =
            traverse
              (\x ->
                 normalizeExpression x >>= \case
                   EFunctionApplication (FunctionApplication _ (EIdentifier _ field) [ty]) ->
                     pure (field, ty)
                   _ ->
                     pushCtxt x $
                     throwNormalizeError "Invalid field definition!")
              fields
       in ERecordType <$> (RecordType (Just expr) <$> normalizedFields)
    fn:args ->
      EFunctionApplication <$>
      (FunctionApplication (Just expr) <$> normalizeExpression fn <*>
       traverse normalizeExpression args)
    [] -> pure $ EEmptySExpression (Just expr)
normalizeExpression expr@(Parse.Symbol _ symbol) =
  pure $ EIdentifier (Just expr) symbol

normalizeFunctionDefinition ::
     (Members '[ Effs.Error Error, Effs.Reader FilePath, Effs.Reader ExprCtxt] effs)
  => SM.Expression
  -> Identifier
  -> [SM.Expression]
  -> SM.Expression
  -> [SM.Expression]
  -> Eff effs (FunctionDefinition (Maybe SM.Expression))
normalizeFunctionDefinition expr fnName arguments body whereDefs =
  FunctionDefinition (Just expr) fnName <$>
  traverse normalizeExpression arguments <*>
  normalizeExpression body <*>
  traverse
    (\x ->
       normalizeStatement x >>= \case
         SFunctionDefinition funDef -> pure funDef
         _ -> pushCtxt x $ throwNormalizeError "Invalid where binding!")
    whereDefs

normalizeStatement ::
     (Members '[ Effs.Error Error, Effs.Reader FilePath, Effs.Reader ExprCtxt] effs)
  => SM.Expression
  -> Eff effs SMStatement
normalizeStatement expr@(Parse.SExpression _ items) =
  pushCtxt expr $
  case items of
    [Parse.Symbol _ "::", Parse.Symbol _ fnName, typeDef] ->
      STypeSignature <$>
      (TypeSignature (Just expr) fnName <$> normalizeExpression typeDef)
    Parse.Symbol _ "=":Parse.Symbol _ fnName:Parse.SExpression _ arguments:body:whereDefs ->
      SFunctionDefinition <$>
      normalizeFunctionDefinition expr fnName arguments body whereDefs
    Parse.Symbol _ "begin":stmts ->
      let normalizedStmts = traverse normalizeStatement stmts
       in STopLevel . TopLevel (Just expr) <$> normalizedStmts
    Parse.Symbol _ "class":classConstraints:className:sigs ->
      let normalizedConstraints =
            normalizeExpression classConstraints >>= \case
              EFunctionApplication (FunctionApplication _ (EIdentifier _ "list") constraints) ->
                pure constraints
              _ ->
                pushCtxt classConstraints $
                throwNormalizeError "Invalid constraints!"
          normalizedSigs =
            traverse
              (\x ->
                 normalizeStatement x >>= \case
                   STypeSignature tySig -> pure tySig
                   _ ->
                     pushCtxt x $ throwNormalizeError "Invalid type signature!")
              sigs
       in STypeclassDefinition <$>
          (TypeclassDefinition (Just expr) <$> normalizeExpression className <*>
           normalizedConstraints <*>
           normalizedSigs)
    Parse.Symbol _ "data":typeDef:constructors ->
      let normalizedConstructors =
            traverse
              (\x ->
                 normalizeExpression x >>= \case
                   EFunctionApplication functionApplication ->
                     pure functionApplication
                   _ ->
                     pushCtxt x $
                     throwNormalizeError "Invalid type constructor!")
              constructors
       in normalizeExpression typeDef >>= \case
            EFunctionApplication typeConstructor ->
              SDataDeclaration <$>
              (DataDeclaration
                 (Just expr)
                 (TypeConstructor (Just expr) typeConstructor) <$>
               normalizedConstructors)
            EIdentifier _ properType ->
              SDataDeclaration <$>
              (DataDeclaration (Just expr) (ProperType (Just expr) properType) <$>
               normalizedConstructors)
            _ -> pushCtxt typeDef $ throwNormalizeError "Invalid type!"
    [Parse.Symbol _ "newtype", typeDef, constructor] ->
      let normalizedConstructor =
            normalizeExpression constructor >>= \case
              EFunctionApplication funApp -> pure funApp
              _ ->
                pushCtxt constructor $
                throwNormalizeError "Invalid type constructor!"
       in normalizeExpression typeDef >>= \case
            EFunctionApplication typeConstructor ->
              SNewtypeDeclaration <$>
              (NewtypeDeclaration
                 (Just expr)
                 (TypeConstructor (Just expr) typeConstructor) <$>
               normalizedConstructor)
            EIdentifier _ properType ->
              SNewtypeDeclaration <$>
              (NewtypeDeclaration
                 (Just expr)
                 (ProperType (Just expr) properType) <$>
               normalizedConstructor)
            _ -> pushCtxt typeDef $ throwNormalizeError "Invalid type!"
    [Parse.Symbol _ "import", Parse.Symbol _ moduleName, importSpec] ->
      SRestrictedImport <$>
      (RestrictedImport (Just expr) moduleName <$>
       normalizeImportSpec importSpec)
    [Parse.Symbol _ "importm", Parse.Symbol _ moduleName, macroImportSpec] ->
      SMacroImport . MacroImport (Just expr) moduleName <$>
      normalizeMacroImportSpec macroImportSpec
    [Parse.Symbol _ "importq", Parse.Symbol _ moduleName, Parse.Symbol _ alias, importSpec] ->
      SQualifiedImport <$>
      (QualifiedImport (Just expr) moduleName alias <$>
       normalizeImportSpec importSpec)
    [Parse.Symbol _ "importUnrestricted", Parse.Symbol _ moduleName] ->
      pure $ SUnrestrictedImport (Just expr) moduleName
    Parse.Symbol _ "instance":instanceName:defs ->
      let normalizedDefs =
            traverse
              (\x ->
                 normalizeStatement x >>= \case
                   SFunctionDefinition fnDef -> pure fnDef
                   _ -> pushCtxt x $ throwNormalizeError "Invalid definition!")
              defs
       in STypeclassInstance <$>
          (TypeclassInstance (Just expr) <$> normalizeExpression instanceName <*>
           normalizedDefs)
    [Parse.Symbol _ "pragma", Parse.LiteralString _ pragma] ->
      pure $ SPragma (Pragma (Just expr) pragma)
    Parse.Symbol _ "=macro":Parse.Symbol _ macroName:Parse.SExpression _ arguments:body:whereBindings ->
      SMacroDefinition . MacroDefinition (Just expr) <$>
      normalizeFunctionDefinition expr macroName arguments body whereBindings
    [Parse.Symbol _ "module", Parse.Symbol _ moduleName] ->
      pure $ SModuleDeclaration (Just expr) moduleName
    [Parse.Symbol _ "raw", rawSource] ->
      let normalizedRawSource =
            case rawSource of
              Parse.LiteralString _ x -> pure x
              x ->
                pushCtxt x $
                throwNormalizeError
                  "`raw` takes strings representing the code to inject directly."
       in SRawStatement (Just expr) <$> normalizedRawSource
    [Parse.Symbol _ "type", alias, def] ->
      let normalizedAlias = normalizeExpression alias
          normalizedDef = normalizeExpression def
       in STypeSynonym <$>
          (TypeSynonym (Just expr) <$> normalizedAlias <*> normalizedDef)
    _ -> throwNormalizeError "Invalid statement!"
  where
    normalizeMacroImportSpec importSpec =
      case importSpec of
        Parse.SExpression _ macroImportList ->
          traverse
            (\case
               Parse.Symbol _ import' -> pure import'
               x -> pushCtxt x $ throwNormalizeError "Invalid macro import!")
            macroImportList
        x ->
          pushCtxt x $ throwNormalizeError "Invalid macro import specification!"
    normalizeImportSpec importSpec =
      case importSpec of
        Parse.Symbol _ "all" -> pure $ ImportAll (Just expr)
        Parse.SExpression _ importList -> normalizeImportList importList
        x -> pushCtxt x $ throwNormalizeError "Invalid import specification!"
    normalizeImportList input =
      ImportOnly (Just expr) <$>
      traverse
        (\item ->
           pushCtxt item $
           case item of
             Parse.Symbol _ import' -> pure $ ImportItem (Just expr) import'
             Parse.SExpression _ (Parse.Symbol _ type':imports) ->
               let normalizedImports =
                     traverse
                       (\case
                          Parse.Symbol _ import' -> pure import'
                          x ->
                            pushCtxt x $ throwNormalizeError "Invalid import!")
                       imports
                in ImportType (Just expr) type' <$> normalizedImports
             _ -> throwNormalizeError "Invalid import!")
        input
normalizeStatement expr =
  pushCtxt expr $ throwNormalizeError "Invalid top-level form!"

unsafeNormalizeStatement :: SM.Expression -> SMStatement
unsafeNormalizeStatement =
  Effs.run .
  unsafeRunError @Error . runReader "" . withExprCtxt . normalizeStatement
