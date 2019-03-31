{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

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
import Axel.Error (Error(NormalizeError))
import qualified Axel.Parse as Parse
  ( Expression(LiteralChar, LiteralInt, LiteralString, SExpression,
           Symbol)
  )
import qualified Axel.Sourcemap as SM (Error, Expression)

import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Error (throwError)
import qualified Control.Monad.Freer.Error as Effs (Error)

normalizeExpression ::
     (Member (Effs.Error SM.Error) effs)
  => SM.Expression
  -> Eff effs (Expression SM.Expression)
normalizeExpression expr@(Parse.LiteralChar _ char) =
  pure $ ELiteral (LChar expr char)
normalizeExpression expr@(Parse.LiteralInt _ int) =
  pure $ ELiteral (LInt expr int)
normalizeExpression expr@(Parse.LiteralString _ string) =
  pure $ ELiteral (LString expr string)
normalizeExpression expr@(Parse.SExpression _ items) =
  case items of
    Parse.Symbol _ "case":var:cases ->
      let normalizedCases =
            traverse
              (\case
                 Parse.SExpression _ [pat, body] ->
                   (,) <$> normalizeExpression pat <*> normalizeExpression body
                 x -> throwError $ NormalizeError "Invalid case!" [x, expr])
              cases
       in ECaseBlock <$>
          (CaseBlock expr <$> normalizeExpression var <*> normalizedCases)
    [Parse.Symbol _ "\\", Parse.SExpression _ args, body] ->
      let normalizedArguments = traverse normalizeExpression args
       in ELambda <$>
          (Lambda expr <$> normalizedArguments <*> normalizeExpression body)
    [Parse.Symbol _ "if", cond, ifTrue, ifFalse] ->
      EIfBlock <$>
      (IfBlock expr <$> normalizeExpression cond <*> normalizeExpression ifTrue <*>
       normalizeExpression ifFalse)
    [Parse.Symbol _ "let", Parse.SExpression _ bindings, body] ->
      let normalizedBindings =
            traverse
              (\case
                 Parse.SExpression _ [name, value] ->
                   (,) <$> normalizeExpression name <*>
                   normalizeExpression value
                 x -> throwError $ NormalizeError "Invalid pattern!" [x, expr])
              bindings
       in ELetBlock <$>
          (LetBlock expr <$> normalizedBindings <*> normalizeExpression body)
    [Parse.Symbol _ "raw", rawSource] ->
      let normalizedRawSource =
            case rawSource of
              Parse.LiteralString _ x -> pure x
              x ->
                throwError $
                NormalizeError
                  "`raw` takes strings representing the code to inject directly."
                  [x, expr]
       in ERawExpression expr <$> normalizedRawSource
    Parse.Symbol _ "record":bindings ->
      let normalizedBindings =
            traverse
              (\x ->
                 normalizeExpression x >>= \case
                   EFunctionApplication (FunctionApplication _ (EIdentifier _ field) [val]) ->
                     pure (field, val)
                   _ ->
                     throwError $
                     NormalizeError "Invalid field binding!" [x, expr])
              bindings
       in ERecordDefinition <$> (RecordDefinition expr <$> normalizedBindings)
    Parse.Symbol _ "recordType":fields ->
      let normalizedFields =
            traverse
              (\x ->
                 normalizeExpression x >>= \case
                   EFunctionApplication (FunctionApplication _ (EIdentifier _ field) [ty]) ->
                     pure (field, ty)
                   _ ->
                     throwError $
                     NormalizeError "Invalid field definition!" [x, expr])
              fields
       in ERecordType <$> (RecordType expr <$> normalizedFields)
    fn:args ->
      EFunctionApplication <$>
      (FunctionApplication expr <$> normalizeExpression fn <*>
       traverse normalizeExpression args)
    [] -> pure $ EEmptySExpression expr
normalizeExpression expr@(Parse.Symbol _ symbol) =
  pure $ EIdentifier expr symbol

normalizeFunctionDefinition ::
     (Member (Effs.Error SM.Error) effs)
  => SM.Expression
  -> Identifier
  -> [SM.Expression]
  -> SM.Expression
  -> [SM.Expression]
  -> Eff effs (FunctionDefinition SM.Expression)
normalizeFunctionDefinition expr fnName arguments body whereDefs =
  FunctionDefinition expr fnName <$> traverse normalizeExpression arguments <*>
  normalizeExpression body <*>
  traverse
    (\x ->
       normalizeStatement x >>= \case
         SFunctionDefinition funDef -> pure funDef
         _ -> throwError $ NormalizeError "Invalid where binding!" [x, expr])
    whereDefs

normalizeStatement ::
     (Member (Effs.Error SM.Error) effs)
  => SM.Expression
  -> Eff effs (Statement SM.Expression)
normalizeStatement expr@(Parse.SExpression _ items) =
  case items of
    [Parse.Symbol _ "::", Parse.Symbol _ fnName, typeDef] ->
      STypeSignature <$>
      (TypeSignature expr fnName <$> normalizeExpression typeDef)
    Parse.Symbol _ "=":Parse.Symbol _ fnName:Parse.SExpression _ arguments:body:whereDefs ->
      SFunctionDefinition <$>
      normalizeFunctionDefinition expr fnName arguments body whereDefs
    Parse.Symbol _ "begin":stmts ->
      let normalizedStmts = traverse normalizeStatement stmts
       in STopLevel . TopLevel expr <$> normalizedStmts
    Parse.Symbol _ "class":classConstraints:className:sigs ->
      let normalizedConstraints =
            normalizeExpression classConstraints >>= \case
              EFunctionApplication (FunctionApplication _ (EIdentifier _ "list") constraints) ->
                pure constraints
              _ ->
                throwError $
                NormalizeError "Invalid constraints!" [classConstraints, expr]
          normalizedSigs =
            traverse
              (\x ->
                 normalizeStatement x >>= \case
                   STypeSignature tySig -> pure tySig
                   _ ->
                     throwError $
                     NormalizeError "Invalid type signature!" [x, expr])
              sigs
       in STypeclassDefinition <$>
          (TypeclassDefinition expr <$> normalizeExpression className <*>
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
                     throwError $
                     NormalizeError "Invalid type constructor!" [x, expr])
              constructors
       in normalizeExpression typeDef >>= \case
            EFunctionApplication typeConstructor ->
              SDataDeclaration <$>
              (DataDeclaration expr (TypeConstructor expr typeConstructor) <$>
               normalizedConstructors)
            EIdentifier _ properType ->
              SDataDeclaration <$>
              (DataDeclaration expr (ProperType expr properType) <$>
               normalizedConstructors)
            _ -> throwError $ NormalizeError "Invalid type!" [typeDef, expr]
    [Parse.Symbol _ "newtype", typeDef, constructor] ->
      let normalizedConstructor =
            normalizeExpression constructor >>= \case
              EFunctionApplication funApp -> pure funApp
              _ ->
                throwError $
                NormalizeError "Invalid type constructor!" [constructor, expr]
       in normalizeExpression typeDef >>= \case
            EFunctionApplication typeConstructor ->
              SNewtypeDeclaration <$>
              (NewtypeDeclaration expr (TypeConstructor expr typeConstructor) <$>
               normalizedConstructor)
            EIdentifier _ properType ->
              SNewtypeDeclaration <$>
              (NewtypeDeclaration expr (ProperType expr properType) <$>
               normalizedConstructor)
            _ -> throwError $ NormalizeError "Invalid type!" [typeDef, expr]
    [Parse.Symbol _ "import", Parse.Symbol _ moduleName, importSpec] ->
      SRestrictedImport <$>
      (RestrictedImport expr moduleName <$> normalizeImportSpec importSpec)
    [Parse.Symbol _ "importm", Parse.Symbol _ moduleName, macroImportSpec] ->
      SMacroImport . MacroImport expr moduleName <$>
      normalizeMacroImportSpec expr macroImportSpec
    [Parse.Symbol _ "importq", Parse.Symbol _ moduleName, Parse.Symbol _ alias, importSpec] ->
      SQualifiedImport <$>
      (QualifiedImport expr moduleName alias <$> normalizeImportSpec importSpec)
    [Parse.Symbol _ "importUnrestricted", Parse.Symbol _ moduleName] ->
      pure $ SUnrestrictedImport expr moduleName
    Parse.Symbol _ "instance":instanceName:defs ->
      let normalizedDefs =
            traverse
              (\x ->
                 normalizeStatement x >>= \case
                   SFunctionDefinition fnDef -> pure fnDef
                   _ ->
                     throwError $ NormalizeError "Invalid definition!" [x, expr])
              defs
       in STypeclassInstance <$>
          (TypeclassInstance expr <$> normalizeExpression instanceName <*>
           normalizedDefs)
    [Parse.Symbol _ "pragma", Parse.LiteralString _ pragma] ->
      pure $ SPragma (Pragma expr pragma)
    Parse.Symbol _ "=macro":Parse.Symbol _ macroName:Parse.SExpression _ arguments:body:whereBindings ->
      SMacroDefinition . MacroDefinition expr <$>
      normalizeFunctionDefinition expr macroName arguments body whereBindings
    [Parse.Symbol _ "module", Parse.Symbol _ moduleName] ->
      pure $ SModuleDeclaration expr moduleName
    [Parse.Symbol _ "raw", rawSource] ->
      let normalizedRawSource =
            case rawSource of
              Parse.LiteralString _ x -> pure x
              x ->
                throwError $
                NormalizeError
                  "`raw` takes strings representing the code to inject directly."
                  [x, expr]
       in SRawStatement expr <$> normalizedRawSource
    [Parse.Symbol _ "type", alias, def] ->
      let normalizedAlias = normalizeExpression alias
          normalizedDef = normalizeExpression def
       in STypeSynonym <$>
          (TypeSynonym expr <$> normalizedAlias <*> normalizedDef)
    _ -> throwError $ NormalizeError "Invalid statement!" [expr]
  where
    normalizeMacroImportSpec ctxt importSpec =
      case importSpec of
        Parse.SExpression _ macroImportList ->
          traverse
            (\case
               Parse.Symbol _ import' -> pure import'
               x ->
                 throwError $ NormalizeError "Invalid macro import!" [x, ctxt])
            macroImportList
        x ->
          throwError $
          NormalizeError "Invalid macro import specification!" [x, ctxt]
    normalizeImportSpec importSpec =
      case importSpec of
        Parse.Symbol _ "all" -> pure $ ImportAll expr
        Parse.SExpression _ importList -> normalizeImportList importList
        x ->
          throwError $ NormalizeError "Invalid import specification!" [x, expr]
    normalizeImportList input =
      ImportOnly expr <$>
      traverse
        (\item ->
           case item of
             Parse.Symbol _ import' -> pure $ ImportItem expr import'
             Parse.SExpression _ (Parse.Symbol _ type':imports) ->
               let normalizedImports =
                     traverse
                       (\case
                          Parse.Symbol _ import' -> pure import'
                          x ->
                            throwError $
                            NormalizeError "Invalid import!" [x, item, expr])
                       imports
                in ImportType expr type' <$> normalizedImports
             x -> throwError $ NormalizeError "Invalid import!" [x, item, expr])
        input
normalizeStatement expr =
  throwError $ NormalizeError "Invalid top-level form!" [expr]
