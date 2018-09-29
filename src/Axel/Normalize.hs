{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Axel.Normalize where

import Axel.AST
  ( CaseBlock(CaseBlock)
  , DataDeclaration(DataDeclaration)
  , Expression(ECaseBlock, EEmptySExpression, EFunctionApplication,
           EIdentifier, ELambda, ELetBlock, ELiteral, ERawExpression,
           ERecordDefinition, ERecordType)
  , FunctionApplication(FunctionApplication)
  , FunctionDefinition(FunctionDefinition)
  , Identifier
  , Import(ImportItem, ImportType)
  , ImportSpecification(ImportAll, ImportOnly)
  , Lambda(Lambda)
  , LetBlock(LetBlock)
  , Literal(LChar, LInt, LString)
  , MacroDefinition(MacroDefinition)
  , NewtypeDeclaration(NewtypeDeclaration)
  , Pragma(Pragma)
  , QualifiedImport(QualifiedImport)
  , RecordDefinition(RecordDefinition)
  , RecordType(RecordType)
  , RestrictedImport(RestrictedImport)
  , Statement(SDataDeclaration, SFunctionDefinition, SMacroDefinition,
          SModuleDeclaration, SNewtypeDeclaration, SPragma, SQualifiedImport,
          SRawStatement, SRestrictedImport, STopLevel, STypeSignature,
          STypeSynonym, STypeclassDefinition, STypeclassInstance,
          SUnrestrictedImport)
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

import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Error (throwError)
import qualified Control.Monad.Freer.Error as Effs (Error)

normalizeExpression ::
     (Member (Effs.Error Error) effs) => Parse.Expression -> Eff effs Expression
normalizeExpression (Parse.LiteralChar char) = pure $ ELiteral (LChar char)
normalizeExpression (Parse.LiteralInt int) = pure $ ELiteral (LInt int)
normalizeExpression (Parse.LiteralString string) =
  pure $ ELiteral (LString string)
normalizeExpression expr@(Parse.SExpression items) =
  case items of
    Parse.Symbol "case":var:cases ->
      let normalizedCases =
            traverse
              (\case
                 Parse.SExpression [pat, body] ->
                   (,) <$> normalizeExpression pat <*> normalizeExpression body
                 x -> throwError $ NormalizeError "Invalid case!" [x, expr])
              cases
       in ECaseBlock <$>
          (CaseBlock <$> normalizeExpression var <*> normalizedCases)
    [Parse.Symbol "\\", Parse.SExpression args, body] ->
      let normalizedArguments = traverse normalizeExpression args
       in ELambda <$>
          (Lambda <$> normalizedArguments <*> normalizeExpression body)
    [Parse.Symbol "let", Parse.SExpression bindings, body] ->
      let normalizedBindings =
            traverse
              (\case
                 Parse.SExpression [name, value] ->
                   (,) <$> normalizeExpression name <*>
                   normalizeExpression value
                 x -> throwError $ NormalizeError "Invalid pattern!" [x, expr])
              bindings
       in ELetBlock <$>
          (LetBlock <$> normalizedBindings <*> normalizeExpression body)
    [Parse.Symbol "raw", rawSource] ->
      let normalizedRawSource =
            case rawSource of
              Parse.LiteralString x -> pure x
              x ->
                throwError $
                NormalizeError
                  "`raw` takes strings representing the code to inject directly."
                  [x, expr]
       in ERawExpression <$> normalizedRawSource
    Parse.Symbol "record":bindings ->
      let normalizedBindings =
            traverse
              (\x ->
                 normalizeExpression x >>= \case
                   EFunctionApplication (FunctionApplication (EIdentifier field) [val]) ->
                     pure (field, val)
                   _ ->
                     throwError $
                     NormalizeError "Invalid field binding!" [x, expr])
              bindings
       in ERecordDefinition <$> (RecordDefinition <$> normalizedBindings)
    Parse.Symbol "recordType":fields ->
      let normalizedFields =
            traverse
              (\x ->
                 normalizeExpression x >>= \case
                   EFunctionApplication (FunctionApplication (EIdentifier field) [ty]) ->
                     pure (field, ty)
                   _ ->
                     throwError $
                     NormalizeError "Invalid field definition!" [x, expr])
              fields
       in ERecordType <$> (RecordType <$> normalizedFields)
    fn:args ->
      EFunctionApplication <$>
      (FunctionApplication <$> normalizeExpression fn <*>
       traverse normalizeExpression args)
    [] -> pure EEmptySExpression
normalizeExpression (Parse.Symbol symbol) = pure $ EIdentifier symbol

normalizeFunctionDefinition ::
     (Member (Effs.Error Error) effs)
  => Identifier
  -> [Parse.Expression]
  -> Parse.Expression
  -> Eff effs FunctionDefinition
normalizeFunctionDefinition fnName arguments body =
  FunctionDefinition fnName <$> traverse normalizeExpression arguments <*>
  normalizeExpression body

normalizeStatement ::
     (Member (Effs.Error Error) effs) => Parse.Expression -> Eff effs Statement
normalizeStatement expr@(Parse.SExpression items) =
  case items of
    [Parse.Symbol "::", Parse.Symbol fnName, typeDef] ->
      STypeSignature <$> (TypeSignature fnName <$> normalizeExpression typeDef)
    [Parse.Symbol "=", Parse.Symbol fnName, Parse.SExpression arguments, body] ->
      SFunctionDefinition <$> normalizeFunctionDefinition fnName arguments body
    Parse.Symbol "begin":stmts ->
      let normalizedStmts = traverse normalizeStatement stmts
       in STopLevel . TopLevel <$> normalizedStmts
    Parse.Symbol "class":classConstraints:className:sigs ->
      let normalizedConstraints =
            normalizeExpression classConstraints >>= \case
              EFunctionApplication (FunctionApplication (EIdentifier "list") constraints) ->
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
          (TypeclassDefinition <$> normalizeExpression className <*>
           normalizedConstraints <*>
           normalizedSigs)
    Parse.Symbol "data":typeDef:constructors ->
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
              (DataDeclaration (TypeConstructor typeConstructor) <$>
               normalizedConstructors)
            EIdentifier properType ->
              SDataDeclaration <$>
              (DataDeclaration (ProperType properType) <$>
               normalizedConstructors)
            _ -> throwError $ NormalizeError "Invalid type!" [typeDef, expr]
    [Parse.Symbol "newtype", typeDef, constructor] ->
      let normalizedConstructor =
            normalizeExpression constructor >>= \case
              EFunctionApplication funApp -> pure funApp
              _ ->
                throwError $
                NormalizeError "Invalid type constructor!" [constructor, expr]
       in normalizeExpression typeDef >>= \case
            EFunctionApplication typeConstructor ->
              SNewtypeDeclaration <$>
              (NewtypeDeclaration (TypeConstructor typeConstructor) <$>
               normalizedConstructor)
            EIdentifier properType ->
              SNewtypeDeclaration <$>
              (NewtypeDeclaration (ProperType properType) <$>
               normalizedConstructor)
            _ -> throwError $ NormalizeError "Invalid type!" [typeDef, expr]
    [Parse.Symbol "import", Parse.Symbol moduleName, importSpec] ->
      SRestrictedImport <$>
      (RestrictedImport moduleName <$> normalizeImportSpec expr importSpec)
    [Parse.Symbol "importq", Parse.Symbol moduleName, Parse.Symbol alias, importSpec] ->
      SQualifiedImport <$>
      (QualifiedImport moduleName alias <$> normalizeImportSpec expr importSpec)
    [Parse.Symbol "importUnrestricted", Parse.Symbol moduleName] ->
      pure $ SUnrestrictedImport moduleName
    Parse.Symbol "instance":instanceName:defs ->
      let normalizedDefs =
            traverse
              (\x ->
                 normalizeStatement x >>= \case
                   SFunctionDefinition fnDef -> pure fnDef
                   _ ->
                     throwError $ NormalizeError "Invalid definition!" [x, expr])
              defs
       in STypeclassInstance <$>
          (TypeclassInstance <$> normalizeExpression instanceName <*>
           normalizedDefs)
    [Parse.Symbol "pragma", Parse.LiteralString pragma] ->
      pure $ SPragma (Pragma pragma)
    [Parse.Symbol "macro", Parse.Symbol macroName, Parse.SExpression arguments, body] ->
      SMacroDefinition . MacroDefinition <$>
      normalizeFunctionDefinition macroName arguments body
    [Parse.Symbol "module", Parse.Symbol moduleName] ->
      pure $ SModuleDeclaration moduleName
    [Parse.Symbol "raw", rawSource] ->
      let normalizedRawSource =
            case rawSource of
              Parse.LiteralString x -> pure x
              x ->
                throwError $
                NormalizeError
                  "`raw` takes strings representing the code to inject directly."
                  [x, expr]
       in SRawStatement <$> normalizedRawSource
    [Parse.Symbol "type", alias, def] ->
      let normalizedAlias = normalizeExpression alias
          normalizedDef = normalizeExpression def
       in STypeSynonym <$> (TypeSynonym <$> normalizedAlias <*> normalizedDef)
    _ -> throwError $ NormalizeError "Invalid top-level form!" [expr]
  where
    normalizeImportSpec ctxt importSpec =
      case importSpec of
        Parse.Symbol "all" -> pure ImportAll
        Parse.SExpression importList -> normalizeImportList ctxt importList
        x ->
          throwError $ NormalizeError "Invalid import specification!" [x, ctxt]
    normalizeImportList ctxt input =
      ImportOnly <$>
      traverse
        (\item ->
           case item of
             Parse.Symbol import' -> pure $ ImportItem import'
             Parse.SExpression (Parse.Symbol type':imports) ->
               let normalizedImports =
                     traverse
                       (\case
                          Parse.Symbol import' -> pure import'
                          x ->
                            throwError $
                            NormalizeError "Invalid import!" [x, item, ctxt])
                       imports
                in ImportType type' <$> normalizedImports
             x -> throwError $ NormalizeError "Invalid import!" [x, item, ctxt])
        input
normalizeStatement expr =
  throwError $ NormalizeError "Invalid top-level form!" [expr]
