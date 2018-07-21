{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Axel.Normalize where

import Axel.AST
  ( ArgumentList(ArgumentList)
  , CaseBlock(CaseBlock)
  , DataDeclaration(DataDeclaration)
  , Expression(ECaseBlock, EEmptySExpression, EFunctionApplication,
           EIdentifier, ELambda, ELetBlock, ELiteral)
  , FunctionApplication(FunctionApplication)
  , FunctionDefinition(FunctionDefinition)
  , Import(ImportItem, ImportType)
  , ImportList(ImportList)
  , Lambda(Lambda)
  , LanguagePragma(LanguagePragma)
  , LetBlock(LetBlock)
  , Literal(LChar, LInt, LString)
  , MacroDefinition(MacroDefinition)
  , QualifiedImport(QualifiedImport)
  , RestrictedImport(RestrictedImport)
  , Statement(SDataDeclaration, SFunctionDefinition, SLanguagePragma,
          SMacroDefinition, SModuleDeclaration, SQualifiedImport,
          SRestrictedImport, STopLevel, STypeSynonym, STypeclassInstance,
          SUnrestrictedImport)
  , TopLevel(TopLevel)
  , TypeDefinition(ProperType, TypeConstructor)
  , TypeSynonym(TypeSynonym)
  , TypeclassInstance(TypeclassInstance)
  )
import Axel.Error (Error(NormalizeError))
import qualified Axel.Parse as Parse
  ( Expression(LiteralChar, LiteralInt, LiteralString, SExpression,
           Symbol)
  )
import Axel.Quote (quoteParseExpression)

import Control.Monad.Except (MonadError, throwError)

normalizeExpression :: (MonadError Error m) => Parse.Expression -> m Expression
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
    [Parse.Symbol "fn", Parse.SExpression args, body] ->
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
    [Parse.Symbol "quote", expr'] -> pure $ quoteParseExpression expr'
    fn:args ->
      EFunctionApplication <$>
      (FunctionApplication <$> normalizeExpression fn <*>
       traverse normalizeExpression args)
    [] -> pure EEmptySExpression
normalizeExpression (Parse.Symbol symbol) = pure $ EIdentifier symbol

normalizeDefinitions ::
     (MonadError Error m)
  => Parse.Expression
  -> [Parse.Expression]
  -> m [(ArgumentList, Expression)]
normalizeDefinitions ctxt =
  traverse
    (\case
       Parse.SExpression [Parse.SExpression args, body] ->
         (,) <$> (ArgumentList <$> traverse normalizeExpression args) <*>
         normalizeExpression body
       x -> throwError $ NormalizeError "Invalid definition!" [x, ctxt])

normalizeStatement :: (MonadError Error m) => Parse.Expression -> m Statement
normalizeStatement expr@(Parse.SExpression items) =
  case items of
    Parse.Symbol "=":Parse.Symbol fnName:typeSig:defs ->
      normalizeExpression typeSig >>= \case
        EFunctionApplication normalizedTypeSig ->
          SFunctionDefinition <$>
          (FunctionDefinition fnName normalizedTypeSig <$>
           normalizeDefinitions expr defs)
        _ ->
          throwError $ NormalizeError "Invalid type signature!" [typeSig, expr]
    Parse.Symbol "begin":stmts ->
      let normalizedStmts = traverse normalizeStatement stmts
      in STopLevel . TopLevel <$> normalizedStmts
    [Parse.Symbol "data", typeDef, Parse.SExpression constructors] ->
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
             (DataDeclaration (ProperType properType) <$> normalizedConstructors)
           _ -> throwError $ NormalizeError "Invalid type!" [typeDef, expr]
    Parse.Symbol "defmacro":Parse.Symbol macroName:defs ->
      SMacroDefinition <$>
      (MacroDefinition macroName <$> normalizeDefinitions expr defs)
    [Parse.Symbol "import", Parse.Symbol moduleName, Parse.SExpression imports] ->
      SRestrictedImport <$>
      (RestrictedImport moduleName <$> normalizeImportList expr imports)
    [Parse.Symbol "importq", Parse.Symbol moduleName, Parse.Symbol alias, Parse.SExpression imports] ->
      SQualifiedImport <$>
      (QualifiedImport moduleName alias <$> normalizeImportList expr imports)
    [Parse.Symbol "importUnrestricted", Parse.Symbol moduleName] ->
      pure $ SUnrestrictedImport moduleName
    [Parse.Symbol "instance", instanceName, Parse.SExpression defs] ->
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
    [Parse.Symbol "language", Parse.Symbol languageName] ->
      pure $ SLanguagePragma (LanguagePragma languageName)
    [Parse.Symbol "module", Parse.Symbol moduleName] ->
      pure $ SModuleDeclaration moduleName
    [Parse.Symbol "type", alias, def] ->
      let normalizedAlias = normalizeExpression alias
          normalizedDef = normalizeExpression def
      in STypeSynonym <$> (TypeSynonym <$> normalizedAlias <*> normalizedDef)
    _ -> throwError $ NormalizeError "Invalid top-level form!" [expr]
  where
    normalizeImportList ctxt input =
      ImportList <$>
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
