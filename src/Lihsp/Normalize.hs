{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Lihsp.Normalize where

import Control.Monad ((>=>))
import Control.Monad.Except (MonadError, throwError)

import Lihsp.AST
       (DataDeclaration(DataDeclaration),
        Expression(EFunctionApplication, EIdentifier, ELetBlock, ELiteral),
        FunctionApplication(FunctionApplication), ImportList(ImportList),
        LanguagePragma(LanguagePragma), LetBlock(LetBlock),
        Literal(LChar, LInt, LList), QualifiedImport(QualifiedImport),
        RestrictedImport(RestrictedImport),
        Statement(SDataDeclaration, SFunctionDefinition, SLanguagePragma,
                  SModuleDeclaration, SQualifiedImport, SRestrictedImport,
                  STypeSynonym, STypeclassInstance, SUnrestrictedImport),
        TypeSynonym(TypeSynonym), TypeclassInstance(TypeclassInstance))

import Lihsp.Error (Error(NormalizeError))
import qualified Lihsp.Parse as Parse
       (Expression(LiteralChar, LiteralInt, LiteralList, SExpression,
                   Symbol))

normalizeExpression :: (MonadError Error m) => Parse.Expression -> m Expression
normalizeExpression (Parse.LiteralChar char) = return $ ELiteral (LChar char)
normalizeExpression (Parse.LiteralInt int) = return $ ELiteral (LInt int)
normalizeExpression (Parse.LiteralList list) =
  ELiteral . LList <$> traverse normalizeExpression list
normalizeExpression (Parse.SExpression items) =
  case items of
    [Parse.Symbol "let", Parse.SExpression bindings', body] ->
      let bindings =
            traverse
              (\case
                 Parse.SExpression [Parse.Symbol name, value'] ->
                   (name, ) <$> normalizeExpression value'
                 _ -> throwError $ NormalizeError "")
              bindings'
      in ELetBlock <$> (LetBlock <$> bindings <*> normalizeExpression body)
    function:arguments ->
      EFunctionApplication <$>
      (FunctionApplication <$> normalizeExpression function <*>
       traverse normalizeExpression arguments)
    _ -> throwError $ NormalizeError ""
normalizeExpression (Parse.Symbol symbol) = return $ EIdentifier symbol

normalizeStatement :: (MonadError Error m) => Parse.Expression -> m Statement
normalizeStatement (Parse.SExpression items) =
  case items of
    [Parse.Symbol "data", typeDefinition', Parse.SExpression constructors'] ->
      let constructors =
            traverse
              (normalizeExpression >=> \case
                 EFunctionApplication functionApplication ->
                   return functionApplication
                 _ -> throwError $ NormalizeError "")
              constructors'
      in normalizeExpression typeDefinition' >>= \case
           EFunctionApplication typeDefinition ->
             SDataDeclaration <$>
             (DataDeclaration typeDefinition <$> constructors)
           _ -> throwError $ NormalizeError ""
    [Parse.Symbol "import", Parse.Symbol moduleName, Parse.SExpression imports] ->
      SRestrictedImport <$>
      (RestrictedImport moduleName <$> normalizeImportList imports)
    [Parse.Symbol "importq", Parse.Symbol moduleName, Parse.Symbol alias, Parse.SExpression imports] ->
      SQualifiedImport <$>
      (QualifiedImport moduleName alias <$> normalizeImportList imports)
    [Parse.Symbol "import-unrestricted", Parse.Symbol moduleName] ->
      return $ SUnrestrictedImport moduleName
    [Parse.Symbol "instance", instanceName', Parse.SExpression definitions'] ->
      let definitions =
            traverse
              (normalizeStatement >=> \case
                 SFunctionDefinition functionDefinition ->
                   return functionDefinition
                 _ -> throwError $ NormalizeError "")
              definitions'
      in STypeclassInstance <$>
         (TypeclassInstance <$> normalizeExpression instanceName' <*>
          definitions)
    [Parse.Symbol "language", Parse.Symbol languageName] ->
      return $ SLanguagePragma (LanguagePragma languageName)
    [Parse.Symbol "module", Parse.Symbol moduleName] ->
      return $ SModuleDeclaration moduleName
    [Parse.Symbol "type", alias', definition'] ->
      let alias = normalizeExpression alias'
          definition = normalizeExpression definition'
      in STypeSynonym <$> (TypeSynonym <$> alias <*> definition)
    _ -> throwError $ NormalizeError ""
  where
    normalizeImportList imports =
      ImportList <$>
      traverse
        (\case
           Parse.Symbol import' -> return import'
           _ -> throwError $ NormalizeError "")
        imports
normalizeStatement _ = throwError $ NormalizeError ""

normalizeProgram :: (MonadError Error m) => [Parse.Expression] -> m [Statement]
normalizeProgram = traverse normalizeStatement
