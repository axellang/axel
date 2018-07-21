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
normalizeExpression expression@(Parse.SExpression items) =
  case items of
    Parse.Symbol "case":var:cases ->
      let normalizedCases =
            traverse
              (\x ->
                 case x of
                   Parse.SExpression [pattern', result'] ->
                     (,) <$> normalizeExpression pattern' <*>
                     normalizeExpression result'
                   _ -> throwError $ NormalizeError "0013" [x, expression])
              cases
      in ECaseBlock <$>
         (CaseBlock <$> normalizeExpression var <*> normalizedCases)
    [Parse.Symbol "fn", Parse.SExpression arguments', body'] ->
      let normalizedArguments = traverse normalizeExpression arguments'
      in ELambda <$>
         (Lambda <$> normalizedArguments <*> normalizeExpression body')
    [Parse.Symbol "let", Parse.SExpression bindings', body'] ->
      let normalizedBindings =
            traverse
              (\case
                 Parse.SExpression [name', value'] ->
                   (,) <$> normalizeExpression name' <*>
                   normalizeExpression value'
                 x -> throwError $ NormalizeError "0001" [x, expression])
              bindings'
      in ELetBlock <$>
         (LetBlock <$> normalizedBindings <*> normalizeExpression body')
    [Parse.Symbol "quote", expr] -> pure $ quoteParseExpression expr
    function':arguments' ->
      EFunctionApplication <$>
      (FunctionApplication <$> normalizeExpression function' <*>
       traverse normalizeExpression arguments')
    [] -> pure EEmptySExpression
normalizeExpression (Parse.Symbol symbol) = pure $ EIdentifier symbol

normalizeDefinitions ::
     (MonadError Error m)
  => Parse.Expression
  -> [Parse.Expression]
  -> m [(ArgumentList, Expression)]
normalizeDefinitions context =
  traverse
    (\case
       Parse.SExpression [Parse.SExpression args', definition] ->
         (,) <$> (ArgumentList <$> traverse normalizeExpression args') <*>
         normalizeExpression definition
       x -> throwError $ NormalizeError "0010" [x, context])

normalizeStatement :: (MonadError Error m) => Parse.Expression -> m Statement
normalizeStatement expression@(Parse.SExpression items) =
  case items of
    Parse.Symbol "=":Parse.Symbol functionName:typeSignature':definitions ->
      normalizeExpression typeSignature' >>= \case
        EFunctionApplication typeSignature ->
          SFunctionDefinition <$>
          (FunctionDefinition functionName typeSignature <$>
           normalizeDefinitions expression definitions)
        _ -> throwError $ NormalizeError "0011" [typeSignature', expression]
    Parse.Symbol "begin":statements' ->
      let statements = traverse normalizeStatement statements'
      in STopLevel . TopLevel <$> statements
    [Parse.Symbol "data", typeDefinition', Parse.SExpression constructors'] ->
      let constructors =
            traverse
              (\x ->
                 normalizeExpression x >>= \case
                   EFunctionApplication functionApplication ->
                     pure functionApplication
                   _ -> throwError $ NormalizeError "0003" [x, expression])
              constructors'
      in normalizeExpression typeDefinition' >>= \case
           EFunctionApplication typeConstructor ->
             SDataDeclaration <$>
             (DataDeclaration (TypeConstructor typeConstructor) <$> constructors)
           EIdentifier properType ->
             SDataDeclaration <$>
             (DataDeclaration (ProperType properType) <$> constructors)
           _ -> throwError $ NormalizeError "0004" [typeDefinition', expression]
    Parse.Symbol "defmacro":Parse.Symbol macroName:definitions ->
      SMacroDefinition <$>
      (MacroDefinition macroName <$> normalizeDefinitions expression definitions)
    [Parse.Symbol "import", Parse.Symbol moduleName, Parse.SExpression imports] ->
      SRestrictedImport <$>
      (RestrictedImport moduleName <$> normalizeImportList expression imports)
    [Parse.Symbol "importq", Parse.Symbol moduleName, Parse.Symbol alias, Parse.SExpression imports] ->
      SQualifiedImport <$>
      (QualifiedImport moduleName alias <$>
       normalizeImportList expression imports)
    [Parse.Symbol "importUnrestricted", Parse.Symbol moduleName] ->
      pure $ SUnrestrictedImport moduleName
    [Parse.Symbol "instance", instanceName', Parse.SExpression definitions'] ->
      let definitions =
            traverse
              (\x ->
                 normalizeStatement x >>= \case
                   SFunctionDefinition functionDefinition ->
                     pure functionDefinition
                   _ -> throwError $ NormalizeError "0005" [x, expression])
              definitions'
      in STypeclassInstance <$>
         (TypeclassInstance <$> normalizeExpression instanceName' <*>
          definitions)
    [Parse.Symbol "language", Parse.Symbol languageName] ->
      pure $ SLanguagePragma (LanguagePragma languageName)
    [Parse.Symbol "module", Parse.Symbol moduleName] ->
      pure $ SModuleDeclaration moduleName
    [Parse.Symbol "type", alias', definition'] ->
      let alias = normalizeExpression alias'
          definition = normalizeExpression definition'
      in STypeSynonym <$> (TypeSynonym <$> alias <*> definition)
    _ -> throwError $ NormalizeError "0006" [expression]
  where
    normalizeImportList context input =
      ImportList <$>
      traverse
        (\item ->
           case item of
             Parse.Symbol import' -> pure $ ImportItem import'
             Parse.SExpression (Parse.Symbol type':imports') ->
               let imports =
                     traverse
                       (\case
                          Parse.Symbol import' -> pure import'
                          x ->
                            throwError $
                            NormalizeError "0009" [x, item, context])
                       imports'
               in ImportType type' <$> imports
             x -> throwError $ NormalizeError "0007" [x, item, context])
        input
normalizeStatement expression = throwError $ NormalizeError "0008" [expression]

normalizeProgram :: (MonadError Error m) => Parse.Expression -> m Statement
normalizeProgram expression =
  normalizeStatement expression >>= \case
    program@(STopLevel _) -> pure program
    _ -> throwError $ NormalizeError "0014" [expression]
