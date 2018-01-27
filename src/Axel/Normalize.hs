{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Axel.Normalize where

import Control.Lens.Operators ((^.))
import Control.Monad ((>=>))
import Control.Monad.Except (MonadError, throwError)

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
  , Literal(LChar, LInt, LList, LString)
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
  , arguments
  , bindings
  , body
  , expr
  , function
  , matches
  )

import Axel.Error (Error(NormalizeError))
import qualified Axel.Parse as Parse
  ( Expression(LiteralChar, LiteralInt, LiteralString, SExpression,
           Symbol)
  )
import Axel.Quote (quoteParseExpression)

normalizeExpression :: (MonadError Error m) => Parse.Expression -> m Expression
normalizeExpression (Parse.LiteralChar char) = return $ ELiteral (LChar char)
normalizeExpression (Parse.LiteralInt int) = return $ ELiteral (LInt int)
normalizeExpression (Parse.LiteralString string) =
  return $ ELiteral (LString string)
normalizeExpression (Parse.SExpression items) =
  case items of
    Parse.Symbol "case":var:cases ->
      let normalizedCases =
            traverse
              (\case
                 Parse.SExpression [pattern', result'] ->
                   (,) <$> normalizeExpression pattern' <*>
                   normalizeExpression result'
                 _ -> throwError $ NormalizeError "0013")
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
                 Parse.SExpression [Parse.Symbol name, value'] ->
                   (name, ) <$> normalizeExpression value'
                 _ -> throwError $ NormalizeError "0001")
              bindings'
      in ELetBlock <$>
         (LetBlock <$> normalizedBindings <*> normalizeExpression body')
    [Parse.Symbol "quote", expression] ->
      return $ quoteParseExpression expression
    function':arguments' ->
      EFunctionApplication <$>
      (FunctionApplication <$> normalizeExpression function' <*>
       traverse normalizeExpression arguments')
    [] -> return EEmptySExpression
normalizeExpression (Parse.Symbol symbol) = return $ EIdentifier symbol

normalizeDefinitions ::
     (MonadError Error m)
  => [Parse.Expression]
  -> m [(ArgumentList, Expression)]
normalizeDefinitions =
  traverse
    (\case
       Parse.SExpression [Parse.SExpression args', definition] ->
         (,) <$> (ArgumentList <$> traverse normalizeExpression args') <*>
         normalizeExpression definition
       _ -> throwError $ NormalizeError "0010")

normalizeStatement :: (MonadError Error m) => Parse.Expression -> m Statement
normalizeStatement (Parse.SExpression items) =
  case items of
    Parse.Symbol "=":Parse.Symbol functionName:typeSignature':definitions ->
      normalizeExpression typeSignature' >>= \case
        EFunctionApplication typeSignature ->
          SFunctionDefinition <$>
          (FunctionDefinition functionName typeSignature <$>
           normalizeDefinitions definitions)
        _ -> throwError $ NormalizeError "0011"
    Parse.Symbol "begin":statements' ->
      let statements = traverse normalizeStatement statements'
      in STopLevel . TopLevel <$> statements
    [Parse.Symbol "data", typeDefinition', Parse.SExpression constructors'] ->
      let constructors =
            traverse
              (normalizeExpression >=> \case
                 EFunctionApplication functionApplication ->
                   return functionApplication
                 _ -> throwError $ NormalizeError "0003")
              constructors'
      in normalizeExpression typeDefinition' >>= \case
           EFunctionApplication typeConstructor ->
             SDataDeclaration <$>
             (DataDeclaration (TypeConstructor typeConstructor) <$> constructors)
           EIdentifier properType ->
             SDataDeclaration <$>
             (DataDeclaration (ProperType properType) <$> constructors)
           _ -> throwError $ NormalizeError "0004"
    Parse.Symbol "defmacro":Parse.Symbol macroName:definitions ->
      SMacroDefinition <$>
      (MacroDefinition macroName <$> normalizeDefinitions definitions)
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
                 _ -> throwError $ NormalizeError "0005")
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
    _ -> throwError $ NormalizeError "0006"
  where
    normalizeImportList input =
      ImportList <$>
      traverse
        (\case
           Parse.Symbol import' -> return $ ImportItem import'
           Parse.SExpression (Parse.Symbol type':imports') ->
             let imports =
                   traverse
                     (\case
                        Parse.Symbol import' -> return import'
                        _ -> throwError $ NormalizeError "0009")
                     imports'
             in ImportType type' <$> imports
           _ -> throwError $ NormalizeError "0007")
        input
normalizeStatement _ = throwError $ NormalizeError "0008"

normalizeProgram :: (MonadError Error m) => Parse.Expression -> m Statement
normalizeProgram =
  normalizeStatement >=> \case
    program@(STopLevel _) -> return program
    _ -> throwError $ NormalizeError "0014"

denormalizeExpression :: Expression -> Parse.Expression
denormalizeExpression (ECaseBlock caseBlock) =
  let denormalizedCases =
        Parse.SExpression $
        map
          (\(pat, res) ->
             Parse.SExpression
               [denormalizeExpression pat, denormalizeExpression res])
          (caseBlock ^. matches)
  in Parse.SExpression
       [ Parse.Symbol "case"
       , denormalizeExpression (caseBlock ^. expr)
       , denormalizedCases
       ]
denormalizeExpression EEmptySExpression = Parse.SExpression []
denormalizeExpression (EFunctionApplication functionApplication) =
  Parse.SExpression $
  denormalizeExpression (functionApplication ^. function) :
  map denormalizeExpression (functionApplication ^. arguments)
denormalizeExpression (EIdentifier x) = Parse.Symbol x
denormalizeExpression (ELambda lambda) =
  let denormalizedArguments =
        Parse.SExpression $ map denormalizeExpression (lambda ^. arguments)
  in Parse.SExpression
       [ Parse.Symbol "fn"
       , denormalizedArguments
       , denormalizeExpression (lambda ^. body)
       ]
denormalizeExpression (ELetBlock letBlock) =
  let denormalizedBindings =
        Parse.SExpression $
        map
          (\(var, val) ->
             Parse.SExpression [Parse.Symbol var, denormalizeExpression val])
          (letBlock ^. bindings)
  in Parse.SExpression
       [ Parse.Symbol "let"
       , denormalizedBindings
       , denormalizeExpression (letBlock ^. body)
       ]
denormalizeExpression (ELiteral x) =
  case x of
    LChar char -> Parse.LiteralChar char
    LInt int -> Parse.LiteralInt int
    LList list ->
      Parse.SExpression $ Parse.Symbol "list" : map denormalizeExpression list
    LString string -> Parse.LiteralString string
