module Axel.Test.ASTGen where

import qualified Axel.AST as AST

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genIdentifier :: (MonadGen m) => m AST.Identifier
genIdentifier = Gen.string (Range.linear 1 5) Gen.alpha

genLiteral :: (MonadGen m) => m AST.Literal
genLiteral =
  Gen.choice
    [ AST.LChar <$> Gen.unicode
    , AST.LInt <$> Gen.int Range.constantBounded
    , AST.LString <$> Gen.string (Range.linear 0 5) Gen.unicode
    ]

genCaseBlock :: (MonadGen m) => m AST.CaseBlock
genCaseBlock =
  AST.CaseBlock <$> genExpression <*>
  Gen.list (Range.linear 0 3) ((,) <$> genExpression <*> genExpression)

genFunctionApplication :: (MonadGen m) => m AST.FunctionApplication
genFunctionApplication =
  AST.FunctionApplication <$> genExpression <*>
  Gen.list (Range.linear 0 3) genExpression

genLambda :: (MonadGen m) => m AST.Lambda
genLambda =
  AST.Lambda <$> Gen.list (Range.linear 0 3) genExpression <*> genExpression

genLetBlock :: (MonadGen m) => m AST.LetBlock
genLetBlock =
  AST.LetBlock <$>
  Gen.list (Range.linear 0 3) ((,) <$> genExpression <*> genExpression) <*>
  genExpression

genExpression :: (MonadGen m) => m AST.Expression
genExpression =
  Gen.recursive
    Gen.choice
    [pure AST.EEmptySExpression, AST.EIdentifier <$> genIdentifier]
    [ AST.ECaseBlock <$> genCaseBlock
    , AST.EFunctionApplication <$> genFunctionApplication
    , AST.ELambda <$> genLambda
    , AST.ELetBlock <$> genLetBlock
    , AST.ELiteral <$> genLiteral
    ]

genTypeDefinition :: (MonadGen m) => m AST.TypeDefinition
genTypeDefinition =
  Gen.choice
    [ AST.ProperType <$> genIdentifier
    , AST.TypeConstructor <$> genFunctionApplication
    ]

genDataDeclaration :: (MonadGen m) => m AST.DataDeclaration
genDataDeclaration =
  AST.DataDeclaration <$> genTypeDefinition <*>
  Gen.list (Range.linear 0 3) genFunctionApplication

genFunctionDefinition :: (MonadGen m) => m AST.FunctionDefinition
genFunctionDefinition =
  AST.FunctionDefinition <$> genIdentifier <*>
  Gen.list (Range.linear 0 3) genExpression <*>
  genExpression

genPragma :: (MonadGen m) => m AST.Pragma
genPragma = AST.Pragma <$> Gen.string (Range.linear 0 10) Gen.ascii

genMacroDefinition :: (MonadGen m) => m AST.MacroDefinition
genMacroDefinition = AST.MacroDefinition <$> genFunctionDefinition

genImport :: (MonadGen m) => m AST.Import
genImport =
  Gen.choice
    [ AST.ImportItem <$> genIdentifier
    , AST.ImportType <$> genIdentifier <*>
      Gen.list (Range.linear 0 3) genIdentifier
    ]

genImportSpecification :: (MonadGen m) => m AST.ImportSpecification
genImportSpecification =
  Gen.choice
    [ pure AST.ImportAll
    , AST.ImportOnly <$> Gen.list (Range.linear 0 3) genImport
    ]

genQualifiedImport :: (MonadGen m) => m AST.QualifiedImport
genQualifiedImport =
  AST.QualifiedImport <$> genIdentifier <*> genIdentifier <*>
  genImportSpecification

genRestrictedImport :: (MonadGen m) => m AST.RestrictedImport
genRestrictedImport =
  AST.RestrictedImport <$> genIdentifier <*> genImportSpecification

genTopLevel :: (MonadGen m) => m AST.TopLevel
genTopLevel = AST.TopLevel <$> Gen.list (Range.linear 0 3) genStatement

genTypeclassInstance :: (MonadGen m) => m AST.TypeclassInstance
genTypeclassInstance =
  AST.TypeclassInstance <$> genExpression <*>
  Gen.list (Range.linear 0 3) genFunctionDefinition

genTypeSignature :: (MonadGen m) => m AST.TypeSignature
genTypeSignature = AST.TypeSignature <$> genIdentifier <*> genExpression

genTypeSynonym :: (MonadGen m) => m AST.TypeSynonym
genTypeSynonym = AST.TypeSynonym <$> genExpression <*> genExpression

genStatement :: (MonadGen m) => m AST.Statement
genStatement =
  Gen.recursive
    Gen.choice
    [ AST.SDataDeclaration <$> genDataDeclaration
    , AST.SFunctionDefinition <$> genFunctionDefinition
    , AST.SPragma <$> genPragma
    , AST.SMacroDefinition <$> genMacroDefinition
    , AST.SModuleDeclaration <$> genIdentifier
    , AST.SQualifiedImport <$> genQualifiedImport
    , AST.SRestrictedImport <$> genRestrictedImport
    , AST.STypeclassInstance <$> genTypeclassInstance
    , AST.STypeSignature <$> genTypeSignature
    , AST.STypeSynonym <$> genTypeSynonym
    , AST.SUnrestrictedImport <$> genIdentifier
    ]
    [AST.STopLevel <$> genTopLevel]
