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

genRawExpression :: (MonadGen m) => m String
genRawExpression = Gen.string (Range.linear 0 10) Gen.unicode

genRecordDefinition :: (MonadGen m) => m AST.RecordDefinition
genRecordDefinition =
  AST.RecordDefinition <$>
  Gen.list (Range.linear 0 3) ((,) <$> genIdentifier <*> genExpression)

genRecordType :: (MonadGen m) => m AST.RecordType
genRecordType =
  AST.RecordType <$>
  Gen.list (Range.linear 0 3) ((,) <$> genIdentifier <*> genExpression)

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
    , AST.ERawExpression <$> genRawExpression
    , AST.ERecordDefinition <$> genRecordDefinition
    , AST.ERecordType <$> genRecordType
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
  genExpression <*>
  Gen.list (Range.linear 0 3) genFunctionDefinition

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

genNewtypeDeclaration :: (MonadGen m) => m AST.NewtypeDeclaration
genNewtypeDeclaration =
  AST.NewtypeDeclaration <$> genTypeDefinition <*> genFunctionApplication

genRawStatement :: (MonadGen m) => m String
genRawStatement = Gen.string (Range.linear 0 10) Gen.unicode

genRestrictedImport :: (MonadGen m) => m AST.RestrictedImport
genRestrictedImport =
  AST.RestrictedImport <$> genIdentifier <*> genImportSpecification

genTopLevel :: (MonadGen m) => m AST.TopLevel
genTopLevel = AST.TopLevel <$> Gen.list (Range.linear 0 3) genStatement

genTypeclassDefinition :: (MonadGen m) => m AST.TypeclassDefinition
genTypeclassDefinition =
  AST.TypeclassDefinition <$> genExpression <*>
  Gen.list (Range.linear 0 3) genExpression <*>
  Gen.list (Range.linear 0 3) genTypeSignature

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
    , AST.SPragma <$> genPragma
    , AST.SModuleDeclaration <$> genIdentifier
    , AST.SQualifiedImport <$> genQualifiedImport
    , AST.SRawStatement <$> genRawStatement
    , AST.SRestrictedImport <$> genRestrictedImport
    , AST.STypeclassDefinition <$> genTypeclassDefinition
    , AST.STypeclassInstance <$> genTypeclassInstance
    , AST.STypeSignature <$> genTypeSignature
    , AST.STypeSynonym <$> genTypeSynonym
    , AST.SUnrestrictedImport <$> genIdentifier
    ]
    [ AST.STopLevel <$> genTopLevel
    , AST.SFunctionDefinition <$> genFunctionDefinition
    , AST.SMacroDefinition <$> genMacroDefinition
    ]
