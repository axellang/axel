{-# LANGUAGE GADTs #-}

module Axel.Test.ASTGen where

import qualified Axel.AST as AST
import qualified Axel.Sourcemap as SM

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import TestUtils

genIdentifier :: (MonadGen m) => m AST.Identifier
genIdentifier = Gen.string (Range.linear 1 5) Gen.alpha

genLiteral :: (MonadGen m) => m (AST.Literal (Maybe SM.Expression))
genLiteral =
  Gen.choice
    [ AST.LChar Nothing <$> Gen.unicode
    , AST.LInt Nothing <$> Gen.int Range.constantBounded
    , AST.LString Nothing <$> Gen.string (Range.linear 0 5) Gen.unicode
    ]

genCaseBlock :: (MonadGen m) => m (AST.CaseBlock (Maybe SM.Expression))
genCaseBlock =
  AST.CaseBlock Nothing <$> genExpression <*>
  Gen.list (Range.linear 0 3) ((,) <$> genExpression <*> genExpression)

genFunctionApplication ::
     (MonadGen m) => m (AST.FunctionApplication (Maybe SM.Expression))
genFunctionApplication =
  AST.FunctionApplication Nothing <$> genExpression <*>
  Gen.list (Range.linear 0 3) genExpression

genIfBlock :: (MonadGen m) => m (AST.IfBlock (Maybe SM.Expression))
genIfBlock =
  AST.IfBlock Nothing <$> genExpression <*> genExpression <*> genExpression

genLambda :: (MonadGen m) => m (AST.Lambda (Maybe SM.Expression))
genLambda =
  AST.Lambda Nothing <$> Gen.list (Range.linear 0 3) genExpression <*>
  genExpression

genLetBlock :: (MonadGen m) => m (AST.LetBlock (Maybe SM.Expression))
genLetBlock =
  AST.LetBlock Nothing <$>
  Gen.list (Range.linear 0 3) ((,) <$> genExpression <*> genExpression) <*>
  genExpression

genRawExpression :: (MonadGen m) => m String
genRawExpression = Gen.string (Range.linear 0 10) Gen.unicode

genRecordDefinition ::
     (MonadGen m) => m (AST.RecordDefinition (Maybe SM.Expression))
genRecordDefinition =
  AST.RecordDefinition Nothing <$>
  Gen.list (Range.linear 0 3) ((,) <$> genIdentifier <*> genExpression)

genRecordType :: (MonadGen m) => m (AST.RecordType (Maybe SM.Expression))
genRecordType =
  AST.RecordType Nothing <$>
  Gen.list (Range.linear 0 3) ((,) <$> genIdentifier <*> genExpression)

genExpression :: (MonadGen m) => m (AST.Expression (Maybe SM.Expression))
genExpression =
  Gen.recursive
    Gen.choice
    [ pure $ AST.EEmptySExpression Nothing
    , AST.EIdentifier Nothing <$> genIdentifier
    ]
    [ AST.ECaseBlock <$> genCaseBlock
    , AST.EFunctionApplication <$> genFunctionApplication
    , AST.ELambda <$> genLambda
    , AST.ELetBlock <$> genLetBlock
    , AST.ELiteral <$> genLiteral
    , AST.EIfBlock <$> genIfBlock
    , AST.ERawExpression Nothing <$> genRawExpression
    , AST.ERecordDefinition <$> genRecordDefinition
    , AST.ERecordType <$> genRecordType
    ]

genTypeDefinition ::
     (MonadGen m) => m (AST.TypeDefinition (Maybe SM.Expression))
genTypeDefinition =
  Gen.choice
    [ AST.ProperType Nothing <$> genIdentifier
    , AST.TypeConstructor Nothing <$> genFunctionApplication
    ]

genDataDeclaration ::
     (MonadGen m) => m (AST.DataDeclaration (Maybe SM.Expression))
genDataDeclaration =
  AST.DataDeclaration Nothing <$> genTypeDefinition <*>
  Gen.list (Range.linear 0 3) genFunctionApplication

genFunctionDefinition ::
     (MonadGen m) => m (AST.FunctionDefinition (Maybe SM.Expression))
genFunctionDefinition =
  AST.FunctionDefinition Nothing <$> genIdentifier <*>
  Gen.list (Range.linear 0 3) genExpression <*>
  genExpression <*>
  Gen.list (Range.linear 0 3) genFunctionDefinition

genPragma :: (MonadGen m) => m (AST.Pragma (Maybe SM.Expression))
genPragma = AST.Pragma Nothing <$> Gen.string (Range.linear 0 10) Gen.ascii

genMacroDefinition ::
     (MonadGen m) => m (AST.MacroDefinition (Maybe SM.Expression))
genMacroDefinition = AST.MacroDefinition Nothing <$> genFunctionDefinition

genImport :: (MonadGen m) => m (AST.Import (Maybe SM.Expression))
genImport =
  Gen.choice
    [ AST.ImportItem Nothing <$> genIdentifier
    , AST.ImportType Nothing <$> genIdentifier <*>
      Gen.list (Range.linear 0 3) genIdentifier
    ]

genImportSpecification ::
     (MonadGen m) => Bool -> m (AST.ImportSpecification (Maybe SM.Expression))
genImportSpecification importAll =
  let options =
        (AST.ImportOnly Nothing <$> Gen.list (Range.linear 0 3) genImport) :
        [pure $ AST.ImportAll Nothing | importAll]
   in Gen.choice options

genQualifiedImport ::
     (MonadGen m) => m (AST.QualifiedImport (Maybe SM.Expression))
genQualifiedImport =
  AST.QualifiedImport Nothing <$> genIdentifier <*> genIdentifier <*>
  genImportSpecification True

genMacroImport :: (MonadGen m) => m (AST.MacroImport (Maybe SM.Expression))
genMacroImport =
  AST.MacroImport Nothing <$> genIdentifier <*>
  Gen.list (Range.linear 0 3) genIdentifier

genNewtypeDeclaration ::
     (MonadGen m) => m (AST.NewtypeDeclaration (Maybe SM.Expression))
genNewtypeDeclaration =
  AST.NewtypeDeclaration Nothing <$> genTypeDefinition <*>
  genFunctionApplication

genRawStatement :: (MonadGen m) => m String
genRawStatement = Gen.string (Range.linear 0 10) Gen.unicode

genRestrictedImport ::
     (MonadGen m) => Bool -> m (AST.RestrictedImport (Maybe SM.Expression))
genRestrictedImport importAll =
  AST.RestrictedImport Nothing <$> genIdentifier <*>
  genImportSpecification importAll

genTopLevel :: (MonadGen m) => m (AST.TopLevel (Maybe SM.Expression))
genTopLevel = AST.TopLevel Nothing <$> Gen.list (Range.linear 0 3) genStatement

genTypeclassDefinition ::
     (MonadGen m) => m (AST.TypeclassDefinition (Maybe SM.Expression))
genTypeclassDefinition =
  AST.TypeclassDefinition Nothing <$> genExpression <*>
  Gen.list (Range.linear 0 3) genExpression <*>
  Gen.list (Range.linear 0 3) genTypeSignature

genTypeclassInstance ::
     (MonadGen m) => m (AST.TypeclassInstance (Maybe SM.Expression))
genTypeclassInstance =
  AST.TypeclassInstance Nothing <$> genExpression <*>
  Gen.list (Range.linear 0 3) genFunctionDefinition

genTypeSignature :: (MonadGen m) => m (AST.TypeSignature (Maybe SM.Expression))
genTypeSignature = AST.TypeSignature Nothing <$> genIdentifier <*> genExpression

genTypeSynonym :: (MonadGen m) => m (AST.TypeSynonym (Maybe SM.Expression))
genTypeSynonym = AST.TypeSynonym Nothing <$> genExpression <*> genExpression

genStatement :: (MonadGen m) => m AST.SMStatement
genStatement =
  Gen.recursive
    Gen.choice
    [ AST.SDataDeclaration <$> genDataDeclaration
    , AST.SPragma <$> genPragma
    , AST.SMacroImport <$> genMacroImport
    , AST.SModuleDeclaration Nothing <$> genIdentifier
    , AST.SQualifiedImport <$> genQualifiedImport
    , AST.SRawStatement Nothing <$> genRawStatement
    , AST.SRestrictedImport <$> genRestrictedImport True
    , AST.STypeclassDefinition <$> genTypeclassDefinition
    , AST.STypeclassInstance <$> genTypeclassInstance
    , AST.STypeSignature <$> genTypeSignature
    , AST.STypeSynonym <$> genTypeSynonym
    , AST.SUnrestrictedImport Nothing <$> genIdentifier
    ]
    [ AST.STopLevel <$> genTopLevel
    , AST.SFunctionDefinition <$> genFunctionDefinition
    , AST.SMacroDefinition <$> genMacroDefinition
    ]
