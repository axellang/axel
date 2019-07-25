module Axel.Test.ASTGen where

import qualified Axel.AST as AST
import Axel.Parse
import qualified Axel.Sourcemap as SM

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genIdentifier :: (MonadGen m) => m AST.Identifier
genIdentifier = Gen.string (Range.linear 1 5) Gen.alpha

genLiteral :: (MonadGen m) => m (AST.Literal SM.Expression)
genLiteral =
  Gen.choice
    [ AST.LChar undefined <$> Gen.unicode
    , AST.LInt undefined <$> Gen.int Range.constantBounded
    , AST.LString undefined <$> Gen.string (Range.linear 0 5) Gen.unicode
    ]

genCaseBlock :: (MonadGen m) => m (AST.CaseBlock SM.Expression)
genCaseBlock =
  AST.CaseBlock undefined <$> genExpression <*>
  Gen.list (Range.linear 0 3) ((,) <$> genExpression <*> genExpression)

genFunctionApplication ::
     (MonadGen m) => m (AST.FunctionApplication SM.Expression)
genFunctionApplication =
  AST.FunctionApplication undefined <$> genExpression <*>
  Gen.list (Range.linear 0 3) genExpression

genIfBlock :: (MonadGen m) => m (AST.IfBlock SM.Expression)
genIfBlock =
  AST.IfBlock undefined <$> genExpression <*> genExpression <*> genExpression

genLambda :: (MonadGen m) => m (AST.Lambda SM.Expression)
genLambda =
  AST.Lambda undefined <$> Gen.list (Range.linear 0 3) genExpression <*>
  genExpression

genLetBlock :: (MonadGen m) => m (AST.LetBlock SM.Expression)
genLetBlock =
  AST.LetBlock undefined <$>
  Gen.list (Range.linear 0 3) ((,) <$> genExpression <*> genExpression) <*>
  genExpression

genRawExpression :: (MonadGen m) => m String
genRawExpression = Gen.string (Range.linear 0 10) Gen.unicode

genRecordDefinition :: (MonadGen m) => m (AST.RecordDefinition SM.Expression)
genRecordDefinition =
  AST.RecordDefinition undefined <$>
  Gen.list (Range.linear 0 3) ((,) <$> genIdentifier <*> genExpression)

genRecordType :: (MonadGen m) => m (AST.RecordType SM.Expression)
genRecordType =
  AST.RecordType undefined <$>
  Gen.list (Range.linear 0 3) ((,) <$> genIdentifier <*> genExpression)

genExpression :: (MonadGen m) => m (AST.Expression SM.Expression)
genExpression =
  Gen.recursive
    Gen.choice
    [ pure $ AST.EEmptySExpression undefined
    , AST.EIdentifier undefined <$> genIdentifier
    ]
    [ AST.ECaseBlock <$> genCaseBlock
    , AST.EFunctionApplication <$> genFunctionApplication
    , AST.ELambda <$> genLambda
    , AST.ELetBlock <$> genLetBlock
    , AST.ELiteral <$> genLiteral
    , AST.EIfBlock <$> genIfBlock
    , AST.ERawExpression undefined <$> genRawExpression
    , AST.ERecordDefinition <$> genRecordDefinition
    , AST.ERecordType <$> genRecordType
    ]

genTypeDefinition :: (MonadGen m) => m (AST.TypeDefinition SM.Expression)
genTypeDefinition =
  Gen.choice
    [ AST.ProperType undefined <$> genIdentifier
    , AST.TypeConstructor undefined <$> genFunctionApplication
    ]

genDataDeclaration :: (MonadGen m) => m (AST.DataDeclaration SM.Expression)
genDataDeclaration =
  AST.DataDeclaration undefined <$> genTypeDefinition <*>
  Gen.list (Range.linear 0 3) genFunctionApplication

genFunctionDefinition ::
     (MonadGen m) => m (AST.FunctionDefinition SM.Expression)
genFunctionDefinition =
  AST.FunctionDefinition undefined <$> genIdentifier <*>
  Gen.list (Range.linear 0 3) genExpression <*>
  genExpression <*>
  Gen.list (Range.linear 0 3) genFunctionDefinition

genPragma :: (MonadGen m) => m (AST.Pragma SM.Expression)
genPragma = AST.Pragma undefined <$> Gen.string (Range.linear 0 10) Gen.ascii

genMacroDefinition :: (MonadGen m) => m (AST.MacroDefinition SM.Expression)
genMacroDefinition = AST.MacroDefinition undefined <$> genFunctionDefinition

genImport :: (MonadGen m) => m (AST.Import SM.Expression)
genImport =
  Gen.choice
    [ AST.ImportItem undefined <$> genIdentifier
    , AST.ImportType undefined <$> genIdentifier <*>
      Gen.list (Range.linear 0 3) genIdentifier
    ]

genImportSpecification ::
     (MonadGen m) => Bool -> m (AST.ImportSpecification SM.Expression)
genImportSpecification importAll =
  let options =
        (AST.ImportOnly undefined <$> Gen.list (Range.linear 0 3) genImport) :
        [pure $ AST.ImportAll undefined | importAll]
   in Gen.choice options

genQualifiedImport :: (MonadGen m) => m (AST.QualifiedImport SM.Expression)
genQualifiedImport =
  AST.QualifiedImport undefined <$> genIdentifier <*> genIdentifier <*>
  genImportSpecification True

genMacroImport :: (MonadGen m) => m (AST.MacroImport SM.Expression)
genMacroImport =
  AST.MacroImport undefined <$> genIdentifier <*>
  Gen.list (Range.linear 0 3) genIdentifier

genNewtypeDeclaration ::
     (MonadGen m) => m (AST.NewtypeDeclaration SM.Expression)
genNewtypeDeclaration =
  AST.NewtypeDeclaration undefined <$> genTypeDefinition <*>
  genFunctionApplication

genRawStatement :: (MonadGen m) => m String
genRawStatement = Gen.string (Range.linear 0 10) Gen.unicode

genRestrictedImport ::
     (MonadGen m) => Bool -> m (AST.RestrictedImport SM.Expression)
genRestrictedImport importAll =
  AST.RestrictedImport undefined <$> genIdentifier <*>
  genImportSpecification importAll

genTopLevel :: (MonadGen m) => m (AST.TopLevel SM.Expression)
genTopLevel =
  AST.TopLevel undefined <$> Gen.list (Range.linear 0 3) genStatement

genTypeclassDefinition ::
     (MonadGen m) => m (AST.TypeclassDefinition SM.Expression)
genTypeclassDefinition =
  AST.TypeclassDefinition undefined <$> genExpression <*>
  Gen.list (Range.linear 0 3) genExpression <*>
  Gen.list (Range.linear 0 3) genTypeSignature

genTypeclassInstance :: (MonadGen m) => m (AST.TypeclassInstance SM.Expression)
genTypeclassInstance =
  AST.TypeclassInstance undefined <$> genExpression <*>
  Gen.list (Range.linear 0 3) genFunctionDefinition

genTypeSignature :: (MonadGen m) => m (AST.TypeSignature SM.Expression)
genTypeSignature =
  AST.TypeSignature undefined <$> genIdentifier <*> genExpression

genTypeSynonym :: (MonadGen m) => m (AST.TypeSynonym SM.Expression)
genTypeSynonym = AST.TypeSynonym undefined <$> genExpression <*> genExpression

genStatement :: (MonadGen m) => m (AST.Statement SM.Expression)
genStatement =
  Gen.recursive
    Gen.choice
    [ AST.SDataDeclaration <$> genDataDeclaration
    , AST.SPragma <$> genPragma
    , AST.SMacroImport <$> genMacroImport
    , AST.SModuleDeclaration undefined <$> genIdentifier
    , AST.SQualifiedImport <$> genQualifiedImport
    , AST.SRawStatement undefined <$> genRawStatement
    , AST.SRestrictedImport <$> genRestrictedImport True
    , AST.STypeclassDefinition <$> genTypeclassDefinition
    , AST.STypeclassInstance <$> genTypeclassInstance
    , AST.STypeSignature <$> genTypeSignature
    , AST.STypeSynonym <$> genTypeSynonym
    , AST.SUnrestrictedImport undefined <$> genIdentifier
    ]
    [ AST.STopLevel <$> genTopLevel
    , AST.SFunctionDefinition <$> genFunctionDefinition
    , AST.SMacroDefinition <$> genMacroDefinition
    ]
