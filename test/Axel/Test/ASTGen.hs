{-# LANGUAGE GADTs #-}

module Axel.Test.ASTGen where

import Axel.Prelude

import qualified Axel.AST as AST
import qualified Axel.Sourcemap as SM

import Control.Applicative
import Control.Lens (isn't)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import TestUtils

genIdentifier :: (MonadGen m) => m AST.Identifier
genIdentifier = Gen.text (Range.linear 1 5) Gen.alpha

genLiteral :: (MonadGen m) => m (AST.Literal (Maybe SM.Expression))
genLiteral =
  Gen.choice
    [ AST.LChar Nothing <$> Gen.unicode
    , AST.LFloat Nothing <$> Gen.float (Range.exponentialFloat (-10000) 10000)
    , AST.LInt Nothing <$> Gen.int Range.constantBounded
    , AST.LString Nothing <$> Gen.text (Range.linear 0 5) Gen.unicode
    ]

genCaseBlock :: (MonadGen m) => m (AST.CaseBlock (Maybe SM.Expression))
genCaseBlock =
  AST.CaseBlock Nothing <$> genExpression <*>
  Gen.list (Range.linear 0 10) ((,) <$> genExpression <*> genExpression)

genFunctionApplication ::
     (MonadGen m) => m (AST.FunctionApplication (Maybe SM.Expression))
genFunctionApplication =
  AST.FunctionApplication Nothing <$>
  Gen.filter (isn't AST._EEmptySExpression) genExpression <*>
  Gen.list (Range.linear 0 10) genExpression

genLambda :: (MonadGen m) => m (AST.Lambda (Maybe SM.Expression))
genLambda =
  AST.Lambda Nothing <$> Gen.list (Range.linear 0 10) genExpression <*>
  genExpression

genLetBlock :: (MonadGen m) => m (AST.LetBlock (Maybe SM.Expression))
genLetBlock =
  AST.LetBlock Nothing <$>
  Gen.list (Range.linear 0 10) ((,) <$> genExpression <*> genExpression) <*>
  genExpression

genRawExpression :: (MonadGen m) => m Text
genRawExpression = Gen.text (Range.linear 0 10) Gen.unicode

genRecordDefinition ::
     (MonadGen m) => m (AST.RecordDefinition (Maybe SM.Expression))
genRecordDefinition =
  AST.RecordDefinition Nothing <$>
  Gen.list (Range.linear 0 10) ((,) <$> genIdentifier <*> genExpression)

genRecordType :: (MonadGen m) => m (AST.RecordType (Maybe SM.Expression))
genRecordType =
  AST.RecordType Nothing <$>
  Gen.list (Range.linear 0 10) ((,) <$> genIdentifier <*> genExpression)

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
     (Alternative m, MonadGen m)
  => m (AST.DataDeclaration (Maybe SM.Expression))
genDataDeclaration =
  AST.DataDeclaration Nothing <$> genTypeDefinition <*>
  Gen.list (Range.linear 0 10) genExpression <*>
  genConstraints

genFunctionDefinition ::
     (Alternative m, MonadGen m)
  => Maybe Int
  -> m (AST.FunctionDefinition (Maybe SM.Expression))
genFunctionDefinition numArgs =
  let genArgs =
        case numArgs of
          Just n -> Range.linear n n
          Nothing -> Range.linear 0 10
   in AST.FunctionDefinition Nothing <$> genIdentifier <*>
      Gen.list genArgs genExpression <*>
      genExpression <*>
      Gen.list
        (Range.linear 0 10)
        ((AST.STypeSignature <$> genTypeSignature) <|>
         (AST.SFunctionDefinition <$> genFunctionDefinition Nothing))

genPragma :: (MonadGen m) => m (AST.Pragma (Maybe SM.Expression))
genPragma = AST.Pragma Nothing <$> Gen.text (Range.linear 0 10) Gen.ascii

genMacroDefinition ::
     (Alternative m, MonadGen m)
  => m (AST.MacroDefinition (Maybe SM.Expression))
genMacroDefinition =
  AST.MacroDefinition Nothing <$> genFunctionDefinition (Just 1)

genImport :: (MonadGen m) => m (AST.Import (Maybe SM.Expression))
genImport =
  Gen.choice
    [ AST.ImportItem Nothing <$> genIdentifier
    , AST.ImportType Nothing <$> genIdentifier <*>
      Gen.list (Range.linear 0 10) genIdentifier
    ]

genImportSpecification ::
     (MonadGen m) => Bool -> m (AST.ImportSpecification (Maybe SM.Expression))
genImportSpecification importAll =
  let options =
        (AST.ImportOnly Nothing <$> Gen.list (Range.linear 0 10) genImport) :
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
  Gen.list (Range.linear 0 10) genIdentifier

genNewtypeDeclaration ::
     (Alternative m, MonadGen m)
  => m (AST.NewtypeDeclaration (Maybe SM.Expression))
genNewtypeDeclaration =
  AST.NewtypeDeclaration Nothing <$> genTypeDefinition <*> genExpression <*>
  genConstraints

genRawStatement :: (MonadGen m) => m Text
genRawStatement = Gen.text (Range.linear 0 10) Gen.unicode

genRestrictedImport ::
     (MonadGen m) => Bool -> m (AST.RestrictedImport (Maybe SM.Expression))
genRestrictedImport importAll =
  AST.RestrictedImport Nothing <$> genIdentifier <*>
  genImportSpecification importAll

genTopLevel ::
     (Alternative m, MonadGen m) => m (AST.TopLevel (Maybe SM.Expression))
genTopLevel = AST.TopLevel Nothing <$> Gen.list (Range.linear 0 10) genStatement

genConstraints :: (Alternative m, MonadGen m) => m [AST.SMExpression]
genConstraints =
  Gen.list
    (Range.linear 0 10)
    ((AST.EIdentifier Nothing <$> genIdentifier) <|>
     (AST.EFunctionApplication <$> genFunctionApplication))

genTypeclassDefinition ::
     (Alternative m, MonadGen m)
  => m (AST.TypeclassDefinition (Maybe SM.Expression))
genTypeclassDefinition =
  AST.TypeclassDefinition Nothing <$>
  ((AST.EFunctionApplication <$> genFunctionApplication) <|>
   (AST.EIdentifier Nothing <$> genIdentifier)) <*>
  genConstraints <*>
  Gen.list (Range.linear 0 10) genTypeSignature

genTypeclassInstance ::
     (Alternative m, MonadGen m)
  => m (AST.TypeclassInstance (Maybe SM.Expression))
genTypeclassInstance =
  AST.TypeclassInstance Nothing <$>
  ((AST.EFunctionApplication <$> genFunctionApplication) <|>
   (AST.EIdentifier Nothing <$> genIdentifier)) <*>
  genConstraints <*>
  Gen.list (Range.linear 0 10) (genFunctionDefinition Nothing)

genTypeSignature ::
     (Alternative m, MonadGen m) => m (AST.TypeSignature (Maybe SM.Expression))
genTypeSignature =
  AST.TypeSignature Nothing <$> genIdentifier <*> genConstraints <*>
  genExpression

genTypeSynonym :: (MonadGen m) => m (AST.TypeSynonym (Maybe SM.Expression))
genTypeSynonym = AST.TypeSynonym Nothing <$> genExpression <*> genExpression

genStatement :: (Alternative m, MonadGen m) => m AST.SMStatement
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
    ]
    [ AST.STopLevel <$> genTopLevel
    , AST.SFunctionDefinition <$> genFunctionDefinition Nothing
    , AST.SMacroDefinition <$> genMacroDefinition
    ]
