module Axel.Test.ASTGen where

import qualified Axel.AST as AST

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genIdentifier :: (MonadGen m) => m AST.Identifier
genIdentifier = Gen.string (Range.linear 0 5) Gen.alpha

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
