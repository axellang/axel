{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Axel.AST where

import Axel.Utils.Display
  ( Bracket(DoubleQuotes, Parentheses, SingleQuotes, SquareBrackets)
  , Delimiter(Commas, Newlines, Pipes, Spaces)
  , delimit
  , isOperator
  , renderBlock
  , renderPragma
  , surround
  )
import Axel.Utils.Recursion
  ( Recursive(bottomUpFmap, bottomUpTraverse, topDownFmap)
  )

import Control.Arrow ((***))
import Control.Lens.Operators ((%~), (^.))
import Control.Lens.TH (makeFieldsNoPrefix)
import Data.Function ((&))
import Data.Semigroup ((<>))

class ToHaskell a where
  toHaskell :: a -> String

type Identifier = String

data CaseBlock = CaseBlock
  { _expr :: Expression
  , _matches :: [(Expression, Expression)]
  } deriving (Eq, Show)

data FunctionApplication = FunctionApplication
  { _function :: Expression
  , _arguments :: [Expression]
  } deriving (Eq, Show)

newtype TopLevel = TopLevel
  { _statements :: [Statement]
  } deriving (Eq, Show)

data TypeDefinition
  = ProperType Identifier
  | TypeConstructor FunctionApplication
  deriving (Eq, Show)

instance ToHaskell TypeDefinition where
  toHaskell :: TypeDefinition -> String
  toHaskell (ProperType x) = x
  toHaskell (TypeConstructor x) = toHaskell x

data DataDeclaration = DataDeclaration
  { _typeDefinition :: TypeDefinition
  , _constructors :: [FunctionApplication]
  } deriving (Eq, Show)

newtype ArgumentList =
  ArgumentList [Expression]
  deriving (Eq, Show)

instance ToHaskell ArgumentList where
  toHaskell :: ArgumentList -> String
  toHaskell (ArgumentList arguments) = delimit Spaces $ map toHaskell arguments

data FunctionDefinition = FunctionDefinition
  { _name :: Identifier
  , _typeSignature :: FunctionApplication
  , _definitions :: [(ArgumentList, Expression)]
  } deriving (Eq, Show)

data Import
  = ImportItem Identifier
  | ImportType Identifier
               [Identifier]
  deriving (Eq, Show)

instance ToHaskell Import where
  toHaskell :: Import -> String
  toHaskell (ImportItem x) =
    if isOperator x
      then surround Parentheses x
      else x
  toHaskell (ImportType typeName imports) =
    typeName <> surround Parentheses (delimit Commas imports)

data ImportSpecification
  = ImportAll
  | ImportOnly [Import]
  deriving (Eq, Show)

instance ToHaskell ImportSpecification where
  toHaskell :: ImportSpecification -> String
  toHaskell ImportAll = ""
  toHaskell (ImportOnly importList) =
    surround Parentheses $ delimit Commas $ map toHaskell importList

data Lambda = Lambda
  { _arguments :: [Expression]
  , _body :: Expression
  } deriving (Eq, Show)

newtype LanguagePragma = LanguagePragma
  { _language :: Identifier
  } deriving (Eq, Show)

data LetBlock = LetBlock
  { _bindings :: [(Expression, Expression)]
  , _body :: Expression
  } deriving (Eq, Show)

data MacroDefinition = MacroDefinition
  { _name :: Identifier
  , _definitions :: [(ArgumentList, Expression)]
  } deriving (Eq, Show)

data QualifiedImport = QualifiedImport
  { _moduleName :: Identifier
  , _alias :: Identifier
  , _imports :: ImportSpecification
  } deriving (Eq, Show)

data RestrictedImport = RestrictedImport
  { _moduleName :: Identifier
  , _imports :: ImportSpecification
  } deriving (Eq, Show)

data TypeclassInstance = TypeclassInstance
  { _instanceName :: Expression
  , _definitions :: [FunctionDefinition]
  } deriving (Eq, Show)

data TypeSynonym = TypeSynonym
  { _alias :: Expression
  , _definition :: Expression
  } deriving (Eq, Show)

data Expression
  = ECaseBlock CaseBlock
  | EEmptySExpression
  | EFunctionApplication FunctionApplication
  | EIdentifier Identifier
  | ELambda Lambda
  | ELetBlock LetBlock
  | ELiteral Literal
  deriving (Eq, Show)

instance ToHaskell Expression where
  toHaskell :: Expression -> String
  toHaskell (ECaseBlock x) = toHaskell x
  toHaskell EEmptySExpression = "()"
  toHaskell (EFunctionApplication x) = toHaskell x
  toHaskell (EIdentifier x) =
    if isOperator x
      then surround Parentheses x
      else x
  toHaskell (ELambda x) = toHaskell x
  toHaskell (ELetBlock x) = toHaskell x
  toHaskell (ELiteral x) = toHaskell x

data Literal
  = LChar Char
  | LInt Int
  | LString String
  deriving (Eq, Show)

instance ToHaskell Literal where
  toHaskell :: Literal -> String
  toHaskell (LChar x) = surround SingleQuotes [x]
  toHaskell (LInt x) = show x
  toHaskell (LString x) = surround DoubleQuotes x

data Statement
  = SDataDeclaration DataDeclaration
  | SFunctionDefinition FunctionDefinition
  | SLanguagePragma LanguagePragma
  | SMacroDefinition MacroDefinition
  | SModuleDeclaration Identifier
  | SQualifiedImport QualifiedImport
  | SRestrictedImport RestrictedImport
  | STopLevel TopLevel
  | STypeclassInstance TypeclassInstance
  | STypeSynonym TypeSynonym
  | SUnrestrictedImport Identifier
  deriving (Eq, Show)

instance ToHaskell Statement where
  toHaskell :: Statement -> String
  toHaskell (SDataDeclaration x) = toHaskell x
  toHaskell (SFunctionDefinition x) = toHaskell x
  toHaskell (SLanguagePragma x) = toHaskell x
  toHaskell (SMacroDefinition x) = toHaskell x
  toHaskell (SModuleDeclaration x) = "module " <> x <> " where"
  toHaskell (SQualifiedImport x) = toHaskell x
  toHaskell (SRestrictedImport x) = toHaskell x
  toHaskell (STopLevel xs) = toHaskell xs
  toHaskell (STypeclassInstance x) = toHaskell x
  toHaskell (STypeSynonym x) = toHaskell x
  toHaskell (SUnrestrictedImport x) = "import " <> x

type Program = [Statement]

makeFieldsNoPrefix ''CaseBlock

makeFieldsNoPrefix ''DataDeclaration

makeFieldsNoPrefix ''FunctionApplication

makeFieldsNoPrefix ''FunctionDefinition

makeFieldsNoPrefix ''Lambda

makeFieldsNoPrefix ''LanguagePragma

makeFieldsNoPrefix ''LetBlock

makeFieldsNoPrefix ''MacroDefinition

makeFieldsNoPrefix ''QualifiedImport

makeFieldsNoPrefix ''RestrictedImport

makeFieldsNoPrefix ''TopLevel

makeFieldsNoPrefix ''TypeclassInstance

makeFieldsNoPrefix ''TypeSynonym

instance ToHaskell CaseBlock where
  toHaskell :: CaseBlock -> String
  toHaskell caseBlock =
    surround Parentheses $
    "case " <> toHaskell (caseBlock ^. expr) <> " of " <>
    renderBlock (map matchToHaskell (caseBlock ^. matches))
    where
      matchToHaskell (pat, result) = toHaskell pat <> " -> " <> toHaskell result

instance ToHaskell FunctionApplication where
  toHaskell :: FunctionApplication -> String
  toHaskell functionApplication =
    case functionApplication ^. function of
      EIdentifier "list" ->
        surround SquareBrackets $
        delimit Commas (map toHaskell $ functionApplication ^. arguments)
      _ ->
        surround Parentheses $
        toHaskell (functionApplication ^. function) <> " " <>
        delimit Spaces (map toHaskell $ functionApplication ^. arguments)

functionDefinitionToHaskell ::
     Identifier -> (ArgumentList, Expression) -> String
functionDefinitionToHaskell functionName (pattern', definitionBody) =
  functionName <> " " <> toHaskell pattern' <> " = " <> toHaskell definitionBody

instance ToHaskell FunctionDefinition where
  toHaskell :: FunctionDefinition -> String
  toHaskell functionDefinition =
    delimit Newlines $
    (functionDefinition ^. name <> " :: " <>
     toHaskell (functionDefinition ^. typeSignature)) :
    map
      (functionDefinitionToHaskell $ functionDefinition ^. name)
      (functionDefinition ^. definitions)

instance ToHaskell DataDeclaration where
  toHaskell :: DataDeclaration -> String
  toHaskell dataDeclaration =
    "data " <> toHaskell (dataDeclaration ^. typeDefinition) <> " = " <>
    delimit
      Pipes
      (map (removeSurroundingParentheses . toHaskell) $
       dataDeclaration ^. constructors)
    where
      removeSurroundingParentheses = tail . init

instance ToHaskell Lambda where
  toHaskell :: Lambda -> String
  toHaskell lambda =
    surround Parentheses $
    "\\" <> delimit Spaces (map toHaskell (lambda ^. arguments)) <> " -> " <>
    toHaskell (lambda ^. body)

instance ToHaskell LanguagePragma where
  toHaskell :: LanguagePragma -> String
  toHaskell languagePragma =
    renderPragma $ "LANGUAGE " <> languagePragma ^. language

instance ToHaskell LetBlock where
  toHaskell :: LetBlock -> String
  toHaskell letBlock =
    surround Parentheses $
    "let " <> renderBlock (map bindingToHaskell (letBlock ^. bindings)) <>
    " in " <>
    toHaskell (letBlock ^. body)
    where
      bindingToHaskell (pattern', value) =
        toHaskell pattern' <> " = " <> toHaskell value

instance ToHaskell MacroDefinition where
  toHaskell :: MacroDefinition -> String
  toHaskell macroDefinition =
    delimit Newlines $
    (macroDefinition ^. name <> " :: [AST.Expression] -> IO [AST.Expression]") :
    map
      (functionDefinitionToHaskell $ macroDefinition ^. name)
      (macroDefinition ^. definitions)

instance ToHaskell QualifiedImport where
  toHaskell :: QualifiedImport -> String
  toHaskell qualifiedImport =
    "import " <> qualifiedImport ^. moduleName <> " as " <> qualifiedImport ^.
    alias <>
    toHaskell (qualifiedImport ^. imports)

instance ToHaskell RestrictedImport where
  toHaskell :: RestrictedImport -> String
  toHaskell restrictedImport =
    "import " <> restrictedImport ^. moduleName <>
    toHaskell (restrictedImport ^. imports)

instance ToHaskell TopLevel where
  toHaskell :: TopLevel -> String
  toHaskell topLevel = delimit Newlines $ map toHaskell (topLevel ^. statements)

instance ToHaskell TypeclassInstance where
  toHaskell :: TypeclassInstance -> String
  toHaskell typeclassInstance =
    "instance " <> toHaskell (typeclassInstance ^. instanceName) <> " where " <>
    renderBlock (map toHaskell $ typeclassInstance ^. definitions)

instance ToHaskell TypeSynonym where
  toHaskell :: TypeSynonym -> String
  toHaskell typeSynonym =
    "type " <> toHaskell (typeSynonym ^. alias) <> " = " <>
    toHaskell (typeSynonym ^. definition)

instance Recursive Expression where
  bottomUpFmap :: (Expression -> Expression) -> Expression -> Expression
  bottomUpFmap f x =
    f $
    case x of
      ECaseBlock caseBlock ->
        ECaseBlock $
        caseBlock & expr %~ bottomUpFmap f &
        matches %~ map (bottomUpFmap f *** bottomUpFmap f)
      EEmptySExpression -> x
      EFunctionApplication functionApplication ->
        EFunctionApplication $
        functionApplication & function %~ bottomUpFmap f &
        arguments %~ map (bottomUpFmap f)
      EIdentifier _ -> x
      ELambda lambda ->
        ELambda $
        lambda & arguments %~ map (bottomUpFmap f) & body %~ bottomUpFmap f
      ELetBlock letBlock ->
        ELetBlock $
        letBlock & bindings %~ map (bottomUpFmap f *** bottomUpFmap f) &
        body %~ bottomUpFmap f
      ELiteral literal ->
        case literal of
          LChar _ -> x
          LInt _ -> x
          LString _ -> x
  topDownFmap :: (Expression -> Expression) -> Expression -> Expression
  topDownFmap f x =
    case f x of
      ECaseBlock caseBlock ->
        ECaseBlock $
        caseBlock & expr %~ bottomUpFmap f &
        matches %~ map (bottomUpFmap f *** bottomUpFmap f)
      EEmptySExpression -> x
      EFunctionApplication functionApplication ->
        EFunctionApplication $
        functionApplication & function %~ bottomUpFmap f &
        arguments %~ map (bottomUpFmap f)
      EIdentifier _ -> x
      ELambda lambda ->
        ELambda $
        lambda & arguments %~ map (bottomUpFmap f) & body %~ bottomUpFmap f
      ELetBlock letBlock ->
        ELetBlock $
        letBlock & bindings %~ map (bottomUpFmap f *** bottomUpFmap f) &
        body %~ bottomUpFmap f
      ELiteral literal ->
        case literal of
          LChar _ -> x
          LInt _ -> x
          LString _ -> x
  bottomUpTraverse ::
       (Monad m) => (Expression -> m Expression) -> Expression -> m Expression
  bottomUpTraverse f x =
    f =<<
    case x of
      ECaseBlock caseBlock ->
        ECaseBlock <$>
        (CaseBlock <$> bottomUpTraverse f (caseBlock ^. expr) <*>
         traverse
           (\(a, b) -> (,) <$> bottomUpTraverse f a <*> bottomUpTraverse f b)
           (caseBlock ^. matches))
      EEmptySExpression -> pure x
      EFunctionApplication functionApplication ->
        EFunctionApplication <$>
        (FunctionApplication <$>
         bottomUpTraverse f (functionApplication ^. function) <*>
         traverse (bottomUpTraverse f) (functionApplication ^. arguments))
      EIdentifier _ -> pure x
      ELambda lambda ->
        ELambda <$>
        (Lambda <$> traverse (bottomUpTraverse f) (lambda ^. arguments) <*>
         bottomUpTraverse f (lambda ^. body))
      ELetBlock letBlock ->
        ELetBlock <$>
        (LetBlock <$>
         traverse
           (\(a, b) -> (a, ) <$> bottomUpTraverse f b)
           (letBlock ^. bindings) <*>
         bottomUpTraverse f (letBlock ^. body))
      ELiteral literal ->
        case literal of
          LChar _ -> pure x
          LInt _ -> pure x
          LString _ -> pure x
