{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Lihsp.AST where

import Control.Lens.Operators ((^.))
import Control.Lens.TH (makeFieldsNoPrefix)

import Data.Semigroup ((<>))

import Lihsp.Utils.Display
  ( Bracket(DoubleQuotes, Parentheses, SingleQuotes, SquareBrackets)
  , Delimiter(Commas, Newlines, Pipes, Spaces)
  , delimit
  , isOperator
  , renderBlock
  , renderPragma
  , surround
  )

class ToHaskell a where
  toHaskell :: a -> String

type Identifier = String

data FunctionApplication = FunctionApplication
  { _function :: Expression
  , _arguments :: [Expression]
  } deriving (Eq)

newtype TopLevel = TopLevel
  { _statements :: [Statement]
  } deriving (Eq)

data TypeDefinition
  = ProperType Identifier
  | TypeConstructor FunctionApplication
  deriving (Eq)

instance ToHaskell TypeDefinition where
  toHaskell :: TypeDefinition -> String
  toHaskell (ProperType x) = x
  toHaskell (TypeConstructor x) = toHaskell x

data DataDeclaration = DataDeclaration
  { _typeDefinition :: TypeDefinition
  , _constructors :: [FunctionApplication]
  } deriving (Eq)

newtype ArgumentList =
  ArgumentList [Expression]
  deriving (Eq)

instance ToHaskell ArgumentList where
  toHaskell :: ArgumentList -> String
  toHaskell (ArgumentList arguments) = concatMap toHaskell arguments

data FunctionDefinition = FunctionDefinition
  { _name :: Identifier
  , _typeSignature :: FunctionApplication
  , _definitions :: [(ArgumentList, Expression)]
  } deriving (Eq)

data Import
  = ImportItem Identifier
  | ImportType Identifier
               [Identifier]
  deriving (Eq)

instance ToHaskell Import where
  toHaskell :: Import -> String
  toHaskell (ImportItem x) =
    if isOperator x
      then surround Parentheses x
      else x
  toHaskell (ImportType typeName imports) =
    typeName <> surround Parentheses (delimit Commas imports)

newtype ImportList =
  ImportList [Import]
  deriving (Eq)

instance ToHaskell ImportList where
  toHaskell :: ImportList -> String
  toHaskell (ImportList importList) =
    surround Parentheses $ delimit Commas $ map toHaskell importList

newtype LanguagePragma = LanguagePragma
  { _language :: Identifier
  } deriving (Eq)

data LetBlock = LetBlock
  { _bindings :: [(Identifier, Expression)]
  , _body :: Expression
  } deriving (Eq)

data MacroDefinition = MacroDefinition
  { _name :: Identifier
  , _definitions :: [(ArgumentList, Expression)]
  } deriving (Eq)

data QualifiedImport = QualifiedImport
  { _moduleName :: Identifier
  , _alias :: Identifier
  , _imports :: ImportList
  } deriving (Eq)

data RestrictedImport = RestrictedImport
  { _moduleName :: Identifier
  , _imports :: ImportList
  } deriving (Eq)

data TypeclassInstance = TypeclassInstance
  { _instanceName :: Expression
  , _definitions :: [FunctionDefinition]
  } deriving (Eq)

data TypeSynonym = TypeSynonym
  { _alias :: Expression
  , _definition :: Expression
  } deriving (Eq)

data Expression
  = EEmptySExpression
  | EFunctionApplication FunctionApplication
  | EIdentifier Identifier
  | ELetBlock LetBlock
  | ELiteral Literal
  deriving (Eq)

instance ToHaskell Expression where
  toHaskell :: Expression -> String
  toHaskell EEmptySExpression = "()"
  toHaskell (EFunctionApplication x) = toHaskell x
  toHaskell (EIdentifier x) =
    if isOperator x
      then surround Parentheses x
      else x
  toHaskell (ELetBlock x) = toHaskell x
  toHaskell (ELiteral x) = toHaskell x

data Literal
  = LChar Char
  | LInt Int
  | LList [Expression]
  | LString String
  deriving (Eq)

instance ToHaskell Literal where
  toHaskell :: Literal -> String
  toHaskell (LChar x) = surround SingleQuotes [x]
  toHaskell (LInt x) = show x
  toHaskell (LList xs) =
    surround SquareBrackets $ delimit Commas $ map toHaskell xs
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
  deriving (Eq)

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
  toHaskell (SUnrestrictedImport x) = show x

type Program = [Statement]

makeFieldsNoPrefix ''DataDeclaration

makeFieldsNoPrefix ''FunctionApplication

makeFieldsNoPrefix ''FunctionDefinition

makeFieldsNoPrefix ''LanguagePragma

makeFieldsNoPrefix ''LetBlock

makeFieldsNoPrefix ''MacroDefinition

makeFieldsNoPrefix ''QualifiedImport

makeFieldsNoPrefix ''RestrictedImport

makeFieldsNoPrefix ''TopLevel

makeFieldsNoPrefix ''TypeclassInstance

makeFieldsNoPrefix ''TypeSynonym

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
    delimit Pipes (map toHaskell $ dataDeclaration ^. constructors)

instance ToHaskell LanguagePragma where
  toHaskell :: LanguagePragma -> String
  toHaskell languagePragma =
    renderPragma $ "LANGUAGE " <> languagePragma ^. language

instance ToHaskell LetBlock where
  toHaskell :: LetBlock -> String
  toHaskell letBlock =
    "let " <> renderBlock (map bindingToHaskell (letBlock ^. bindings)) <>
    " in " <>
    toHaskell (letBlock ^. body)
    where
      bindingToHaskell (identifier, value) =
        identifier <> " = " <> toHaskell value

instance ToHaskell MacroDefinition where
  toHaskell :: MacroDefinition -> String
  toHaskell macroDefinition =
    delimit Newlines $
    (macroDefinition ^. name <> " :: [Expression] -> IO [Expression]") :
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
