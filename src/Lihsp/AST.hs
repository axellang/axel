{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Lihsp.AST where

import Control.Lens.Operators ((^.))
import Control.Lens.TH (makeFieldsNoPrefix)

import Data.Semigroup ((<>))

import Lihsp.Utils
       (Bracket(Parentheses, SingleQuotes, SquareBrackets),
        Delimiter(Commas, Newlines, Pipes, Spaces), delimit, isOperator,
        renderBlock, renderPragma, surround)

type Identifier = String

data FunctionApplication = FunctionApplication
  { _function :: Expression
  , _arguments :: [Expression]
  }

data TypeDefinition
  = ProperType Identifier
  | TypeConstructor FunctionApplication

instance Show TypeDefinition where
  show :: TypeDefinition -> String
  show (ProperType x) = x
  show (TypeConstructor x) = show x

data DataDeclaration = DataDeclaration
  { _typeDefinition :: TypeDefinition
  , _constructors :: [FunctionApplication]
  }

newtype ArgumentList =
  ArgumentList [Expression]

instance Show ArgumentList where
  show :: ArgumentList -> String
  show (ArgumentList arguments) = concatMap show arguments

data FunctionDefinition = FunctionDefinition
  { _name :: Identifier
  , _typeSignature :: FunctionApplication
  , _definitions :: [(ArgumentList, Expression)]
  }

data Import
  = ImportItem Identifier
  | ImportType Identifier
               [Identifier]

instance Show Import where
  show :: Import -> String
  show (ImportItem x) =
    if isOperator x
      then surround Parentheses x
      else x
  show (ImportType typeName imports) =
    typeName <> surround Parentheses (delimit Commas imports)

newtype ImportList =
  ImportList [Import]

instance Show ImportList where
  show :: ImportList -> String
  show (ImportList importList) =
    surround Parentheses $ delimit Commas $ map show importList

newtype LanguagePragma = LanguagePragma
  { _language :: Identifier
  }

data LetBlock = LetBlock
  { _bindings :: [(Identifier, Expression)]
  , _body :: Expression
  }

data QualifiedImport = QualifiedImport
  { _moduleName :: Identifier
  , _alias :: Identifier
  , _imports :: ImportList
  }

data RestrictedImport = RestrictedImport
  { _moduleName :: Identifier
  , _imports :: ImportList
  }

data TypeclassInstance = TypeclassInstance
  { _instanceName :: Expression
  , _definitions :: [FunctionDefinition]
  }

data TypeSynonym = TypeSynonym
  { _alias :: Expression
  , _definition :: Expression
  }

data Expression
  = EFunctionApplication FunctionApplication
  | EIdentifier Identifier
  | ELetBlock LetBlock
  | ELiteral Literal

instance Show Expression where
  show :: Expression -> String
  show (EFunctionApplication x) = show x
  show (EIdentifier x) =
    if isOperator x
      then surround Parentheses x
      else x
  show (ELetBlock x) = show x
  show (ELiteral x) = show x

data Literal
  = LChar Char
  | LInt Int
  | LList [Expression]

instance Show Literal where
  show :: Literal -> String
  show (LInt int) = show int
  show (LChar char) = surround SingleQuotes [char]
  show (LList list) = surround SquareBrackets $ delimit Commas (map show list)

data Statement
  = SDataDeclaration DataDeclaration
  | SFunctionDefinition FunctionDefinition
  | SLanguagePragma LanguagePragma
  | SModuleDeclaration Identifier
  | SQualifiedImport QualifiedImport
  | SRestrictedImport RestrictedImport
  | STypeclassInstance TypeclassInstance
  | STypeSynonym TypeSynonym
  | SUnrestrictedImport Identifier

instance Show Statement where
  show :: Statement -> String
  show (SDataDeclaration x) = show x
  show (SFunctionDefinition x) = show x
  show (SLanguagePragma x) = show x
  show (SModuleDeclaration x) = "module " <> x <> " where"
  show (SQualifiedImport x) = show x
  show (SRestrictedImport x) = show x
  show (STypeclassInstance x) = show x
  show (STypeSynonym x) = show x
  show (SUnrestrictedImport x) = show x

type Program = [Statement]

makeFieldsNoPrefix ''DataDeclaration

makeFieldsNoPrefix ''FunctionApplication

makeFieldsNoPrefix ''FunctionDefinition

makeFieldsNoPrefix ''LanguagePragma

makeFieldsNoPrefix ''LetBlock

makeFieldsNoPrefix ''QualifiedImport

makeFieldsNoPrefix ''RestrictedImport

makeFieldsNoPrefix ''TypeclassInstance

makeFieldsNoPrefix ''TypeSynonym

instance Show FunctionApplication where
  show :: FunctionApplication -> String
  show functionApplication =
    surround Parentheses $
    show (functionApplication ^. function) <> " " <>
    delimit Spaces (map show $ functionApplication ^. arguments)

instance Show FunctionDefinition where
  show :: FunctionDefinition -> String
  show functionDefinition =
    delimit Newlines $
    (functionDefinition ^. name <> " :: " <>
     show (functionDefinition ^. typeSignature)) :
    map showDefinition (functionDefinition ^. definitions)
    where
      showDefinition (pattern', definitionBody) =
        functionDefinition ^. name <> " " <> show pattern' <> " = " <>
        show definitionBody

instance Show DataDeclaration where
  show :: DataDeclaration -> String
  show dataDeclaration =
    "data " <> show (dataDeclaration ^. typeDefinition) <> " = " <>
    delimit Pipes (map show $ dataDeclaration ^. constructors)

instance Show LanguagePragma where
  show :: LanguagePragma -> String
  show languagePragma = renderPragma $ "LANGUAGE " <> languagePragma ^. language

instance Show LetBlock where
  show :: LetBlock -> String
  show letBlock =
    "let " <> renderBlock (map showBinding (letBlock ^. bindings)) <> " in " <>
    show (letBlock ^. body)
    where
      showBinding (identifier, value) = identifier <> " = " <> show value

instance Show QualifiedImport where
  show :: QualifiedImport -> String
  show qualifiedImport =
    "import " <> qualifiedImport ^. moduleName <> " as " <> qualifiedImport ^.
    alias <>
    show (qualifiedImport ^. imports)

instance Show RestrictedImport where
  show :: RestrictedImport -> String
  show restrictedImport =
    "import " <> restrictedImport ^. moduleName <>
    show (restrictedImport ^. imports)

instance Show TypeclassInstance where
  show :: TypeclassInstance -> String
  show typeclassInstance =
    "instance " <> show (typeclassInstance ^. instanceName) <> " where " <>
    renderBlock (map show $ typeclassInstance ^. definitions)

instance Show TypeSynonym where
  show :: TypeSynonym -> String
  show typeSynonym =
    "type " <> show (typeSynonym ^. alias) <> " = " <>
    show (typeSynonym ^. definition)
