{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

{-
* Macros
 Macros edit the parse tree, which the compiler converts to an AST which is transpiled into Eta.
 1) Top-level macros are run, with the code forms represented as a lists of parse expressions (lists, symbols, and literals).
    The output of the macro programs is rendered as a list and inserted into the file.
 2) Repeat step 1 until no more macros are available.
-}
module Lihsp.AST where

import Control.Lens.Operators ((^.))
import Control.Lens.TH (makeFieldsNoPrefix)

import Data.Semigroup ((<>))

import Lihsp.Utils
       (Bracket(Parentheses, SingleQuotes, SquareBrackets),
        Delimiter(Commas, Pipes), delimit, renderBlock, surround)

type Identifier = String

data FunctionApplication = FunctionApplication
  { _function :: Expression
  , _arguments :: [Expression]
  }

data DataDeclaration = DataDeclaration
  { _typeDefinition :: FunctionApplication
  , _constructors :: [FunctionApplication]
  }

newtype ArgumentList =
  ArgumentList [Expression]

instance Show ArgumentList where
  show :: ArgumentList -> String
  show (ArgumentList arguments) = concatMap show arguments

newtype FunctionDefinition =
  FunctionDefinition (Identifier, [(ArgumentList, Expression)])

instance Show FunctionDefinition where
  show :: FunctionDefinition -> String
  show (FunctionDefinition (name, cases)) = renderBlock $ map showCase cases
    where
      showCase (pattern', body) = show name <> show pattern' <> show body

newtype ImportList =
  ImportList [Identifier]

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
  { _moduleName :: Expression
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
  show (EIdentifier x) = surround Parentheses x
  show (ELetBlock x) = show x
  show (ELiteral x) = surround Parentheses $ show x

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
  show (SModuleDeclaration x) = show x
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
    show (functionApplication ^. function) <>
    concatMap show (functionApplication ^. arguments)

instance Show DataDeclaration where
  show :: DataDeclaration -> String
  show dataDeclaration =
    "data " <> show (dataDeclaration ^. typeDefinition) <> "=" <>
    delimit Pipes (map show $ dataDeclaration ^. constructors)

instance Show LanguagePragma where
  show :: LanguagePragma -> String
  show languagePragma = "{#-LANGUAGE" <> (languagePragma ^. language) <> "#-}"

instance Show LetBlock where
  show :: LetBlock -> String
  show letBlock =
    "let " <> renderBlock (map showBinding (letBlock ^. bindings)) <> "in" <>
    show (letBlock ^. body)
    where
      showBinding (identifier, value) = identifier <> "=" <> show value

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
    "instance" <> show (typeclassInstance ^. moduleName) <> "where" <>
    renderBlock (map show $ typeclassInstance ^. definitions)

instance Show TypeSynonym where
  show :: TypeSynonym -> String
  show typeSynonym =
    "type" <> show (typeSynonym ^. alias) <> "=" <>
    show (typeSynonym ^. definition)
