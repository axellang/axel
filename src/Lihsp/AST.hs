{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Lihsp.AST where

import Control.Lens.Operators ((%~), (^.))
import Control.Lens.TH (makeFieldsNoPrefix)
import Control.Lens.Tuple (_2)

import Data.Function ((&))
import Data.Semigroup ((<>))

import Lihsp.Utils.Display
  ( Bracket(DoubleQuotes, Parentheses, SingleQuotes)
  , Delimiter(Commas, Newlines, Pipes, Spaces)
  , delimit
  , isOperator
  , renderBlock
  , renderPragma
  , surround
  )
import Lihsp.Utils.Recursion
  ( Recursive(bottomUpAny, bottomUpFmap, bottomUpTraverse)
  )

class ToHaskell a where
  toHaskell :: a -> String

type Identifier = String

data FunctionApplication = FunctionApplication
  { _function :: Expression
  , _arguments :: [Expression]
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
  = EFunctionApplication FunctionApplication
  | EIdentifier Identifier
  | ELetBlock LetBlock
  | ELiteral Literal
  deriving (Eq)

instance ToHaskell Expression where
  toHaskell :: Expression -> String
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
  | LString String
  deriving (Eq)

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

makeFieldsNoPrefix ''TypeclassInstance

makeFieldsNoPrefix ''TypeSynonym

instance ToHaskell FunctionApplication where
  toHaskell :: FunctionApplication -> String
  toHaskell functionApplication =
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
    (macroDefinition ^. name <> " :: [Expression] -> IO Expression") :
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
  bottomUpAny :: (Expression -> Bool) -> Expression -> Bool
  bottomUpAny f x =
    case x of
      EFunctionApplication functionApplication ->
        f x ||
        f (functionApplication ^. function) ||
        any f (functionApplication ^. arguments)
      EIdentifier _ -> f x
      ELetBlock letBlock ->
        let checkBindings = any (f . snd) (letBlock ^. bindings)
            checkBody = f (letBlock ^. body)
        in f x || checkBindings || checkBody
      ELiteral _ -> f x
  bottomUpFmap :: (Expression -> Expression) -> Expression -> Expression
  bottomUpFmap f x =
    case x of
      EFunctionApplication functionApplication ->
        f $
        EFunctionApplication
          (functionApplication & function %~ f & arguments %~ map f)
      EIdentifier _ -> f x
      ELetBlock letBlock ->
        let updateBindings = bindings %~ map (_2 %~ f)
            updateBody = body %~ f
        in f $ ELetBlock (updateBindings $ updateBody letBlock)
      ELiteral _ -> f x
  bottomUpTraverse ::
       (Monad m) => (Expression -> m Expression) -> Expression -> m Expression
  bottomUpTraverse f x =
    case x of
      EFunctionApplication functionApplication ->
        f =<<
        (EFunctionApplication <$>
         (FunctionApplication <$> f (functionApplication ^. function) <*>
          traverse (bottomUpTraverse f) (functionApplication ^. arguments)))
      EIdentifier _ -> f x
      ELetBlock letBlock ->
        let updatedBindings =
              traverse (\(var, val) -> (var, ) <$> f val) (letBlock ^. bindings)
            updatedBody = f (letBlock ^. body)
        in f =<< (ELetBlock <$> (LetBlock <$> updatedBindings <*> updatedBody))
      ELiteral _ -> f x

instance Recursive Statement where
  bottomUpAny :: (Statement -> Bool) -> Statement -> Bool
  bottomUpAny f x =
    case x of
      SDataDeclaration dataDeclaration ->
        let checkTypeDefinition = undefined
            checkConstructors =
              any (bottomUpAny f) (dataDeclaration ^. constructors)
        in f x || checkTypeDefinition || checkConstructors
      SFunctionDefinition functionDefinition -> undefined
      SLanguagePragma languagePragma -> undefined
      SMacroDefinition macroDefinition -> undefined
      SModuleDeclaration moduleDeclaration -> undefined
      SQualifiedImport qualifiedImport -> undefined
      SRestrictedImport restrictedImport -> undefined
      STypeclassInstance typeclassInstance -> undefined
      STypeSynonym typeSynonym -> undefined
      SUnrestrictedImport unrestrictedImport -> undefined
  bottomUpFmap :: (Statement -> Statement) -> Statement -> Statement
  bottomUpFmap f x = undefined
  bottomUpTraverse ::
       (Monad m) => (Statement -> m Statement) -> Statement -> m Statement
  bottomUpTraverse f x = undefined
