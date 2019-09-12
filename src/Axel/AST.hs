{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- TODO Should this module (as well as the corresponding `denormalize*` utilities)
--      be exposed during the expansion phase, so that a higher-level (and more typesafe)
--      representation of the AST can be used if the user so wishes?
module Axel.AST where

import Axel.Haskell.Language (isOperator)
import Axel.Haskell.Macros (hygenisizeMacroName)
import qualified Axel.Parse.AST as Parse
  ( Expression(LiteralChar, LiteralInt, LiteralString, SExpression,
           Symbol)
  )
import Axel.Sourcemap
  ( Bracket(CurlyBraces, DoubleQuotes, Parentheses, SingleQuotes,
        SquareBrackets)
  , Delimiter(Commas, Newlines, Pipes, Spaces)
  )
import qualified Axel.Sourcemap as SM
  ( Expression
  , Output(Output)
  , SourceMetadata
  , delimit
  , renderBlock
  , surround
  )
import qualified Axel.Utils.Display as Display (delimit, renderPragma, surround)
import Axel.Utils.String (handleStringEscapes)
import Axel.Utils.Tuple (annotate, unannotated)

import Control.Lens.Combinators (_head, _last)
import Control.Lens.Operators ((%~), (^.))
import Control.Lens.TH (makeFieldsNoPrefix, makePrisms)
import Control.Lens.Wrapped (_Wrapped)
import Control.Monad ((>=>))

import Data.Data (Data)
import Data.Semigroup ((<>))

class ToHaskell a where
  toHaskell :: a -> SM.Output

type Identifier = String

data CaseBlock ann =
  CaseBlock
    { _ann :: ann
    , _expr :: Expression ann
    , _matches :: [(Expression ann, Expression ann)]
    }
  deriving (Data, Eq, Functor, Show)

data FunctionApplication ann =
  FunctionApplication
    { _ann :: ann
    , _function :: Expression ann
    , _arguments :: [Expression ann]
    }
  deriving (Data, Eq, Functor, Show)

data IfBlock ann =
  IfBlock
    { _ann :: ann
    , _cond :: Expression ann
    , _ifTrue :: Expression ann
    , _ifFalse :: Expression ann
    }
  deriving (Data, Eq, Functor, Show)

data TopLevel ann =
  TopLevel
    { _ann :: ann
    , _statements :: [Statement ann]
    }
  deriving (Data, Eq, Functor, Show)

data TypeDefinition ann
  = ProperType ann Identifier
  | TypeConstructor ann (FunctionApplication ann)
  deriving (Data, Eq, Functor, Show)

data DataDeclaration ann =
  DataDeclaration
    { _ann :: ann
    , _typeDefinition :: TypeDefinition ann
    , _constructors :: [FunctionApplication ann]
    }
  deriving (Data, Eq, Functor, Show)

data NewtypeDeclaration ann =
  NewtypeDeclaration
    { _ann :: ann
    , _typeDefinition :: TypeDefinition ann
    , _constructor :: FunctionApplication ann
    }
  deriving (Data, Eq, Functor, Show)

data FunctionDefinition ann =
  FunctionDefinition
    { _ann :: ann
    , _name :: Identifier
    , _arguments :: [Expression ann]
    , _body :: Expression ann
    , _whereBindings :: [FunctionDefinition ann]
    }
  deriving (Data, Eq, Functor, Show)

data Import ann
  = ImportItem ann Identifier
  | ImportType ann Identifier [Identifier]
  deriving (Data, Eq, Functor, Show)

data ImportSpecification ann
  = ImportAll ann
  | ImportOnly ann [Import ann]
  deriving (Data, Eq, Functor, Show)

data Lambda ann =
  Lambda
    { _ann :: ann
    , _arguments :: [Expression ann]
    , _body :: Expression ann
    }
  deriving (Data, Eq, Functor, Show)

data LetBlock ann =
  LetBlock
    { _ann :: ann
    , _bindings :: [(Expression ann, Expression ann)]
    , _body :: Expression ann
    }
  deriving (Data, Eq, Functor, Show)

data MacroDefinition ann =
  MacroDefinition
    { _ann :: ann
    , _functionDefinition :: FunctionDefinition ann
    }
  deriving (Data, Eq, Functor, Show)

data MacroImport ann =
  MacroImport
    { _ann :: ann
    , _moduleName :: Identifier
    , _imports :: [Identifier]
    }
  deriving (Data, Eq, Functor, Show)

data Pragma ann =
  Pragma
    { _ann :: ann
    , _pragmaSpecification :: String
    }
  deriving (Data, Eq, Functor, Show)

data QualifiedImport ann =
  QualifiedImport
    { _ann :: ann
    , _moduleName :: Identifier
    , _alias :: Identifier
    , _imports :: ImportSpecification ann
    }
  deriving (Data, Eq, Functor, Show)

data RecordDefinition ann =
  RecordDefinition
    { _ann :: ann
    , _bindings :: [(Identifier, Expression ann)]
    }
  deriving (Data, Eq, Functor, Show)

data RecordType ann =
  RecordType
    { _ann :: ann
    , _fields :: [(Identifier, Expression ann)]
    }
  deriving (Data, Eq, Functor, Show)

data RestrictedImport ann =
  RestrictedImport
    { _ann :: ann
    , _moduleName :: Identifier
    , _imports :: ImportSpecification ann
    }
  deriving (Data, Eq, Functor, Show)

data TypeclassDefinition ann =
  TypeclassDefinition
    { _ann :: ann
    , _name :: Expression ann
    , _constraints :: [Expression ann]
    , _signatures :: [TypeSignature ann]
    }
  deriving (Data, Eq, Functor, Show)

data TypeclassInstance ann =
  TypeclassInstance
    { _ann :: ann
    , _instanceName :: Expression ann
    , _definitions :: [FunctionDefinition ann]
    }
  deriving (Data, Eq, Functor, Show)

data TypeSignature ann =
  TypeSignature
    { _ann :: ann
    , _name :: Identifier
    , _typeDefinition :: Expression ann
    }
  deriving (Data, Eq, Functor, Show)

data TypeSynonym ann =
  TypeSynonym
    { _ann :: ann
    , _alias :: Expression ann
    , _definition :: Expression ann
    }
  deriving (Data, Eq, Functor, Show)

data Expression ann
  = ECaseBlock (CaseBlock ann)
  | EEmptySExpression ann
  | EFunctionApplication (FunctionApplication ann)
  | EIdentifier ann Identifier
  | EIfBlock (IfBlock ann)
  | ELambda (Lambda ann)
  | ELetBlock (LetBlock ann)
  | ELiteral (Literal ann)
  | ERawExpression ann String
  | ERecordDefinition (RecordDefinition ann)
  | ERecordType (RecordType ann)
  deriving (Data, Eq, Functor, Show)

data Literal ann
  = LChar ann Char
  | LInt ann Int
  | LString ann String
  deriving (Data, Eq, Functor, Show)

data Statement ann
  = STopLevel (TopLevel ann)
  | SDataDeclaration (DataDeclaration ann)
  | SFunctionDefinition (FunctionDefinition ann)
  | SMacroDefinition (MacroDefinition ann)
  | SMacroImport (MacroImport ann)
  | SModuleDeclaration ann Identifier
  | SNewtypeDeclaration (NewtypeDeclaration ann)
  | SPragma (Pragma ann)
  | SQualifiedImport (QualifiedImport ann)
  | SRawStatement ann String
  | SRestrictedImport (RestrictedImport ann)
  | STypeclassDefinition (TypeclassDefinition ann)
  | STypeclassInstance (TypeclassInstance ann)
  | STypeSignature (TypeSignature ann)
  | STypeSynonym (TypeSynonym ann)
  | SUnrestrictedImport ann Identifier
  deriving (Data, Eq, Functor, Show)

type Program ann = [Statement ann]

makePrisms ''Statement

makeFieldsNoPrefix ''CaseBlock

makeFieldsNoPrefix ''TopLevel

makeFieldsNoPrefix ''DataDeclaration

makeFieldsNoPrefix ''FunctionApplication

makeFieldsNoPrefix ''FunctionDefinition

makeFieldsNoPrefix ''Lambda

makeFieldsNoPrefix ''LetBlock

makeFieldsNoPrefix ''IfBlock

makeFieldsNoPrefix ''MacroDefinition

makeFieldsNoPrefix ''MacroImport

makeFieldsNoPrefix ''NewtypeDeclaration

makeFieldsNoPrefix ''Pragma

makeFieldsNoPrefix ''QualifiedImport

makeFieldsNoPrefix ''RecordDefinition

makeFieldsNoPrefix ''RecordType

makeFieldsNoPrefix ''RestrictedImport

makeFieldsNoPrefix ''TypeclassDefinition

makeFieldsNoPrefix ''TypeclassInstance

makeFieldsNoPrefix ''TypeSignature

makeFieldsNoPrefix ''TypeSynonym

type SMExpression = Expression (Maybe SM.Expression)

type SMStatement = Statement (Maybe SM.Expression)

-- TODO Instead of using `HasAnnotation`, manually implement `HasAnn`
--      for everything instead.
class HasAnnotation a ann | a -> ann where
  getAnn :: a -> ann

-- TODO Figure out another way to do this,
--      since the current implementation needs overlapping instances.
instance {-# OVERLAPPABLE #-} (HasAnn a ann) => HasAnnotation a ann where
  getAnn :: a -> ann
  getAnn = (^. ann)

instance {-# OVERLAPPING #-} HasAnnotation (Expression ann) ann where
  getAnn :: Expression ann -> ann
  getAnn (ECaseBlock caseBlock) = caseBlock ^. ann
  getAnn (EEmptySExpression ann') = ann'
  getAnn (EFunctionApplication fnApp) = fnApp ^. ann
  getAnn (EIdentifier ann' _) = ann'
  getAnn (EIfBlock ifBlock) = ifBlock ^. ann
  getAnn (ELambda lambda) = lambda ^. ann
  getAnn (ELetBlock letBlock) = letBlock ^. ann
  getAnn (ELiteral literal) = getAnn literal
  getAnn (ERawExpression ann' _) = ann'
  getAnn (ERecordDefinition recordDefinition) = recordDefinition ^. ann
  getAnn (ERecordType recordType) = recordType ^. ann

instance {-# OVERLAPPING #-} HasAnnotation (Statement ann) ann where
  getAnn :: Statement ann -> ann
  getAnn (STopLevel topLevel) = getAnn topLevel
  getAnn (SDataDeclaration dataDeclaration) = getAnn dataDeclaration
  getAnn (SFunctionDefinition fnDef) = getAnn fnDef
  getAnn (SMacroDefinition macroDef) = getAnn macroDef
  getAnn (SMacroImport macroImport) = getAnn macroImport
  getAnn (SModuleDeclaration ann' _) = ann'
  getAnn (SNewtypeDeclaration newtypeDeclaration) = getAnn newtypeDeclaration
  getAnn (SPragma pragma) = getAnn pragma
  getAnn (SQualifiedImport qualifiedImport) = getAnn qualifiedImport
  getAnn (SRawStatement ann' _) = ann'
  getAnn (SRestrictedImport restrictedImport) = getAnn restrictedImport
  getAnn (STypeclassDefinition typeclassDefinition) = getAnn typeclassDefinition
  getAnn (STypeclassInstance typeclassInstance) = getAnn typeclassInstance
  getAnn (STypeSignature typeSig) = getAnn typeSig
  getAnn (STypeSynonym typeSynonym) = getAnn typeSynonym
  getAnn (SUnrestrictedImport ann' _) = ann'

instance {-# OVERLAPPING #-} HasAnnotation (Parse.Expression ann) ann where
  getAnn :: Parse.Expression ann -> ann
  getAnn (Parse.LiteralChar ann' _) = ann'
  getAnn (Parse.LiteralInt ann' _) = ann'
  getAnn (Parse.LiteralString ann' _) = ann'
  getAnn (Parse.SExpression ann' _) = ann'
  getAnn (Parse.Symbol ann' _) = ann'

instance {-# OVERLAPPING #-} HasAnnotation (Literal ann) ann where
  getAnn :: Literal ann -> ann
  getAnn (LChar ann' _) = ann'
  getAnn (LInt ann' _) = ann'
  getAnn (LString ann' _) = ann'

instance {-# OVERLAPPING #-} HasAnnotation (TypeDefinition ann) ann where
  getAnn :: TypeDefinition ann -> ann
  getAnn (ProperType ann' _) = ann'
  getAnn (TypeConstructor ann' _) = ann'

instance {-# OVERLAPPING #-} HasAnnotation (ImportSpecification ann) ann where
  getAnn :: ImportSpecification ann -> ann
  getAnn (ImportAll ann') = ann'
  getAnn (ImportOnly ann' _) = ann'

instance {-# OVERLAPPING #-} HasAnnotation (Import ann) ann where
  getAnn :: Import ann -> ann
  getAnn (ImportItem ann' _) = ann'
  getAnn (ImportType ann' _ _) = ann'

-- | Get a nested annotation, where the higher-level annotation is behind a `Maybe`.
getAnn' :: (HasAnnotation a (Maybe SM.Expression)) => a -> SM.SourceMetadata
getAnn' = getAnn >=> getAnn

mkHaskell :: (HasAnnotation a (Maybe SM.Expression)) => a -> String -> SM.Output
mkHaskell x haskellRendering = SM.Output [annotate (getAnn' x) haskellRendering]

instance ToHaskell SMStatement where
  toHaskell :: SMStatement -> SM.Output
  toHaskell (STopLevel xs) = toHaskell xs
  toHaskell (SDataDeclaration x) = toHaskell x
  toHaskell (SFunctionDefinition x) = toHaskell x
  toHaskell (SPragma x) = toHaskell x
  toHaskell (SMacroDefinition x) = toHaskell x
  toHaskell (SMacroImport x) = toHaskell x
  toHaskell stmt@(SModuleDeclaration _ x) =
    mkHaskell stmt $ "module " <> x <> " where"
  toHaskell (SNewtypeDeclaration x) = toHaskell x
  toHaskell (SQualifiedImport x) = toHaskell x
  toHaskell stmt@(SRawStatement _ x) = mkHaskell stmt x
  toHaskell (SRestrictedImport x) = toHaskell x
  toHaskell (STypeclassDefinition x) = toHaskell x
  toHaskell (STypeclassInstance x) = toHaskell x
  toHaskell (STypeSignature x) = toHaskell x
  toHaskell (STypeSynonym x) = toHaskell x
  toHaskell stmt@(SUnrestrictedImport _ x) = mkHaskell stmt $ "import " <> x

instance ToHaskell (TypeDefinition (Maybe SM.Expression)) where
  toHaskell :: TypeDefinition (Maybe SM.Expression) -> SM.Output
  toHaskell (TypeConstructor _ x) = toHaskell x
  toHaskell stmt@(ProperType _ x) = mkHaskell stmt x

instance ToHaskell (CaseBlock (Maybe SM.Expression)) where
  toHaskell :: CaseBlock (Maybe SM.Expression) -> SM.Output
  toHaskell caseBlock =
    SM.surround Parentheses $ mkHaskell caseBlock "case " <>
    toHaskell (caseBlock ^. expr) <>
    mkHaskell caseBlock " of " <>
    SM.renderBlock (map matchToHaskell (caseBlock ^. matches))
    where
      matchToHaskell (pat, result) =
        toHaskell pat <> mkHaskell caseBlock " -> " <> toHaskell result

instance ToHaskell (FunctionApplication (Maybe SM.Expression)) where
  toHaskell :: FunctionApplication (Maybe SM.Expression) -> SM.Output
  toHaskell functionApplication =
    case functionApplication ^. function of
      EIdentifier _ "list" ->
        SM.surround SquareBrackets $
        SM.delimit Commas (map toHaskell $ functionApplication ^. arguments)
      _ ->
        SM.surround Parentheses $ toHaskell (functionApplication ^. function) <>
        mkHaskell functionApplication " " <>
        SM.delimit Spaces (map toHaskell $ functionApplication ^. arguments)

instance ToHaskell (Literal (Maybe SM.Expression)) where
  toHaskell :: Literal (Maybe SM.Expression) -> SM.Output
  toHaskell literal@(LChar _ x) =
    mkHaskell literal $ Display.surround SingleQuotes (handleStringEscapes [x])
  toHaskell literal@(LInt _ x) = mkHaskell literal $ show x
  toHaskell literal@(LString _ x) =
    mkHaskell literal $ Display.surround DoubleQuotes (handleStringEscapes x)

instance ToHaskell (TypeSignature (Maybe SM.Expression)) where
  toHaskell :: TypeSignature (Maybe SM.Expression) -> SM.Output
  toHaskell typeSignature =
    toHaskell (EIdentifier (typeSignature ^. ann) (typeSignature ^. name)) <>
    mkHaskell typeSignature " :: " <>
    toHaskell (typeSignature ^. typeDefinition)

instance ToHaskell (FunctionDefinition (Maybe SM.Expression)) where
  toHaskell :: FunctionDefinition (Maybe SM.Expression) -> SM.Output
  toHaskell fnDef =
    toHaskell (EIdentifier (fnDef ^. ann) (fnDef ^. name)) <>
    mkHaskell fnDef " " <>
    SM.delimit Spaces (map toHaskell (fnDef ^. arguments)) <>
    mkHaskell fnDef " = " <>
    toHaskell (fnDef ^. body) <>
    auxBindings
    where
      auxBindings =
        if null (fnDef ^. whereBindings)
          then mempty
          else mkHaskell fnDef " where " <>
               SM.renderBlock (map toHaskell (fnDef ^. whereBindings))

instance ToHaskell (DataDeclaration (Maybe SM.Expression)) where
  toHaskell :: DataDeclaration (Maybe SM.Expression) -> SM.Output
  toHaskell dataDeclaration =
    mkHaskell dataDeclaration "data " <>
    toHaskell (dataDeclaration ^. typeDefinition) <>
    mkHaskell dataDeclaration " = " <>
    SM.delimit
      Pipes
      (map (removeSurroundingParentheses . toHaskell) $ dataDeclaration ^.
       constructors)

removeSurroundingParentheses :: SM.Output -> SM.Output
removeSurroundingParentheses = removeOpen . removeClosed
  where
    removeOpen = _Wrapped . _head . unannotated %~ tail
    removeClosed = _Wrapped . _last . unannotated %~ init

instance ToHaskell (IfBlock (Maybe SM.Expression)) where
  toHaskell :: IfBlock (Maybe SM.Expression) -> SM.Output
  toHaskell ifBlock =
    mkHaskell ifBlock "if " <> toHaskell (ifBlock ^. cond) <>
    mkHaskell ifBlock " then " <>
    toHaskell (ifBlock ^. ifTrue) <>
    mkHaskell ifBlock " else " <>
    toHaskell (ifBlock ^. ifFalse)

instance ToHaskell (NewtypeDeclaration (Maybe SM.Expression)) where
  toHaskell :: NewtypeDeclaration (Maybe SM.Expression) -> SM.Output
  toHaskell newtypeDeclaration =
    mkHaskell newtypeDeclaration "newtype " <>
    toHaskell (newtypeDeclaration ^. typeDefinition) <>
    mkHaskell newtypeDeclaration " = " <>
    removeSurroundingParentheses (toHaskell (newtypeDeclaration ^. constructor))

instance ToHaskell (Lambda (Maybe SM.Expression)) where
  toHaskell :: Lambda (Maybe SM.Expression) -> SM.Output
  toHaskell lambda =
    SM.surround Parentheses $ mkHaskell lambda "\\" <>
    SM.delimit Spaces (map toHaskell (lambda ^. arguments)) <>
    mkHaskell lambda " -> " <>
    toHaskell (lambda ^. body)

instance ToHaskell (Pragma (Maybe SM.Expression)) where
  toHaskell :: Pragma (Maybe SM.Expression) -> SM.Output
  toHaskell pragma =
    mkHaskell pragma $ Display.renderPragma (pragma ^. pragmaSpecification)

instance ToHaskell (LetBlock (Maybe SM.Expression)) where
  toHaskell :: LetBlock (Maybe SM.Expression) -> SM.Output
  toHaskell letBlock =
    SM.surround Parentheses $ mkHaskell letBlock "let " <>
    SM.renderBlock (map bindingToHaskell (letBlock ^. bindings)) <>
    mkHaskell letBlock " in " <>
    toHaskell (letBlock ^. body)
    where
      bindingToHaskell (pattern', value) =
        toHaskell pattern' <> mkHaskell letBlock " = " <> toHaskell value

instance ToHaskell (MacroDefinition (Maybe SM.Expression)) where
  toHaskell :: MacroDefinition (Maybe SM.Expression) -> SM.Output
  toHaskell macroDefinition = toHaskell (macroDefinition ^. functionDefinition)

instance ToHaskell (MacroImport (Maybe SM.Expression)) where
  toHaskell :: MacroImport (Maybe SM.Expression) -> SM.Output
  toHaskell macroImport =
    toHaskell $
    RestrictedImport
      (macroImport ^. ann)
      (macroImport ^. moduleName)
      (ImportOnly (macroImport ^. ann) $
       map (ImportItem (macroImport ^. ann) . hygenisizeMacroName) $
       macroImport ^.
       imports)

instance ToHaskell (ImportSpecification (Maybe SM.Expression)) where
  toHaskell :: ImportSpecification (Maybe SM.Expression) -> SM.Output
  toHaskell importSpec@(ImportAll _) = mkHaskell importSpec ""
  toHaskell (ImportOnly _ importList) =
    SM.surround Parentheses $ SM.delimit Commas $ map toHaskell importList

instance ToHaskell (QualifiedImport (Maybe SM.Expression)) where
  toHaskell :: QualifiedImport (Maybe SM.Expression) -> SM.Output
  toHaskell qualifiedImport =
    mkHaskell
      qualifiedImport
      ("import qualified " <> qualifiedImport ^. moduleName <> " as " <>
       qualifiedImport ^.
       alias) <>
    toHaskell (qualifiedImport ^. imports)

instance ToHaskell (Expression (Maybe SM.Expression)) where
  toHaskell :: Expression (Maybe SM.Expression) -> SM.Output
  toHaskell (ECaseBlock x) = toHaskell x
  toHaskell expr'@(EEmptySExpression _) = mkHaskell expr' "()"
  toHaskell (EFunctionApplication x) = toHaskell x
  toHaskell expr'@(EIdentifier _ x) =
    mkHaskell expr' $
    if isOperator x
      then Display.surround Parentheses x
      else x
  toHaskell (EIfBlock x) = toHaskell x
  toHaskell (ELambda x) = toHaskell x
  toHaskell (ELetBlock x) = toHaskell x
  toHaskell (ELiteral x) = toHaskell x
  toHaskell expr'@(ERawExpression _ x) = mkHaskell expr' x
  toHaskell (ERecordDefinition x) = toHaskell x
  toHaskell (ERecordType x) = toHaskell x

instance ToHaskell (RecordDefinition (Maybe SM.Expression)) where
  toHaskell :: RecordDefinition (Maybe SM.Expression) -> SM.Output
  toHaskell recordDefinition =
    SM.surround CurlyBraces $ SM.delimit Commas $
    map
      (\(var, val) -> mkHaskell recordDefinition (var <> " = ") <> toHaskell val)
      (recordDefinition ^. bindings)

instance ToHaskell (RecordType (Maybe SM.Expression)) where
  toHaskell :: RecordType (Maybe SM.Expression) -> SM.Output
  toHaskell recordDefinition =
    SM.surround CurlyBraces $ SM.delimit Commas $
    map
      (\(field, ty) ->
         mkHaskell recordDefinition (field <> " :: ") <> toHaskell ty)
      (recordDefinition ^. fields)

instance ToHaskell (Import (Maybe SM.Expression)) where
  toHaskell :: Import (Maybe SM.Expression) -> SM.Output
  toHaskell import'@(ImportItem _ x) =
    mkHaskell import' $
    if isOperator x
      then Display.surround Parentheses x
      else x
  toHaskell import'@(ImportType _ typeName imports') =
    mkHaskell import' $ typeName <>
    Display.surround Parentheses (Display.delimit Commas imports')

instance ToHaskell (RestrictedImport (Maybe SM.Expression)) where
  toHaskell :: RestrictedImport (Maybe SM.Expression) -> SM.Output
  toHaskell restrictedImport =
    mkHaskell restrictedImport ("import " <> restrictedImport ^. moduleName) <>
    toHaskell (restrictedImport ^. imports)

instance ToHaskell (TopLevel (Maybe SM.Expression)) where
  toHaskell :: TopLevel (Maybe SM.Expression) -> SM.Output
  toHaskell topLevel =
    SM.delimit Newlines $ map toHaskell (topLevel ^. statements)

instance ToHaskell (TypeclassDefinition (Maybe SM.Expression)) where
  toHaskell :: TypeclassDefinition (Maybe SM.Expression) -> SM.Output
  toHaskell typeclassDefinition =
    mkHaskell typeclassDefinition "class " <>
    SM.surround
      Parentheses
      (SM.delimit Commas (map toHaskell (typeclassDefinition ^. constraints))) <>
    mkHaskell typeclassDefinition " => " <>
    toHaskell (typeclassDefinition ^. name) <>
    mkHaskell typeclassDefinition " where " <>
    SM.renderBlock (map toHaskell $ typeclassDefinition ^. signatures)

instance ToHaskell (TypeclassInstance (Maybe SM.Expression)) where
  toHaskell :: TypeclassInstance (Maybe SM.Expression) -> SM.Output
  toHaskell typeclassInstance =
    mkHaskell typeclassInstance "instance " <>
    toHaskell (typeclassInstance ^. instanceName) <>
    mkHaskell typeclassInstance " where " <>
    SM.renderBlock (map toHaskell $ typeclassInstance ^. definitions)

instance ToHaskell (TypeSynonym (Maybe SM.Expression)) where
  toHaskell :: TypeSynonym (Maybe SM.Expression) -> SM.Output
  toHaskell typeSynonym =
    mkHaskell typeSynonym "type " <> toHaskell (typeSynonym ^. alias) <>
    mkHaskell typeSynonym " = " <>
    toHaskell (typeSynonym ^. definition)

statementsToProgram :: [SMStatement] -> SMStatement
statementsToProgram = STopLevel . TopLevel Nothing
