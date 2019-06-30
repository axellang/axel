{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Axel.AST where

import Axel.Haskell.Language (isOperator)
import Axel.Haskell.Macros (hygenisizeMacroName)
import qualified Axel.Parse.AST as Parse
  ( Expression(LiteralChar, LiteralInt, LiteralString, SExpression,
           Symbol)
  , SourceMetadata
  )
import Axel.Sourcemap
  ( Bracket(CurlyBraces, DoubleQuotes, Parentheses, SingleQuotes,
        SquareBrackets)
  , Delimiter(Commas, Newlines, Pipes, Spaces)
  )
import qualified Axel.Sourcemap as SM
  ( Expression
  , Output(Output)
  , delimit
  , renderBlock
  , surround
  )
import qualified Axel.Utils.Display as Display (delimit, renderPragma, surround)
import Axel.Utils.Recursion (Recursive(bottomUpFmap, bottomUpTraverse))

import Control.Arrow ((***))
import Control.Lens.Combinators (_head, _last)
import Control.Lens.Operators ((%~), (^.))
import Control.Lens.TH (makeFieldsNoPrefix, makePrisms)
import Control.Lens.Tuple (_2)
import Control.Lens.Wrapped (_Wrapped)

import Data.Function ((&))
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
  deriving (Eq, Show)

data FunctionApplication ann =
  FunctionApplication
    { _ann :: ann
    , _function :: Expression ann
    , _arguments :: [Expression ann]
    }
  deriving (Eq, Show)

data IfBlock ann =
  IfBlock
    { _ann :: ann
    , _cond :: Expression ann
    , _ifTrue :: Expression ann
    , _ifFalse :: Expression ann
    }
  deriving (Eq, Show)

data TopLevel ann =
  TopLevel
    { _ann :: ann
    , _statements :: [Statement ann]
    }
  deriving (Eq, Show)

data TypeDefinition ann
  = ProperType ann Identifier
  | TypeConstructor ann (FunctionApplication ann)
  deriving (Eq, Show)

data DataDeclaration ann =
  DataDeclaration
    { _ann :: ann
    , _typeDefinition :: TypeDefinition ann
    , _constructors :: [FunctionApplication ann]
    }
  deriving (Eq, Show)

data NewtypeDeclaration ann =
  NewtypeDeclaration
    { _ann :: ann
    , _typeDefinition :: TypeDefinition ann
    , _constructor :: FunctionApplication ann
    }
  deriving (Eq, Show)

data FunctionDefinition ann =
  FunctionDefinition
    { _ann :: ann
    , _name :: Identifier
    , _arguments :: [Expression ann]
    , _body :: Expression ann
    , _whereBindings :: [FunctionDefinition ann]
    }
  deriving (Eq, Show)

data Import ann
  = ImportItem ann Identifier
  | ImportType ann Identifier [Identifier]
  deriving (Eq, Show)

data ImportSpecification ann
  = ImportAll ann
  | ImportOnly ann [Import ann]
  deriving (Eq, Show)

data Lambda ann =
  Lambda
    { _ann :: ann
    , _arguments :: [Expression ann]
    , _body :: Expression ann
    }
  deriving (Eq, Show)

data LetBlock ann =
  LetBlock
    { _ann :: ann
    , _bindings :: [(Expression ann, Expression ann)]
    , _body :: Expression ann
    }
  deriving (Eq, Show)

data MacroDefinition ann =
  MacroDefinition
    { _ann :: ann
    , _functionDefinition :: FunctionDefinition ann
    }
  deriving (Eq, Show)

data MacroImport ann =
  MacroImport
    { _ann :: ann
    , _moduleName :: Identifier
    , _imports :: [Identifier]
    }
  deriving (Eq, Show)

data Pragma ann =
  Pragma
    { _ann :: ann
    , _pragmaSpecification :: String
    }
  deriving (Eq, Show)

data QualifiedImport ann =
  QualifiedImport
    { _ann :: ann
    , _moduleName :: Identifier
    , _alias :: Identifier
    , _imports :: ImportSpecification ann
    }
  deriving (Eq, Show)

data RecordDefinition ann =
  RecordDefinition
    { _ann :: ann
    , _bindings :: [(Identifier, Expression ann)]
    }
  deriving (Eq, Show)

data RecordType ann =
  RecordType
    { _ann :: ann
    , _fields :: [(Identifier, Expression ann)]
    }
  deriving (Eq, Show)

data RestrictedImport ann =
  RestrictedImport
    { _ann :: ann
    , _moduleName :: Identifier
    , _imports :: ImportSpecification ann
    }
  deriving (Eq, Show)

data TypeclassDefinition ann =
  TypeclassDefinition
    { _ann :: ann
    , _name :: Expression ann
    , _constraints :: [Expression ann]
    , _signatures :: [TypeSignature ann]
    }
  deriving (Eq, Show)

data TypeclassInstance ann =
  TypeclassInstance
    { _ann :: ann
    , _instanceName :: Expression ann
    , _definitions :: [FunctionDefinition ann]
    }
  deriving (Eq, Show)

data TypeSignature ann =
  TypeSignature
    { _ann :: ann
    , _name :: Identifier
    , _typeDefinition :: Expression ann
    }
  deriving (Eq, Show)

data TypeSynonym ann =
  TypeSynonym
    { _ann :: ann
    , _alias :: Expression ann
    , _definition :: Expression ann
    }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data Literal ann
  = LChar ann Char
  | LInt ann Int
  | LString ann String
  deriving (Eq, Show)

data Statement ann
  = SDataDeclaration (DataDeclaration ann)
  | SFunctionDefinition (FunctionDefinition ann)
  | SMacroDefinition (MacroDefinition ann)
  | SMacroImport (MacroImport ann)
  | SModuleDeclaration ann Identifier
  | SNewtypeDeclaration (NewtypeDeclaration ann)
  | SPragma (Pragma ann)
  | SQualifiedImport (QualifiedImport ann)
  | SRawStatement ann String
  | SRestrictedImport (RestrictedImport ann)
  | STopLevel (TopLevel ann)
  | STypeclassDefinition (TypeclassDefinition ann)
  | STypeclassInstance (TypeclassInstance ann)
  | STypeSignature (TypeSignature ann)
  | STypeSynonym (TypeSynonym ann)
  | SUnrestrictedImport ann Identifier
  deriving (Eq, Show)

type Program ann = [Statement ann]

makePrisms ''Statement

makeFieldsNoPrefix ''CaseBlock

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

makeFieldsNoPrefix ''TopLevel

makeFieldsNoPrefix ''TypeclassDefinition

makeFieldsNoPrefix ''TypeclassInstance

makeFieldsNoPrefix ''TypeSignature

makeFieldsNoPrefix ''TypeSynonym

class HasAnnotation a ann | a -> ann where
  getAnn :: a -> ann

instance HasAnnotation (Expression ann) ann where
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

instance HasAnnotation (Statement ann) ann where
  getAnn :: Statement ann -> ann
  getAnn (SDataDeclaration dataDeclaration) = dataDeclaration ^. ann
  getAnn (SFunctionDefinition fnDef) = fnDef ^. ann
  getAnn (SMacroDefinition macroDef) = macroDef ^. ann
  getAnn (SMacroImport macroImport) = macroImport ^. ann
  getAnn (SModuleDeclaration ann' _) = ann'
  getAnn (SNewtypeDeclaration newtypeDeclaration) = newtypeDeclaration ^. ann
  getAnn (SPragma pragma) = pragma ^. ann
  getAnn (SQualifiedImport qualifiedImport) = qualifiedImport ^. ann
  getAnn (SRawStatement ann' _) = ann'
  getAnn (SRestrictedImport restrictedImport) = restrictedImport ^. ann
  getAnn (STopLevel topLevel) = topLevel ^. ann
  getAnn (STypeclassDefinition typeclassDefinition) = typeclassDefinition ^. ann
  getAnn (STypeclassInstance typeclassInstance) = typeclassInstance ^. ann
  getAnn (STypeSignature typeSig) = typeSig ^. ann
  getAnn (STypeSynonym typeSynonym) = typeSynonym ^. ann
  getAnn (SUnrestrictedImport ann' _) = ann'

instance HasAnnotation (Parse.Expression ann) ann where
  getAnn :: Parse.Expression ann -> ann
  getAnn (Parse.LiteralChar ann' _) = ann'
  getAnn (Parse.LiteralInt ann' _) = ann'
  getAnn (Parse.LiteralString ann' _) = ann'
  getAnn (Parse.SExpression ann' _) = ann'
  getAnn (Parse.Symbol ann' _) = ann'

instance HasAnnotation (Literal ann) ann where
  getAnn :: Literal ann -> ann
  getAnn (LChar ann' _) = ann'
  getAnn (LInt ann' _) = ann'
  getAnn (LString ann' _) = ann'

instance HasAnnotation (TypeDefinition ann) ann where
  getAnn :: TypeDefinition ann -> ann
  getAnn (ProperType ann' _) = ann'
  getAnn (TypeConstructor ann' _) = ann'

instance HasAnnotation (ImportSpecification ann) ann where
  getAnn :: ImportSpecification ann -> ann
  getAnn (ImportAll ann') = ann'
  getAnn (ImportOnly ann' _) = ann'

instance HasAnnotation (Import ann) ann where
  getAnn :: Import ann -> ann
  getAnn (ImportItem ann' _) = ann'
  getAnn (ImportType ann' _ _) = ann'

mkHaskell ::
     (HasAnnotation a b, HasAnnotation b Parse.SourceMetadata)
  => a
  -> String
  -> SM.Output
mkHaskell x haskellRendering = SM.Output [(getAnn (getAnn x), haskellRendering)]

mkHaskell' ::
     (HasAnn a b, HasAnnotation b Parse.SourceMetadata)
  => a
  -> String
  -> SM.Output
mkHaskell' x haskellRendering =
  SM.Output [(getAnn (x ^. ann), haskellRendering)]

instance ToHaskell (Statement SM.Expression) where
  toHaskell :: Statement SM.Expression -> SM.Output
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
  toHaskell (STopLevel xs) = toHaskell xs
  toHaskell (STypeclassDefinition x) = toHaskell x
  toHaskell (STypeclassInstance x) = toHaskell x
  toHaskell (STypeSignature x) = toHaskell x
  toHaskell (STypeSynonym x) = toHaskell x
  toHaskell stmt@(SUnrestrictedImport _ x) = mkHaskell stmt $ "import " <> x

instance ToHaskell (TypeDefinition SM.Expression) where
  toHaskell :: TypeDefinition SM.Expression -> SM.Output
  toHaskell (TypeConstructor _ x) = toHaskell x
  toHaskell stmt@(ProperType _ x) = mkHaskell stmt x

instance ToHaskell (CaseBlock SM.Expression) where
  toHaskell :: CaseBlock SM.Expression -> SM.Output
  toHaskell caseBlock =
    SM.surround Parentheses $ mkHaskell' caseBlock "case " <>
    toHaskell (caseBlock ^. expr) <>
    mkHaskell' caseBlock " of " <>
    SM.renderBlock (map matchToHaskell (caseBlock ^. matches))
    where
      matchToHaskell (pat, result) =
        toHaskell pat <> mkHaskell' caseBlock " -> " <> toHaskell result

instance ToHaskell (FunctionApplication SM.Expression) where
  toHaskell :: FunctionApplication SM.Expression -> SM.Output
  toHaskell functionApplication =
    case functionApplication ^. function of
      EIdentifier _ "list" ->
        SM.surround SquareBrackets $
        SM.delimit Commas (map toHaskell $ functionApplication ^. arguments)
      _ ->
        SM.surround Parentheses $ toHaskell (functionApplication ^. function) <>
        mkHaskell' functionApplication " " <>
        SM.delimit Spaces (map toHaskell $ functionApplication ^. arguments)

instance ToHaskell (Literal SM.Expression) where
  toHaskell :: Literal SM.Expression -> SM.Output
  toHaskell literal@(LChar _ x) =
    mkHaskell literal $ Display.surround SingleQuotes [x]
  toHaskell literal@(LInt _ x) = mkHaskell literal $ show x
  toHaskell literal@(LString _ x) =
    mkHaskell literal $ Display.surround DoubleQuotes x

instance ToHaskell (TypeSignature SM.Expression) where
  toHaskell :: TypeSignature SM.Expression -> SM.Output
  toHaskell typeSignature =
    toHaskell (EIdentifier (typeSignature ^. ann) (typeSignature ^. name)) <>
    mkHaskell' typeSignature " :: " <>
    toHaskell (typeSignature ^. typeDefinition)

instance ToHaskell (FunctionDefinition SM.Expression) where
  toHaskell :: FunctionDefinition SM.Expression -> SM.Output
  toHaskell fnDef =
    toHaskell (EIdentifier (fnDef ^. ann) (fnDef ^. name)) <>
    mkHaskell' fnDef " " <>
    SM.delimit Spaces (map toHaskell (fnDef ^. arguments)) <>
    mkHaskell' fnDef " = " <>
    toHaskell (fnDef ^. body) <>
    auxBindings
    where
      auxBindings =
        if null (fnDef ^. whereBindings)
          then mempty
          else mkHaskell' fnDef " where " <>
               SM.renderBlock (map toHaskell (fnDef ^. whereBindings))

instance ToHaskell (DataDeclaration SM.Expression) where
  toHaskell :: DataDeclaration SM.Expression -> SM.Output
  toHaskell dataDeclaration =
    mkHaskell' dataDeclaration "data " <>
    toHaskell (dataDeclaration ^. typeDefinition) <>
    mkHaskell' dataDeclaration " = " <>
    SM.delimit
      Pipes
      (map (removeSurroundingParentheses . toHaskell) $ dataDeclaration ^.
       constructors)

removeSurroundingParentheses :: SM.Output -> SM.Output
removeSurroundingParentheses = removeOpen . removeClosed
  where
    removeOpen = _Wrapped . _head . _2 %~ tail
    removeClosed = _Wrapped . _last . _2 %~ init

instance ToHaskell (IfBlock SM.Expression) where
  toHaskell :: IfBlock SM.Expression -> SM.Output
  toHaskell ifBlock =
    mkHaskell' ifBlock "if " <> toHaskell (ifBlock ^. cond) <>
    mkHaskell' ifBlock " then " <>
    toHaskell (ifBlock ^. ifTrue) <>
    mkHaskell' ifBlock " else " <>
    toHaskell (ifBlock ^. ifFalse)

instance ToHaskell (NewtypeDeclaration SM.Expression) where
  toHaskell :: NewtypeDeclaration SM.Expression -> SM.Output
  toHaskell newtypeDeclaration =
    mkHaskell' newtypeDeclaration "newtype " <>
    toHaskell (newtypeDeclaration ^. typeDefinition) <>
    mkHaskell' newtypeDeclaration " = " <>
    removeSurroundingParentheses (toHaskell (newtypeDeclaration ^. constructor))

instance ToHaskell (Lambda SM.Expression) where
  toHaskell :: Lambda SM.Expression -> SM.Output
  toHaskell lambda =
    SM.surround Parentheses $ mkHaskell' lambda "\\" <>
    SM.delimit Spaces (map toHaskell (lambda ^. arguments)) <>
    mkHaskell' lambda " -> " <>
    toHaskell (lambda ^. body)

instance ToHaskell (Pragma SM.Expression) where
  toHaskell :: Pragma SM.Expression -> SM.Output
  toHaskell pragma =
    mkHaskell' pragma $ Display.renderPragma (pragma ^. pragmaSpecification)

instance ToHaskell (LetBlock SM.Expression) where
  toHaskell :: LetBlock SM.Expression -> SM.Output
  toHaskell letBlock =
    SM.surround Parentheses $ mkHaskell' letBlock "let " <>
    SM.renderBlock (map bindingToHaskell (letBlock ^. bindings)) <>
    mkHaskell' letBlock " in " <>
    toHaskell (letBlock ^. body)
    where
      bindingToHaskell (pattern', value) =
        toHaskell pattern' <> mkHaskell' letBlock " = " <> toHaskell value

instance ToHaskell (MacroDefinition SM.Expression) where
  toHaskell :: MacroDefinition SM.Expression -> SM.Output
  toHaskell macroDefinition = toHaskell (macroDefinition ^. functionDefinition)

instance ToHaskell (MacroImport SM.Expression) where
  toHaskell :: MacroImport SM.Expression -> SM.Output
  toHaskell macroImport =
    toHaskell $
    RestrictedImport
      (macroImport ^. ann)
      (macroImport ^. moduleName)
      (ImportOnly (macroImport ^. ann) $
       map (ImportItem (macroImport ^. ann) . hygenisizeMacroName) $
       macroImport ^.
       imports)

instance ToHaskell (ImportSpecification SM.Expression) where
  toHaskell :: ImportSpecification SM.Expression -> SM.Output
  toHaskell importSpec@(ImportAll _) = mkHaskell importSpec ""
  toHaskell (ImportOnly _ importList) =
    SM.surround Parentheses $ SM.delimit Commas $ map toHaskell importList

instance ToHaskell (QualifiedImport SM.Expression) where
  toHaskell :: QualifiedImport SM.Expression -> SM.Output
  toHaskell qualifiedImport =
    mkHaskell'
      qualifiedImport
      ("import qualified " <> qualifiedImport ^. moduleName <> " as " <>
       qualifiedImport ^.
       alias) <>
    toHaskell (qualifiedImport ^. imports)

instance ToHaskell (Expression SM.Expression) where
  toHaskell :: Expression SM.Expression -> SM.Output
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

instance ToHaskell (RecordDefinition SM.Expression) where
  toHaskell :: RecordDefinition SM.Expression -> SM.Output
  toHaskell recordDefinition =
    SM.surround CurlyBraces $ SM.delimit Commas $
    map
      (\(var, val) ->
         mkHaskell' recordDefinition (var <> " = ") <> toHaskell val)
      (recordDefinition ^. bindings)

instance ToHaskell (RecordType SM.Expression) where
  toHaskell :: RecordType SM.Expression -> SM.Output
  toHaskell recordDefinition =
    SM.surround CurlyBraces $ SM.delimit Commas $
    map
      (\(field, ty) ->
         mkHaskell' recordDefinition (field <> " :: ") <> toHaskell ty)
      (recordDefinition ^. fields)

instance ToHaskell (Import SM.Expression) where
  toHaskell :: Import SM.Expression -> SM.Output
  toHaskell import'@(ImportItem _ x) =
    mkHaskell import' $
    if isOperator x
      then Display.surround Parentheses x
      else x
  toHaskell import'@(ImportType _ typeName imports') =
    mkHaskell import' $ typeName <>
    Display.surround Parentheses (Display.delimit Commas imports')

instance ToHaskell (RestrictedImport SM.Expression) where
  toHaskell :: RestrictedImport SM.Expression -> SM.Output
  toHaskell restrictedImport =
    mkHaskell' restrictedImport ("import " <> restrictedImport ^. moduleName) <>
    toHaskell (restrictedImport ^. imports)

instance ToHaskell (TopLevel SM.Expression) where
  toHaskell :: TopLevel SM.Expression -> SM.Output
  toHaskell topLevel =
    SM.delimit Newlines $ map toHaskell (topLevel ^. statements)

instance ToHaskell (TypeclassDefinition SM.Expression) where
  toHaskell :: TypeclassDefinition SM.Expression -> SM.Output
  toHaskell typeclassDefinition =
    mkHaskell' typeclassDefinition "class " <>
    SM.surround
      Parentheses
      (SM.delimit Commas (map toHaskell (typeclassDefinition ^. constraints))) <>
    mkHaskell' typeclassDefinition " => " <>
    toHaskell (typeclassDefinition ^. name) <>
    mkHaskell' typeclassDefinition " where " <>
    SM.renderBlock (map toHaskell $ typeclassDefinition ^. signatures)

instance ToHaskell (TypeclassInstance SM.Expression) where
  toHaskell :: TypeclassInstance SM.Expression -> SM.Output
  toHaskell typeclassInstance =
    mkHaskell' typeclassInstance "instance " <>
    toHaskell (typeclassInstance ^. instanceName) <>
    mkHaskell' typeclassInstance " where " <>
    SM.renderBlock (map toHaskell $ typeclassInstance ^. definitions)

instance ToHaskell (TypeSynonym SM.Expression) where
  toHaskell :: TypeSynonym SM.Expression -> SM.Output
  toHaskell typeSynonym =
    mkHaskell' typeSynonym "type " <> toHaskell (typeSynonym ^. alias) <>
    mkHaskell' typeSynonym " = " <>
    toHaskell (typeSynonym ^. definition)

instance Recursive (Expression ann) where
  bottomUpFmap ::
       (Expression ann -> Expression ann) -> Expression ann -> Expression ann
  bottomUpFmap f x =
    f $
    case x of
      ECaseBlock caseBlock ->
        ECaseBlock $ caseBlock & expr %~ bottomUpFmap f & matches %~
        map (bottomUpFmap f *** bottomUpFmap f)
      EEmptySExpression _ -> x
      EFunctionApplication functionApplication ->
        EFunctionApplication $ functionApplication & function %~ bottomUpFmap f &
        arguments %~
        map (bottomUpFmap f)
      EIdentifier _ _ -> x
      EIfBlock ifBlock ->
        EIfBlock $ ifBlock & cond %~ bottomUpFmap f & ifTrue %~ bottomUpFmap f &
        ifFalse %~
        bottomUpFmap f
      ELambda lambda ->
        ELambda $ lambda & arguments %~ map (bottomUpFmap f) & body %~
        bottomUpFmap f
      ELetBlock letBlock ->
        ELetBlock $ letBlock & bindings %~
        map (bottomUpFmap f *** bottomUpFmap f) &
        body %~
        bottomUpFmap f
      ELiteral literal ->
        case literal of
          LChar _ _ -> x
          LInt _ _ -> x
          LString _ _ -> x
      ERawExpression _ _ -> x
      ERecordDefinition _ -> x
      ERecordType _ -> x
  bottomUpTraverse ::
       (Monad m)
    => (Expression ann -> m (Expression ann))
    -> Expression ann
    -> m (Expression ann)
  bottomUpTraverse f x =
    f =<<
    case x of
      ECaseBlock caseBlock ->
        ECaseBlock <$>
        (CaseBlock (caseBlock ^. ann) <$> bottomUpTraverse f (caseBlock ^. expr) <*>
         traverse
           (\(a, b) -> (,) <$> bottomUpTraverse f a <*> bottomUpTraverse f b)
           (caseBlock ^. matches))
      EEmptySExpression _ -> pure x
      EFunctionApplication functionApplication ->
        EFunctionApplication <$>
        (FunctionApplication (functionApplication ^. ann) <$>
         bottomUpTraverse f (functionApplication ^. function) <*>
         traverse (bottomUpTraverse f) (functionApplication ^. arguments))
      EIdentifier _ _ -> pure x
      EIfBlock ifBlock ->
        EIfBlock <$>
        (IfBlock (ifBlock ^. ann) <$> bottomUpTraverse f (ifBlock ^. cond) <*>
         bottomUpTraverse f (ifBlock ^. ifTrue) <*>
         bottomUpTraverse f (ifBlock ^. ifFalse))
      ELambda lambda ->
        ELambda <$>
        (Lambda (lambda ^. ann) <$>
         traverse (bottomUpTraverse f) (lambda ^. arguments) <*>
         bottomUpTraverse f (lambda ^. body))
      ELetBlock letBlock ->
        ELetBlock <$>
        (LetBlock (letBlock ^. ann) <$>
         traverse
           (\(a, b) -> (a, ) <$> bottomUpTraverse f b)
           (letBlock ^. bindings) <*>
         bottomUpTraverse f (letBlock ^. body))
      ELiteral literal ->
        case literal of
          LChar _ _ -> pure x
          LInt _ _ -> pure x
          LString _ _ -> pure x
      ERawExpression _ _ -> pure x
      ERecordDefinition _ -> pure x
      ERecordType _ -> pure x
