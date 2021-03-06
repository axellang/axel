module Axel.Denormalize where

import Axel.Prelude

import Axel.AST
  ( Expression(ECaseBlock, EEmptySExpression, EFunctionApplication,
           EIdentifier, ELambda, ELetBlock, ELiteral, EPatternBinding,
           ERawExpression, ERecordDefinition, ERecordType)
  , Import(ImportItem, ImportType)
  , ImportSpecification(ImportAll, ImportOnly)
  , Literal(LChar, LFloat, LInt, LString)
  , SMExpression
  , SMStatement
  , Statement(SDataDeclaration, SFunctionDefinition, SMacroDefinition,
          SMacroImport, SModuleDeclaration, SNewtypeDeclaration, SPragma,
          SQualifiedImport, SRawStatement, SRestrictedImport, STopLevel,
          STypeSignature, STypeSynonym, STypeclassDefinition,
          STypeclassInstance)
  , TopLevel(TopLevel)
  , TypeDefinition(ProperType, TypeConstructor)
  , alias
  , arguments
  , bindings
  , body
  , constraints
  , constructors
  , definition
  , definitions
  , derivedConstraints
  , expr
  , fields
  , function
  , functionDefinition
  , getAnn'
  , imports
  , instanceName
  , matches
  , moduleName
  , name
  , pattern'
  , pragmaSpecification
  , signatures
  , typeDefinition
  , whereBindings
  , wrappedType
  )
import qualified Axel.Parse.AST as Parse
import qualified Axel.Sourcemap as SM
import Axel.Utils.List (unsafeHead)

import Control.Lens.Operators ((^.), (|>))

import qualified Data.Text as T

-- | Metadata is only approximately restored. Thus, `Axel.Normalize.normalizeExpression` and
-- | `denormalizeExpression` are only inverses if metadata information is
-- | ignored, although they should be pretty close either way.
denormalizeExpression :: SMExpression -> SM.Expression
denormalizeExpression (ECaseBlock caseBlock) =
  let ann' = getAnn' caseBlock
      denormalizedCases =
        map
          (\(pat, res) ->
             Parse.SExpression
               ann'
               [denormalizeExpression pat, denormalizeExpression res])
          (caseBlock ^. matches)
   in Parse.SExpression ann' $ Parse.Symbol ann' "case" :
      denormalizeExpression (caseBlock ^. expr) :
      denormalizedCases
denormalizeExpression expr'@(EEmptySExpression _) =
  Parse.SExpression (getAnn' expr') []
denormalizeExpression (EFunctionApplication functionApplication) =
  Parse.SExpression (getAnn' functionApplication) $
  denormalizeExpression (functionApplication ^. function) :
  map denormalizeExpression (functionApplication ^. arguments)
denormalizeExpression expr'@(EIdentifier _ x) =
  Parse.Symbol (getAnn' expr') (T.unpack x)
denormalizeExpression (ELambda lambda) =
  let ann' = getAnn' lambda
      denormalizedArguments =
        Parse.SExpression ann' $ Parse.Symbol ann' "list" :
        map denormalizeExpression (lambda ^. arguments)
   in Parse.SExpression
        ann'
        [ Parse.Symbol ann' "\\"
        , denormalizedArguments
        , denormalizeExpression (lambda ^. body)
        ]
denormalizeExpression (ELetBlock letBlock) =
  let ann' = getAnn' letBlock
      denormalizedBindings =
        Parse.SExpression ann' $ Parse.Symbol ann' "list" :
        map
          (\(var, val) ->
             Parse.SExpression
               ann'
               [denormalizeExpression var, denormalizeExpression val])
          (letBlock ^. bindings)
   in Parse.SExpression
        ann'
        [ Parse.Symbol ann' "let"
        , denormalizedBindings
        , denormalizeExpression (letBlock ^. body)
        ]
denormalizeExpression (ELiteral x) =
  case x of
    LChar _ char -> Parse.LiteralChar (getAnn' x) char
    LFloat _ float -> Parse.LiteralFloat (getAnn' x) float
    LInt _ int -> Parse.LiteralInt (getAnn' x) int
    LString _ string -> Parse.LiteralString (getAnn' x) (T.unpack string)
denormalizeExpression (EPatternBinding patternBinding) =
  let ann' = getAnn' patternBinding
   in Parse.SExpression
        ann'
        [ Parse.Symbol ann' "@"
        , denormalizeExpression (patternBinding ^. name)
        , denormalizeExpression (patternBinding ^. pattern')
        ]
denormalizeExpression expr'@(ERawExpression _ rawSource) =
  let ann' = getAnn' expr'
   in Parse.SExpression
        ann'
        [Parse.Symbol ann' "raw", Parse.LiteralString ann' (T.unpack rawSource)]
denormalizeExpression (ERecordDefinition recordDefinition) =
  let ann' = getAnn' recordDefinition
      denormalizedBindings =
        map
          (\(var, val) ->
             Parse.SExpression
               ann'
               [Parse.Symbol ann' (T.unpack var), denormalizeExpression val])
          (recordDefinition ^. bindings)
   in Parse.SExpression ann' (Parse.Symbol ann' "record" : denormalizedBindings)
denormalizeExpression (ERecordType recordType) =
  let ann' = getAnn' recordType
      denormalizedFields =
        map
          (\(field, ty) ->
             Parse.SExpression
               ann'
               [Parse.Symbol ann' (T.unpack field), denormalizeExpression ty])
          (recordType ^. fields)
   in Parse.SExpression
        ann'
        (Parse.Symbol ann' "recordType" : denormalizedFields)

denormalizeImportSpecification ::
     ImportSpecification (Maybe SM.Expression) -> SM.Expression
denormalizeImportSpecification importSpec@(ImportAll _) =
  Parse.Symbol (getAnn' importSpec) "all"
denormalizeImportSpecification importSpec@(ImportOnly _ importList) =
  let ann' = getAnn' importSpec
   in Parse.SExpression ann' $ Parse.Symbol ann' "list" :
      map denormalizeImport importList
  where
    denormalizeImport importList'@(ImportItem _ item) =
      Parse.Symbol (getAnn' importList') (T.unpack item)
    denormalizeImport importList'@(ImportType _ type' items) =
      let ann' = getAnn' importList'
       in Parse.SExpression
            ann'
            (Parse.Symbol ann' (T.unpack type') :
             map (Parse.Symbol ann' . T.unpack) items)

denormalizeStatement :: SMStatement -> SM.Expression
denormalizeStatement (SDataDeclaration dataDeclaration) =
  let ann' = getAnn' dataDeclaration
      denormalizedTypeDefinition =
        case dataDeclaration ^. typeDefinition of
          TypeConstructor _ typeConstructor ->
            denormalizeExpression $ EFunctionApplication typeConstructor
          ProperType _ properType ->
            Parse.Symbol
              (getAnn' $ dataDeclaration ^. typeDefinition)
              (T.unpack properType)
      denormalizedConstructors =
        map denormalizeExpression (dataDeclaration ^. constructors)
      denormalizedDerivedConstraintList =
        Parse.SExpression ann' $ Parse.Symbol ann' "list" :
        map denormalizeExpression (dataDeclaration ^. derivedConstraints)
   in Parse.SExpression ann' $ Parse.Symbol ann' "data" :
      denormalizedTypeDefinition :
      (denormalizedConstructors |> denormalizedDerivedConstraintList)
denormalizeStatement (SFunctionDefinition fnDef) =
  let ann' = getAnn' fnDef
   in Parse.SExpression ann' $ Parse.Symbol ann' "=" :
      Parse.SExpression
        ann'
        (Parse.Symbol ann' (T.unpack $ fnDef ^. name) :
         map denormalizeExpression (fnDef ^. arguments)) :
      denormalizeExpression (fnDef ^. body) :
      map denormalizeStatement (fnDef ^. whereBindings)
denormalizeStatement (SMacroDefinition macroDef) =
  let ann' = getAnn' macroDef
   in Parse.SExpression ann' $ Parse.Symbol ann' "=macro" :
      Parse.Symbol ann' (T.unpack $ macroDef ^. functionDefinition . name) :
      denormalizeExpression
        (unsafeHead $ macroDef ^. functionDefinition . arguments) :
      denormalizeExpression (macroDef ^. functionDefinition . body) :
      map denormalizeStatement (macroDef ^. functionDefinition . whereBindings)
denormalizeStatement (SMacroImport macroImport) =
  let ann' = getAnn' macroImport
   in Parse.SExpression
        ann'
        [ Parse.Symbol ann' "importm"
        , Parse.Symbol ann' (T.unpack $ macroImport ^. moduleName)
        , Parse.SExpression ann' $ Parse.Symbol ann' "list" :
          map (Parse.Symbol ann' . T.unpack) (macroImport ^. imports)
        ]
denormalizeStatement stmt@(SModuleDeclaration _ identifier) =
  let ann' = getAnn' stmt
   in Parse.SExpression
        ann'
        [Parse.Symbol ann' "module", Parse.Symbol ann' (T.unpack identifier)]
denormalizeStatement (SNewtypeDeclaration newtypeDeclaration) =
  let ann' = getAnn' newtypeDeclaration
      denormalizedTypeDefinition =
        case newtypeDeclaration ^. typeDefinition of
          TypeConstructor _ typeConstructor ->
            denormalizeExpression $ EFunctionApplication typeConstructor
          ProperType _ properType ->
            Parse.Symbol
              (getAnn' $ newtypeDeclaration ^. typeDefinition)
              (T.unpack properType)
      denormalizedDerivedConstraintList =
        Parse.SExpression ann' $ Parse.Symbol ann' "list" :
        map denormalizeExpression (newtypeDeclaration ^. derivedConstraints)
   in Parse.SExpression
        ann'
        [ Parse.Symbol ann' "newtype"
        , denormalizedTypeDefinition
        , denormalizeExpression $ newtypeDeclaration ^. wrappedType
        , denormalizedDerivedConstraintList
        ]
denormalizeStatement (SPragma pragma) =
  let ann' = getAnn' pragma
   in Parse.SExpression
        ann'
        [ Parse.Symbol ann' "pragma"
        , Parse.LiteralString ann' (T.unpack $ pragma ^. pragmaSpecification)
        ]
denormalizeStatement (SQualifiedImport qualifiedImport) =
  let ann' = getAnn' qualifiedImport
   in Parse.SExpression
        ann'
        [ Parse.Symbol ann' "importq"
        , Parse.Symbol ann' (T.unpack $ qualifiedImport ^. moduleName)
        , Parse.Symbol ann' (T.unpack $ qualifiedImport ^. alias)
        , denormalizeImportSpecification (qualifiedImport ^. imports)
        ]
denormalizeStatement expr'@(SRawStatement _ rawSource) =
  let ann' = getAnn' expr'
   in Parse.SExpression
        ann'
        [Parse.Symbol ann' "raw", Parse.LiteralString ann' (T.unpack rawSource)]
denormalizeStatement (SRestrictedImport restrictedImport) =
  let ann' = getAnn' restrictedImport
   in Parse.SExpression
        ann'
        [ Parse.Symbol ann' "import"
        , Parse.Symbol ann' (T.unpack $ restrictedImport ^. moduleName)
        , denormalizeImportSpecification (restrictedImport ^. imports)
        ]
denormalizeStatement stmt@(STopLevel (TopLevel _ statements)) =
  let ann' = getAnn' stmt
   in Parse.SExpression ann' $ Parse.Symbol ann' "begin" :
      map denormalizeStatement statements
denormalizeStatement (STypeclassDefinition typeclassDefinition) =
  let ann' = getAnn' typeclassDefinition
   in Parse.SExpression
        ann'
        (Parse.Symbol ann' "class" :
         Parse.SExpression
           ann'
           (Parse.Symbol ann' "list" :
            map denormalizeExpression (typeclassDefinition ^. constraints)) :
         denormalizeExpression (typeclassDefinition ^. name) :
         map
           (denormalizeStatement . STypeSignature)
           (typeclassDefinition ^. signatures))
denormalizeStatement (STypeclassInstance typeclassInstance) =
  let ann' = getAnn' typeclassInstance
   in Parse.SExpression
        ann'
        (Parse.Symbol ann' "instance" :
         Parse.SExpression
           ann'
           (Parse.Symbol ann' "list" :
            map denormalizeExpression (typeclassInstance ^. constraints)) :
         denormalizeExpression (typeclassInstance ^. instanceName) :
         map
           (denormalizeStatement . SFunctionDefinition)
           (typeclassInstance ^. definitions))
denormalizeStatement (STypeSignature typeSig) =
  let ann' = getAnn' typeSig
   in Parse.SExpression
        ann'
        [ Parse.Symbol ann' "::"
        , Parse.Symbol ann' (T.unpack $ typeSig ^. name)
        , Parse.SExpression
            ann'
            (Parse.Symbol ann' "list" :
             map denormalizeExpression (typeSig ^. constraints))
        , denormalizeExpression (typeSig ^. typeDefinition)
        ]
denormalizeStatement (STypeSynonym typeSynonym) =
  let ann' = getAnn' typeSynonym
   in Parse.SExpression
        ann'
        [ Parse.Symbol ann' "type"
        , denormalizeExpression (typeSynonym ^. alias)
        , denormalizeExpression (typeSynonym ^. definition)
        ]
