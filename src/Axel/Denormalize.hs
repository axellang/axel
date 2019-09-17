module Axel.Denormalize where

import Axel.Prelude

import Axel.AST
  ( Expression(ECaseBlock, EEmptySExpression, EFunctionApplication,
           EIdentifier, EIfBlock, ELambda, ELetBlock, ELiteral,
           ERawExpression, ERecordDefinition, ERecordType)
  , Import(ImportItem, ImportType)
  , ImportSpecification(ImportAll, ImportOnly)
  , Literal(LChar, LInt, LString)
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
  , cond
  , constraints
  , constructor
  , constructors
  , definition
  , definitions
  , expr
  , fields
  , function
  , functionDefinition
  , getAnn'
  , ifFalse
  , ifTrue
  , imports
  , instanceName
  , matches
  , moduleName
  , name
  , pragmaSpecification
  , signatures
  , typeDefinition
  , whereBindings
  )
import qualified Axel.Parse.AST as Parse
  ( Expression(LiteralChar, LiteralInt, LiteralString, SExpression,
           Symbol)
  )
import qualified Axel.Sourcemap as SM (Expression)

import Control.Lens.Operators ((^.))

import qualified Data.Text as T

-- | Metadata is only approximately restored. Thus, `normalizeExpression` and
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
denormalizeExpression (EIfBlock ifBlock) =
  let ann' = getAnn' ifBlock
   in Parse.SExpression
        ann'
        [ Parse.Symbol ann' "if"
        , denormalizeExpression (ifBlock ^. cond)
        , denormalizeExpression (ifBlock ^. ifTrue)
        , denormalizeExpression (ifBlock ^. ifFalse)
        ]
denormalizeExpression (ELambda lambda) =
  let ann' = getAnn' lambda
      denormalizedArguments =
        Parse.SExpression ann' $ map denormalizeExpression (lambda ^. arguments)
   in Parse.SExpression
        ann'
        [ Parse.Symbol ann' "\\"
        , denormalizedArguments
        , denormalizeExpression (lambda ^. body)
        ]
denormalizeExpression (ELetBlock letBlock) =
  let ann' = getAnn' letBlock
      denormalizedBindings =
        Parse.SExpression ann' $
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
    LInt _ int -> Parse.LiteralInt (getAnn' x) int
    LString _ string -> Parse.LiteralString (getAnn' x) (T.unpack string)
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
  Parse.SExpression (getAnn' importSpec) $ map denormalizeImport importList
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
   in Parse.SExpression
        ann'
        (Parse.Symbol ann' "data" : denormalizedTypeDefinition :
         map
           (denormalizeExpression . EFunctionApplication)
           (dataDeclaration ^. constructors))
denormalizeStatement (SFunctionDefinition fnDef) =
  let ann' = getAnn' fnDef
   in Parse.SExpression ann' $ Parse.Symbol ann' "=" :
      Parse.Symbol ann' (T.unpack $ fnDef ^. name) :
      Parse.SExpression ann' (map denormalizeExpression (fnDef ^. arguments)) :
      denormalizeExpression (fnDef ^. body) :
      map denormalizeStatement (fnDef ^. whereBindings)
denormalizeStatement (SMacroDefinition macroDef) =
  let ann' = getAnn' macroDef
   in Parse.SExpression ann' $ Parse.Symbol ann' "=macro" :
      Parse.Symbol ann' (T.unpack $ macroDef ^. functionDefinition . name) :
      Parse.SExpression
        ann'
        (map denormalizeExpression (macroDef ^. functionDefinition . arguments)) :
      denormalizeExpression (macroDef ^. functionDefinition . body) :
      map denormalizeStatement (macroDef ^. functionDefinition . whereBindings)
denormalizeStatement (SMacroImport macroImport) =
  let ann' = getAnn' macroImport
   in Parse.SExpression
        ann'
        [ Parse.Symbol ann' "importm"
        , Parse.Symbol ann' (T.unpack $ macroImport ^. moduleName)
        , Parse.SExpression ann' $
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
   in Parse.SExpression
        ann'
        [ Parse.Symbol ann' "newtype"
        , denormalizedTypeDefinition
        , denormalizeExpression $
          EFunctionApplication (newtypeDeclaration ^. constructor)
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
