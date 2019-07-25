module Axel.Denormalize where

import Axel.AST
  ( Expression(ECaseBlock, EEmptySExpression, EFunctionApplication,
           EIdentifier, EIfBlock, ELambda, ELetBlock, ELiteral,
           ERawExpression, ERecordDefinition, ERecordType)
  , Import(ImportItem, ImportType)
  , ImportSpecification(ImportAll, ImportOnly)
  , Literal(LChar, LInt, LString)
  , Statement(SDataDeclaration, SFunctionDefinition, SMacroDefinition,
          SMacroImport, SModuleDeclaration, SNewtypeDeclaration, SPragma,
          SQualifiedImport, SRawStatement, SRestrictedImport, STopLevel,
          STypeSignature, STypeSynonym, STypeclassDefinition,
          STypeclassInstance, SUnrestrictedImport)
  , TopLevel(TopLevel)
  , TypeDefinition(ProperType, TypeConstructor)
  , alias
  , ann
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
  , getAnn
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
import qualified Axel.Parse as Parse
  ( Expression(LiteralChar, LiteralInt, LiteralString, SExpression,
           Symbol)
  )
import qualified Axel.Sourcemap as SM (Expression)

import Control.Lens.Operators ((^.))

denormalizeExpression :: Expression SM.Expression -> SM.Expression
denormalizeExpression (ECaseBlock caseBlock) =
  let ann' = getAnn (caseBlock ^. ann)
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
denormalizeExpression (EEmptySExpression ann') =
  Parse.SExpression (getAnn ann') []
denormalizeExpression (EFunctionApplication functionApplication) =
  Parse.SExpression (getAnn (functionApplication ^. ann)) $
  denormalizeExpression (functionApplication ^. function) :
  map denormalizeExpression (functionApplication ^. arguments)
denormalizeExpression (EIdentifier ann' x) = Parse.Symbol (getAnn ann') x
denormalizeExpression (EIfBlock ifBlock) =
  let ann' = getAnn (ifBlock ^. ann)
   in Parse.SExpression
        ann'
        [ Parse.Symbol ann' "if"
        , denormalizeExpression (ifBlock ^. cond)
        , denormalizeExpression (ifBlock ^. ifTrue)
        , denormalizeExpression (ifBlock ^. ifFalse)
        ]
denormalizeExpression (ELambda lambda) =
  let ann' = getAnn (lambda ^. ann)
      denormalizedArguments =
        Parse.SExpression ann' $ map denormalizeExpression (lambda ^. arguments)
   in Parse.SExpression
        ann'
        [ Parse.Symbol ann' "\\"
        , denormalizedArguments
        , denormalizeExpression (lambda ^. body)
        ]
denormalizeExpression (ELetBlock letBlock) =
  let ann' = getAnn (letBlock ^. ann)
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
    LChar ann' char -> Parse.LiteralChar (getAnn ann') char
    LInt ann' int -> Parse.LiteralInt (getAnn ann') int
    LString ann' string -> Parse.LiteralString (getAnn ann') string
denormalizeExpression (ERawExpression ann' rawSource) =
  Parse.SExpression
    (getAnn ann')
    [ Parse.Symbol (getAnn ann') "raw"
    , Parse.LiteralString (getAnn ann') rawSource
    ]
denormalizeExpression (ERecordDefinition recordDefinition) =
  let ann' = getAnn (recordDefinition ^. ann)
      denormalizedBindings =
        map
          (\(var, val) ->
             Parse.SExpression
               ann'
               [Parse.Symbol ann' var, denormalizeExpression val])
          (recordDefinition ^. bindings)
   in Parse.SExpression ann' (Parse.Symbol ann' "record" : denormalizedBindings)
denormalizeExpression (ERecordType recordType) =
  let ann' = getAnn (recordType ^. ann)
      denormalizedFields =
        map
          (\(field, ty) ->
             Parse.SExpression
               ann'
               [Parse.Symbol ann' field, denormalizeExpression ty])
          (recordType ^. fields)
   in Parse.SExpression
        ann'
        (Parse.Symbol ann' "recordType" : denormalizedFields)

denormalizeImportSpecification ::
     ImportSpecification SM.Expression -> SM.Expression
denormalizeImportSpecification (ImportAll ann') =
  Parse.Symbol (getAnn ann') "all"
denormalizeImportSpecification (ImportOnly ann' importList) =
  Parse.SExpression (getAnn ann') $ map denormalizeImport importList
  where
    denormalizeImport (ImportItem ann'' item) = Parse.Symbol (getAnn ann'') item
    denormalizeImport (ImportType ann'' type' items) =
      Parse.SExpression
        (getAnn ann'')
        (Parse.Symbol (getAnn ann'') type' :
         map (Parse.Symbol (getAnn ann'')) items)

denormalizeStatement :: Statement SM.Expression -> SM.Expression
denormalizeStatement (SDataDeclaration dataDeclaration) =
  let ann' = getAnn (dataDeclaration ^. ann)
      denormalizedTypeDefinition =
        case dataDeclaration ^. typeDefinition of
          TypeConstructor _ typeConstructor ->
            denormalizeExpression $ EFunctionApplication typeConstructor
          ProperType ann'' properType -> Parse.Symbol (getAnn ann'') properType
   in Parse.SExpression
        ann'
        (Parse.Symbol ann' "data" : denormalizedTypeDefinition :
         map
           (denormalizeExpression . EFunctionApplication)
           (dataDeclaration ^. constructors))
denormalizeStatement (SFunctionDefinition fnDef) =
  let ann' = getAnn (fnDef ^. ann)
   in Parse.SExpression ann' $ Parse.Symbol ann' "=" :
      Parse.Symbol ann' (fnDef ^. name) :
      Parse.SExpression ann' (map denormalizeExpression (fnDef ^. arguments)) :
      denormalizeExpression (fnDef ^. body) :
      map (denormalizeStatement . SFunctionDefinition) (fnDef ^. whereBindings)
denormalizeStatement (SMacroDefinition macroDef) =
  let ann' = getAnn (macroDef ^. ann)
   in Parse.SExpression ann' $ Parse.Symbol ann' "=macro" :
      Parse.Symbol ann' (macroDef ^. functionDefinition . name) :
      Parse.SExpression
        ann'
        (map denormalizeExpression (macroDef ^. functionDefinition . arguments)) :
      denormalizeExpression (macroDef ^. functionDefinition . body) :
      map
        (denormalizeStatement . SFunctionDefinition)
        (macroDef ^. functionDefinition . whereBindings)
denormalizeStatement (SMacroImport macroImport) =
  let ann' = getAnn (macroImport ^. ann)
   in Parse.SExpression
        ann'
        [ Parse.Symbol ann' "importm"
        , Parse.Symbol ann' $ macroImport ^. moduleName
        , Parse.SExpression ann' $
          map (Parse.Symbol ann') (macroImport ^. imports)
        ]
denormalizeStatement (SModuleDeclaration ann' identifier) =
  Parse.SExpression
    (getAnn ann')
    [Parse.Symbol (getAnn ann') "module", Parse.Symbol (getAnn ann') identifier]
denormalizeStatement (SNewtypeDeclaration newtypeDeclaration) =
  let ann' = getAnn (newtypeDeclaration ^. ann)
      denormalizedTypeDefinition =
        case newtypeDeclaration ^. typeDefinition of
          TypeConstructor _ typeConstructor ->
            denormalizeExpression $ EFunctionApplication typeConstructor
          ProperType ann'' properType -> Parse.Symbol (getAnn ann'') properType
   in Parse.SExpression
        ann'
        [ Parse.Symbol ann' "newtype"
        , denormalizedTypeDefinition
        , denormalizeExpression $
          EFunctionApplication (newtypeDeclaration ^. constructor)
        ]
denormalizeStatement (SPragma pragma) =
  let ann' = getAnn (pragma ^. ann)
   in Parse.SExpression
        ann'
        [ Parse.Symbol ann' "pragma"
        , Parse.LiteralString ann' (pragma ^. pragmaSpecification)
        ]
denormalizeStatement (SQualifiedImport qualifiedImport) =
  let ann' = getAnn (qualifiedImport ^. ann)
   in Parse.SExpression
        ann'
        [ Parse.Symbol ann' "importq"
        , Parse.Symbol ann' $ qualifiedImport ^. moduleName
        , Parse.Symbol ann' $ qualifiedImport ^. alias
        , denormalizeImportSpecification (qualifiedImport ^. imports)
        ]
denormalizeStatement (SRawStatement ann' rawSource) =
  Parse.SExpression
    (getAnn ann')
    [ Parse.Symbol (getAnn ann') "raw"
    , Parse.LiteralString (getAnn ann') rawSource
    ]
denormalizeStatement (SRestrictedImport restrictedImport) =
  let ann' = getAnn (restrictedImport ^. ann)
   in Parse.SExpression
        ann'
        [ Parse.Symbol ann' "import"
        , Parse.Symbol ann' $ restrictedImport ^. moduleName
        , denormalizeImportSpecification (restrictedImport ^. imports)
        ]
denormalizeStatement (STopLevel (TopLevel ann' statements)) =
  Parse.SExpression (getAnn ann') $ Parse.Symbol (getAnn ann') "begin" :
  map denormalizeStatement statements
denormalizeStatement (STypeclassDefinition typeclassDefinition) =
  let ann' = getAnn (typeclassDefinition ^. ann)
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
  let ann' = getAnn (typeclassInstance ^. ann)
   in Parse.SExpression
        ann'
        (Parse.Symbol ann' "instance" :
         denormalizeExpression (typeclassInstance ^. instanceName) :
         map
           (denormalizeStatement . SFunctionDefinition)
           (typeclassInstance ^. definitions))
denormalizeStatement (STypeSignature typeSig) =
  let ann' = getAnn (typeSig ^. ann)
   in Parse.SExpression
        ann'
        [ Parse.Symbol ann' "::"
        , Parse.Symbol ann' (typeSig ^. name)
        , denormalizeExpression (typeSig ^. typeDefinition)
        ]
denormalizeStatement (STypeSynonym typeSynonym) =
  let ann' = getAnn (typeSynonym ^. ann)
   in Parse.SExpression
        ann'
        [ Parse.Symbol ann' "type"
        , denormalizeExpression (typeSynonym ^. alias)
        , denormalizeExpression (typeSynonym ^. definition)
        ]
denormalizeStatement (SUnrestrictedImport ann' identifier) =
  Parse.SExpression
    (getAnn ann')
    [ Parse.Symbol (getAnn ann') "importUnrestricted"
    , Parse.Symbol (getAnn ann') identifier
    ]
