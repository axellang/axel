module Axel.Denormalize where

import Axel.AST
  ( Expression(ECaseBlock, EEmptySExpression, EFunctionApplication,
           EIdentifier, ELambda, ELetBlock, ELiteral)
  , Import(ImportItem, ImportType)
  , ImportSpecification(ImportAll, ImportOnly)
  , Literal(LChar, LInt, LString)
  , Statement(SDataDeclaration, SFunctionDefinition, SMacroDefinition,
          SModuleDeclaration, SPragma, SQualifiedImport, SRestrictedImport,
          STopLevel, STypeSignature, STypeSynonym, STypeclassInstance,
          SUnrestrictedImport)
  , TopLevel(TopLevel)
  , TypeDefinition(ProperType, TypeConstructor)
  , alias
  , arguments
  , bindings
  , body
  , constructors
  , definition
  , definitions
  , expr
  , function
  , functionDefinition
  , imports
  , instanceName
  , matches
  , moduleName
  , name
  , pragmaSpecification
  , typeDefinition
  )
import qualified Axel.Parse as Parse
  ( Expression(LiteralChar, LiteralInt, LiteralString, SExpression,
           Symbol)
  )

import Control.Lens.Operators ((^.))

denormalizeExpression :: Expression -> Parse.Expression
denormalizeExpression (ECaseBlock caseBlock) =
  let denormalizedCases =
        map
          (\(pat, res) ->
             Parse.SExpression
               [denormalizeExpression pat, denormalizeExpression res])
          (caseBlock ^. matches)
   in Parse.SExpression $ Parse.Symbol "case" :
      denormalizeExpression (caseBlock ^. expr) :
      denormalizedCases
denormalizeExpression EEmptySExpression = Parse.SExpression []
denormalizeExpression (EFunctionApplication functionApplication) =
  Parse.SExpression $ denormalizeExpression (functionApplication ^. function) :
  map denormalizeExpression (functionApplication ^. arguments)
denormalizeExpression (EIdentifier x) = Parse.Symbol x
denormalizeExpression (ELambda lambda) =
  let denormalizedArguments =
        Parse.SExpression $ map denormalizeExpression (lambda ^. arguments)
   in Parse.SExpression
        [ Parse.Symbol "\\"
        , denormalizedArguments
        , denormalizeExpression (lambda ^. body)
        ]
denormalizeExpression (ELetBlock letBlock) =
  let denormalizedBindings =
        Parse.SExpression $
        map
          (\(var, val) ->
             Parse.SExpression
               [denormalizeExpression var, denormalizeExpression val])
          (letBlock ^. bindings)
   in Parse.SExpression
        [ Parse.Symbol "let"
        , denormalizedBindings
        , denormalizeExpression (letBlock ^. body)
        ]
denormalizeExpression (ELiteral x) =
  case x of
    LChar char -> Parse.LiteralChar char
    LInt int -> Parse.LiteralInt int
    LString string -> Parse.LiteralString string

denormalizeImportSpecification :: ImportSpecification -> Parse.Expression
denormalizeImportSpecification ImportAll = Parse.Symbol "all"
denormalizeImportSpecification (ImportOnly importList) =
  Parse.SExpression $ map denormalizeImport importList
  where
    denormalizeImport (ImportItem item) = Parse.Symbol item
    denormalizeImport (ImportType type' items) =
      Parse.SExpression (Parse.Symbol type' : map Parse.Symbol items)

denormalizeStatement :: Statement -> Parse.Expression
denormalizeStatement (SDataDeclaration dataDeclaration) =
  let denormalizedTypeDefinition =
        case dataDeclaration ^. typeDefinition of
          TypeConstructor typeConstructor ->
            denormalizeExpression $ EFunctionApplication typeConstructor
          ProperType properType -> Parse.Symbol properType
   in Parse.SExpression
        (Parse.Symbol "data" : denormalizedTypeDefinition :
         map
           (denormalizeExpression . EFunctionApplication)
           (dataDeclaration ^. constructors))
denormalizeStatement (SFunctionDefinition fnDef) =
  Parse.SExpression
    [ Parse.Symbol "="
    , Parse.Symbol (fnDef ^. name)
    , Parse.SExpression (map denormalizeExpression (fnDef ^. arguments))
    , denormalizeExpression (fnDef ^. body)
    ]
denormalizeStatement (SPragma pragma) =
  Parse.SExpression
    [Parse.Symbol "pragma", Parse.LiteralString (pragma ^. pragmaSpecification)]
denormalizeStatement (SMacroDefinition macroDef) =
  Parse.SExpression
    [ Parse.Symbol "macro"
    , Parse.Symbol (macroDef ^. functionDefinition . name)
    , Parse.SExpression
        (map denormalizeExpression (macroDef ^. functionDefinition . arguments))
    , denormalizeExpression (macroDef ^. functionDefinition . body)
    ]
denormalizeStatement (SModuleDeclaration identifier) =
  Parse.SExpression [Parse.Symbol "module", Parse.Symbol identifier]
denormalizeStatement (SQualifiedImport qualifiedImport) =
  Parse.SExpression
    [ Parse.Symbol "importq"
    , Parse.Symbol $ qualifiedImport ^. moduleName
    , Parse.Symbol $ qualifiedImport ^. alias
    , denormalizeImportSpecification (qualifiedImport ^. imports)
    ]
denormalizeStatement (SRestrictedImport restrictedImport) =
  Parse.SExpression
    [ Parse.Symbol "import"
    , Parse.Symbol $ restrictedImport ^. moduleName
    , denormalizeImportSpecification (restrictedImport ^. imports)
    ]
denormalizeStatement (STopLevel (TopLevel statements)) =
  Parse.SExpression $ Parse.Symbol "begin" : map denormalizeStatement statements
denormalizeStatement (STypeclassInstance typeclassInstance) =
  Parse.SExpression
    (Parse.Symbol "instance" :
     denormalizeExpression (typeclassInstance ^. instanceName) :
     map
       (denormalizeStatement . SFunctionDefinition)
       (typeclassInstance ^. definitions))
denormalizeStatement (STypeSignature typeSig) =
  Parse.SExpression
    [ Parse.Symbol "::"
    , Parse.Symbol (typeSig ^. name)
    , denormalizeExpression (typeSig ^. typeDefinition)
    ]
denormalizeStatement (STypeSynonym typeSynonym) =
  Parse.SExpression
    [ Parse.Symbol "type"
    , denormalizeExpression (typeSynonym ^. alias)
    , denormalizeExpression (typeSynonym ^. definition)
    ]
denormalizeStatement (SUnrestrictedImport identifier) =
  Parse.SExpression [Parse.Symbol "importUnrestricted", Parse.Symbol identifier]
