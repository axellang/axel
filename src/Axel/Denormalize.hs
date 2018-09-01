module Axel.Denormalize where

import Axel.AST
  ( ArgumentList(ArgumentList)
  , Expression(ECaseBlock, EEmptySExpression, EFunctionApplication,
           EIdentifier, ELambda, ELetBlock, ELiteral)
  , Import(ImportItem, ImportType)
  , ImportSpecification(ImportAll, ImportOnly)
  , Literal(LChar, LInt, LString)
  , Statement(SDataDeclaration, SFunctionDefinition, SLanguagePragma,
          SMacroDefinition, SModuleDeclaration, SQualifiedImport,
          SRestrictedImport, STopLevel, STypeSynonym, STypeclassInstance,
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
  , imports
  , instanceName
  , language
  , matches
  , moduleName
  , name
  , typeDefinition
  , typeSignature
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
   in Parse.SExpression $
      Parse.Symbol "case" :
      denormalizeExpression (caseBlock ^. expr) : denormalizedCases
denormalizeExpression EEmptySExpression = Parse.SExpression []
denormalizeExpression (EFunctionApplication functionApplication) =
  Parse.SExpression $
  denormalizeExpression (functionApplication ^. function) :
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

denormalizeBinding :: (ArgumentList, Expression) -> Parse.Expression
denormalizeBinding (ArgumentList argumentList, expression) =
  Parse.SExpression
    [ Parse.SExpression $ map denormalizeExpression argumentList
    , denormalizeExpression expression
    ]

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
        [ Parse.Symbol "data"
        , denormalizedTypeDefinition
        , Parse.SExpression $
          map
            (denormalizeExpression . EFunctionApplication)
            (dataDeclaration ^. constructors)
        ]
denormalizeStatement (SFunctionDefinition functionDefinition) =
  Parse.SExpression $
  Parse.Symbol "=" :
  Parse.Symbol (functionDefinition ^. name) :
  denormalizeExpression
    (EFunctionApplication (functionDefinition ^. typeSignature)) :
  map denormalizeBinding (functionDefinition ^. definitions)
denormalizeStatement (SLanguagePragma languagePragma) =
  Parse.SExpression
    [Parse.Symbol "language", Parse.Symbol $ languagePragma ^. language]
denormalizeStatement (SMacroDefinition macroDefinition) =
  Parse.SExpression $
  Parse.Symbol "defmacro" :
  Parse.Symbol (macroDefinition ^. name) :
  map denormalizeBinding (macroDefinition ^. definitions)
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
    [ Parse.Symbol "instance"
    , denormalizeExpression (typeclassInstance ^. instanceName)
    , Parse.SExpression $
      map
        (denormalizeStatement . SFunctionDefinition)
        (typeclassInstance ^. definitions)
    ]
denormalizeStatement (STypeSynonym typeSynonym) =
  Parse.SExpression
    [ Parse.Symbol "type"
    , denormalizeExpression (typeSynonym ^. alias)
    , denormalizeExpression (typeSynonym ^. definition)
    ]
denormalizeStatement (SUnrestrictedImport identifier) =
  Parse.SExpression [Parse.Symbol "importUnrestricted", Parse.Symbol identifier]
