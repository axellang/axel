{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}

module Axel.Normalize where

import Axel.Prelude

import Axel.AST
  ( CaseBlock(CaseBlock)
  , DataDeclaration(DataDeclaration)
  , Expression(ECaseBlock, EEmptySExpression, EFunctionApplication,
           EIdentifier, EIfBlock, ELambda, ELetBlock, ELiteral,
           ERawExpression, ERecordDefinition, ERecordType)
  , FunctionApplication(FunctionApplication)
  , FunctionDefinition(FunctionDefinition)
  , Identifier
  , IfBlock(IfBlock)
  , Import(ImportItem, ImportType)
  , ImportSpecification(ImportAll, ImportOnly)
  , Lambda(Lambda)
  , LetBlock(LetBlock)
  , Literal(LChar, LFloat, LInt, LString)
  , MacroDefinition(MacroDefinition)
  , MacroImport(MacroImport)
  , NewtypeDeclaration(NewtypeDeclaration)
  , Pragma(Pragma)
  , QualifiedImport(QualifiedImport)
  , RecordDefinition(RecordDefinition)
  , RecordType(RecordType)
  , RestrictedImport(RestrictedImport)
  , SMStatement
  , Statement(SDataDeclaration, SFunctionDefinition, SMacroDefinition,
          SMacroImport, SModuleDeclaration, SNewtypeDeclaration, SPragma,
          SQualifiedImport, SRawStatement, SRestrictedImport, STopLevel,
          STypeSignature, STypeSynonym, STypeclassDefinition,
          STypeclassInstance)
  , TopLevel(TopLevel)
  , TypeDefinition(ProperType, TypeConstructor)
  , TypeSignature(TypeSignature)
  , TypeSynonym(TypeSynonym)
  , TypeclassDefinition(TypeclassDefinition)
  , TypeclassInstance(TypeclassInstance)
  )
import Axel.Eff.Error (Error(NormalizeError), renderError, unsafeRunError)
import qualified Axel.Parse.AST as Parse
  ( Expression(LiteralChar, LiteralFloat, LiteralInt, LiteralString,
           SExpression, Symbol)
  )
import qualified Axel.Sourcemap as SM (Expression)

import qualified Data.Text as T

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem
import qualified Polysemy.Reader as Sem

type ExprCtxt = [SM.Expression]

pushCtxt ::
     (Sem.Member (Sem.Reader [SM.Expression]) effs)
  => SM.Expression
  -> Sem.Sem effs a
  -> Sem.Sem effs a
pushCtxt newCtxt = Sem.local (newCtxt :)

withExprCtxt :: Sem.Sem (Sem.Reader [SM.Expression] ': effs) a -> Sem.Sem effs a
withExprCtxt = Sem.runReader []

throwNormalizeError ::
     (Sem.Members '[ Sem.Error Error, Sem.Reader FilePath, Sem.Reader ExprCtxt] effs)
  => Text
  -> Sem.Sem effs a
throwNormalizeError msg = do
  filePath <- Sem.ask @FilePath
  exprCtxt <- Sem.ask @ExprCtxt
  Sem.throw $ NormalizeError filePath msg exprCtxt

normalizeExpression ::
     (Sem.Members '[ Sem.Error Error, Sem.Reader FilePath, Sem.Reader ExprCtxt] effs)
  => SM.Expression
  -> Sem.Sem effs (Expression (Maybe SM.Expression))
normalizeExpression expr@(Parse.LiteralChar _ char) =
  pure $ ELiteral (LChar (Just expr) char)
normalizeExpression expr@(Parse.LiteralFloat _ float) =
  pure $ ELiteral (LFloat (Just expr) float)
normalizeExpression expr@(Parse.LiteralInt _ int) =
  pure $ ELiteral (LInt (Just expr) int)
normalizeExpression expr@(Parse.LiteralString _ string) =
  pure $ ELiteral (LString (Just expr) (T.pack string))
normalizeExpression expr@(Parse.SExpression _ items) =
  pushCtxt expr $
  case items of
    Parse.Symbol _ "case":var:cases ->
      let normalizedCases =
            traverse
              (\case
                 Parse.SExpression _ [pat, body] ->
                   (,) <$> normalizeExpression pat <*> normalizeExpression body
                 x ->
                   pushCtxt x $
                   throwNormalizeError
                     "A `case` clause must be an s-expression containing: 1) a pattern and 2) a body.")
              cases
       in ECaseBlock <$>
          (CaseBlock (Just expr) <$> normalizeExpression var <*> normalizedCases)
    Parse.Symbol _ "\\":args ->
      case args of
        [Parse.SExpression _ args', body] ->
          let normalizedArguments = traverse normalizeExpression args'
           in ELambda <$>
              (Lambda (Just expr) <$> normalizedArguments <*>
               normalizeExpression body)
        _ ->
          throwNormalizeError
            "`\\` takes exactly two arguments: 1) an argument list and 2) a body."
    Parse.Symbol _ "if":args ->
      case args of
        [cond, ifTrue, ifFalse] ->
          EIfBlock <$>
          (IfBlock (Just expr) <$> normalizeExpression cond <*>
           normalizeExpression ifTrue <*>
           normalizeExpression ifFalse)
        _ ->
          throwNormalizeError
            "`if` takes exactly three arguments: 1) the condition to test, 2) the value if it is true, and 3) the value if it is false."
    Parse.Symbol _ "let":args ->
      case args of
        [Parse.SExpression _ bindings, body] ->
          let normalizedBindings =
                traverse
                  (\case
                     Parse.SExpression _ [name, value] ->
                       (,) <$> normalizeExpression name <*>
                       normalizeExpression value
                     x ->
                       pushCtxt x $
                       throwNormalizeError
                         "Each `let` binding must be an s-expression containing: 1) a pattern and 2) a value!")
                  bindings
           in ELetBlock <$>
              (LetBlock (Just expr) <$> normalizedBindings <*>
               normalizeExpression body)
        _ ->
          throwNormalizeError
            "`let` takes exactly two arguments: 1) a bindings list and 2) a body."
    Parse.Symbol _ "raw":args ->
      case args of
        [rawSource] ->
          let normalizedRawSource =
                case rawSource of
                  Parse.LiteralString _ x -> pure x
                  x ->
                    pushCtxt x $
                    throwNormalizeError
                      "`raw` takes strings representing the code to inject directly."
           in ERawExpression (Just expr) . T.pack <$> normalizedRawSource
        _ ->
          throwNormalizeError
            "`raw` takes exactly one argument: a string literal."
    Parse.Symbol _ "record":bindings ->
      let normalizedBindings =
            traverse
              (\x ->
                 normalizeExpression x >>= \case
                   EFunctionApplication (FunctionApplication _ (EIdentifier _ field) [val]) ->
                     pure (field, val)
                   _ ->
                     pushCtxt x $
                     throwNormalizeError
                       "Each `record` field setter must be an s-expression containing: 1) a field name and 2) a value!")
              bindings
       in ERecordDefinition <$>
          (RecordDefinition (Just expr) <$> normalizedBindings)
    Parse.Symbol _ "recordType":fields ->
      let normalizedFields =
            traverse
              (\x ->
                 normalizeExpression x >>= \case
                   EFunctionApplication (FunctionApplication _ (EIdentifier _ field) [ty]) ->
                     pure (field, ty)
                   _ ->
                     pushCtxt x $
                     throwNormalizeError
                       "Each `recordType` field descriptor must be an s-expression containing: 1) a field name and 2) a type!")
              fields
       in ERecordType <$> (RecordType (Just expr) <$> normalizedFields)
    fn:args ->
      EFunctionApplication <$>
      (FunctionApplication (Just expr) <$> normalizeExpression fn <*>
       traverse normalizeExpression args)
    [] -> pure $ EEmptySExpression (Just expr)
normalizeExpression expr@(Parse.Symbol _ symbol) =
  pure $ EIdentifier (Just expr) (T.pack symbol)

normalizeFunctionDefinition ::
     (Sem.Members '[ Sem.Error Error, Sem.Reader FilePath, Sem.Reader ExprCtxt] effs)
  => SM.Expression
  -> Identifier
  -> [SM.Expression]
  -> SM.Expression
  -> [SM.Expression]
  -> Sem.Sem effs (FunctionDefinition (Maybe SM.Expression))
normalizeFunctionDefinition expr fnName arguments body whereDefs =
  FunctionDefinition (Just expr) fnName <$>
  traverse normalizeExpression arguments <*>
  normalizeExpression body <*>
  traverse
    (\x ->
       normalizeStatement x >>= \case
         stmt@(SFunctionDefinition _) -> pure stmt
         stmt@(STypeSignature _) -> pure stmt
         _ ->
           pushCtxt x $
           throwNormalizeError
             "Each `where` binding must be either a function definition or a type signature!")
    whereDefs

normalizeStatement ::
     (Sem.Members '[ Sem.Error Error, Sem.Reader FilePath, Sem.Reader ExprCtxt] effs)
  => SM.Expression
  -> Sem.Sem effs SMStatement
normalizeStatement expr@(Parse.SExpression _ items) =
  pushCtxt expr $
  case items of
    Parse.Symbol _ "::":args ->
      case args of
        [Parse.Symbol _ fnName, typeDef] ->
          STypeSignature <$>
          (TypeSignature (Just expr) (T.pack fnName) <$>
           normalizeExpression typeDef)
        _ ->
          throwNormalizeError
            "`::` takes two arguments: 1) a function name and 2) a type."
    Parse.Symbol _ "=":args ->
      case args of
        Parse.Symbol _ fnName:Parse.SExpression _ arguments:body:whereDefs ->
          SFunctionDefinition <$>
          normalizeFunctionDefinition
            expr
            (T.pack fnName)
            arguments
            body
            whereDefs
        _ ->
          throwNormalizeError
            "`=` takes 1) a function name, 2) an argument list, 3) an expression, and 4) a list of statements."
    Parse.Symbol _ "begin":stmts ->
      let normalizedStmts = traverse normalizeStatement stmts
       in STopLevel . TopLevel (Just expr) <$> normalizedStmts
    Parse.Symbol _ "class":classConstraints:className:sigs ->
      let normalizedConstraints =
            normalizeExpression classConstraints >>= \case
              EEmptySExpression _ -> pure []
              EFunctionApplication (FunctionApplication _ constraintsHead constraintsRest) ->
                pure $ constraintsHead : constraintsRest
              _ ->
                pushCtxt classConstraints $
                throwNormalizeError
                  "A `class` constraint list must only contain type constructors!"
          normalizedSigs =
            traverse
              (\x ->
                 normalizeStatement x >>= \case
                   STypeSignature tySig -> pure tySig
                   _ ->
                     pushCtxt x $
                     throwNormalizeError
                       "`class`'s body must contain type signatures!")
              sigs
       in STypeclassDefinition <$>
          (TypeclassDefinition (Just expr) <$> normalizeExpression className <*>
           normalizedConstraints <*>
           normalizedSigs)
    Parse.Symbol _ "data":typeDef:constructors ->
      normalizeExpression typeDef >>= \case
        EFunctionApplication typeConstructor ->
          SDataDeclaration <$>
          (DataDeclaration
             (Just expr)
             (TypeConstructor (Just expr) typeConstructor) <$>
           traverse normalizeExpression constructors)
        EIdentifier _ properType ->
          SDataDeclaration <$>
          (DataDeclaration (Just expr) (ProperType (Just expr) properType) <$>
           traverse normalizeExpression constructors)
        _ -> pushCtxt typeDef $ throwNormalizeError "Invalid type constructor!"
    Parse.Symbol _ "newtype":args ->
      case args of
        [typeDef, wrappedType] ->
          normalizeExpression typeDef >>= \case
            EFunctionApplication typeConstructor ->
              SNewtypeDeclaration <$>
              (NewtypeDeclaration
                 (Just expr)
                 (TypeConstructor (Just expr) typeConstructor) <$>
               normalizeExpression wrappedType)
            EIdentifier _ properType ->
              SNewtypeDeclaration <$>
              (NewtypeDeclaration
                 (Just expr)
                 (ProperType (Just expr) properType) <$>
               normalizeExpression wrappedType)
            _ ->
              pushCtxt typeDef $ throwNormalizeError "Invalid type constructor!"
        _ ->
          throwNormalizeError
            "`newtype` takes exactly two arguments: 1) a type constructor and 2) a type."
    Parse.Symbol _ "import":args ->
      case args of
        [Parse.Symbol _ moduleName, importSpec] ->
          SRestrictedImport <$>
          (RestrictedImport (Just expr) (T.pack moduleName) <$>
           normalizeImportSpec importSpec)
        _ ->
          throwNormalizeError
            "`import` takes exactly two arguments: 1) a module name and 2) an import specification."
    Parse.Symbol _ "importm":args ->
      case args of
        [Parse.Symbol _ moduleName, macroImportSpec] ->
          SMacroImport . MacroImport (Just expr) (T.pack moduleName) <$>
          normalizeMacroImportSpec macroImportSpec
        _ ->
          throwNormalizeError
            "`importm` takes exactly two arguments: 1) a module name and 2) a list of macro names."
    Parse.Symbol _ "importq":args ->
      case args of
        [Parse.Symbol _ moduleName, Parse.Symbol _ alias, importSpec] ->
          SQualifiedImport <$>
          (QualifiedImport (Just expr) (T.pack moduleName) (T.pack alias) <$>
           normalizeImportSpec importSpec)
        _ ->
          throwNormalizeError
            "`importq` takes exactly three arguments: 1) a module name, 2) a module name, and 3) an import specification."
    Parse.Symbol _ "instance":instanceConstraints:instanceName:defs ->
      let normalizedConstraints =
            normalizeExpression instanceConstraints >>= \case
              EEmptySExpression _ -> pure []
              EFunctionApplication (FunctionApplication _ constraintsHead constraintsRest) ->
                pure $ constraintsHead : constraintsRest
              _ ->
                pushCtxt instanceConstraints $
                throwNormalizeError
                  "`instance`'s constraint list must only contain type constructors!"
          normalizedDefs =
            traverse
              (\x ->
                 normalizeStatement x >>= \case
                   SFunctionDefinition fnDef -> pure fnDef
                   _ ->
                     pushCtxt x $
                     throwNormalizeError
                       "`instance`'s body must only contain function definitions!")
              defs
       in STypeclassInstance <$>
          (TypeclassInstance (Just expr) <$> normalizeExpression instanceName <*>
           normalizedConstraints <*>
           normalizedDefs)
    Parse.Symbol _ "pragma":args ->
      case args of
        [Parse.LiteralString _ pragma] ->
          pure $ SPragma (Pragma (Just expr) (T.pack pragma))
        _ ->
          throwNormalizeError
            "`pragma` takes exactly one argument: a string literal."
    Parse.Symbol _ "=macro":args ->
      case args of
        Parse.Symbol _ macroName:Parse.SExpression _ arguments:body:whereBindings ->
          SMacroDefinition . MacroDefinition (Just expr) <$>
          normalizeFunctionDefinition
            expr
            (T.pack macroName)
            arguments
            body
            whereBindings
        _ ->
          throwNormalizeError
            "`=macro` takes 1) an identifier, 2) an argument list, 3) an expression, and 4) a list of statements."
    Parse.Symbol _ "module":args ->
      case args of
        [Parse.Symbol _ moduleName] ->
          pure $ SModuleDeclaration (Just expr) (T.pack moduleName)
        _ ->
          throwNormalizeError
            "`module` takes exactly one argument: a module name."
    Parse.Symbol _ "raw":args ->
      case args of
        [rawSource] ->
          let normalizedRawSource =
                case rawSource of
                  Parse.LiteralString _ x -> pure $ T.pack x
                  x ->
                    pushCtxt x $
                    throwNormalizeError
                      "`raw` takes strings representing the code to inject directly."
           in SRawStatement (Just expr) <$> normalizedRawSource
        _ ->
          throwNormalizeError
            "`raw` takes exactly one argument: a string literal."
    Parse.Symbol _ "type":args ->
      case args of
        [alias, def] ->
          let normalizedAlias = normalizeExpression alias
              normalizedDef = normalizeExpression def
           in STypeSynonym <$>
              (TypeSynonym (Just expr) <$> normalizedAlias <*> normalizedDef)
        _ ->
          throwNormalizeError
            "`type` takes exactly two arguments: 1) a type constructor and 2) a type."
    _ -> throwNormalizeError "Invalid statement!"
  where
    normalizeMacroImportSpec importSpec =
      case importSpec of
        Parse.SExpression _ macroImportList ->
          traverse
            (\case
               Parse.Symbol _ import' -> pure $ T.pack import'
               x ->
                 pushCtxt x $
                 throwNormalizeError "A macro import must be an identifier!")
            macroImportList
        x ->
          pushCtxt x $
          throwNormalizeError
            "`importm`'s import specification must be a list of identifiers (`all` is not valid for macro imports)!"
    normalizeImportSpec importSpec =
      case importSpec of
        Parse.Symbol _ "all" -> pure $ ImportAll (Just expr)
        Parse.SExpression _ importList -> normalizeImportList importList
        x ->
          pushCtxt x $
          throwNormalizeError
            "An import specification must be either `all` or a list of import items!"
    normalizeImportList input =
      ImportOnly (Just expr) <$>
      traverse
        (\item ->
           pushCtxt item $
           case item of
             Parse.Symbol _ import' ->
               pure $ ImportItem (Just expr) (T.pack import')
             Parse.SExpression _ (Parse.Symbol _ type':imports) ->
               let normalizedImports =
                     traverse
                       (\case
                          Parse.Symbol _ import' -> pure $ T.pack import'
                          x ->
                            pushCtxt x $
                            throwNormalizeError
                              "An import subitem must be an identifier!")
                       imports
                in ImportType (Just expr) (T.pack type') <$> normalizedImports
             _ ->
               throwNormalizeError
                 "An import item must be either an identifier or an s-expression of identifiers!")
        input
normalizeStatement expr =
  pushCtxt expr $ throwNormalizeError "Invalid top-level form!"

unsafeNormalize ::
     (SM.Expression -> Sem.Sem '[ Sem.Reader ExprCtxt, Sem.Reader FilePath, Sem.Error Error] a)
  -> SM.Expression
  -> a
unsafeNormalize f =
  Sem.run .
  unsafeRunError renderError . Sem.runReader (FilePath "") . withExprCtxt . f
