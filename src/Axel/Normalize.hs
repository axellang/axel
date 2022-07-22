{-# LANGUAGE ApplicativeDo #-}

module Axel.Normalize where

import Axel.Prelude

import Axel.AST
  ( CaseBlock(CaseBlock)
  , DataDeclaration(DataDeclaration)
  , Expression(ECaseBlock, EEmptySExpression, EFunctionApplication,
           EIdentifier, ELambda, ELetBlock, ELiteral, EPatternBinding,
           ERawExpression, ERecordDefinition, ERecordType)
  , FunctionApplication(FunctionApplication)
  , FunctionDefinition(FunctionDefinition)
  , Identifier
  , Import(ImportItem, ImportType)
  , ImportSpecification(ImportAll, ImportOnly)
  , Lambda(Lambda)
  , LetBlock(LetBlock)
  , Literal(LChar, LFloat, LInt, LString)
  , MacroDefinition(MacroDefinition)
  , MacroImport(MacroImport)
  , NewtypeDeclaration(NewtypeDeclaration)
  , PatternBinding(PatternBinding)
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
import Axel.Parse (unhygenisizeIdentifier)
import qualified Axel.Parse.AST as Parse
  ( Expression(LiteralChar, LiteralFloat, LiteralInt, LiteralString,
           SExpression, Symbol)
  )
import qualified Axel.Sourcemap as SM (Expression)

import qualified Data.Text as T

import Effectful ((:>), (:>>))
import qualified Effectful as Eff
import qualified Effectful.Error.Static as Eff
import qualified Effectful.Reader.Static as Eff

type ExprCtxt = [SM.Expression]

pushCtxt ::
     (Eff.Reader [SM.Expression] :> effs)
  => SM.Expression
  -> Eff.Eff effs a
  -> Eff.Eff effs a
pushCtxt newCtxt = Eff.local (newCtxt :)

withExprCtxt :: Eff.Eff (Eff.Reader [SM.Expression] ': effs) a -> Eff.Eff effs a
withExprCtxt = Eff.runReader []

throwNormalizeError ::
     ('[ Eff.Error Error, Eff.Reader FilePath, Eff.Reader ExprCtxt] :>> effs)
  => Text
  -> Eff.Eff effs a
throwNormalizeError msg = do
  filePath <- Eff.ask @FilePath
  exprCtxt <- Eff.ask @ExprCtxt
  Eff.throwError $ NormalizeError filePath msg exprCtxt

normalizeExpression ::
     ('[ Eff.Error Error, Eff.Reader FilePath, Eff.Reader ExprCtxt] :>> effs)
  => SM.Expression
  -> Eff.Eff effs (Expression (Maybe SM.Expression))
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
    (Parse.Symbol sourceMetadata special):args ->
      case unhygenisizeIdentifier special of
        "case" ->
          case args of
            var:cases ->
              let normalizedCases =
                    traverse
                      (\case
                         Parse.SExpression _ [pat, body] ->
                           (,) <$> normalizeExpression pat <*>
                           normalizeExpression body
                         x ->
                           pushCtxt x $
                           throwNormalizeError
                             "A `case` clause must be an s-expression containing: 1) a pattern and 2) a body.")
                      cases
               in ECaseBlock <$>
                  (CaseBlock (Just expr) <$> normalizeExpression var <*>
                   normalizedCases)
            _ ->
              throwNormalizeError
                "`case` takes an expression followed by `case` clauses."
        "@" ->
          case args of
            [name, pattern'] ->
              EPatternBinding <$>
              (PatternBinding (Just expr) <$> normalizeExpression name <*>
               normalizeExpression pattern')
            _ -> throwNormalizeError "`@` takes: 1) a name and 2) a pattern."
        "\\" ->
          case args of
            [Parse.SExpression _ (Parse.Symbol _ "list":args'), body] ->
              let normalizedArguments = traverse normalizeExpression args'
               in ELambda <$>
                  (Lambda (Just expr) <$> normalizedArguments <*>
                   normalizeExpression body)
            _ ->
              throwNormalizeError
                "`\\` takes exactly two arguments: 1) an argument list and 2) a body."
        "let" ->
          case args of
            [Parse.SExpression _ (Parse.Symbol _ "list":bindings), body] ->
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
        "raw" ->
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
        "record" ->
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
                  args
           in ERecordDefinition <$>
              (RecordDefinition (Just expr) <$> normalizedBindings)
        "recordType" ->
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
                  args
           in ERecordType <$> (RecordType (Just expr) <$> normalizedFields)
        _ ->
          EFunctionApplication <$>
          (FunctionApplication (Just expr) <$>
           normalizeExpression (Parse.Symbol sourceMetadata special) <*>
           traverse normalizeExpression args)
    fn:args ->
      EFunctionApplication <$>
      (FunctionApplication (Just expr) <$> normalizeExpression fn <*>
       traverse normalizeExpression args)
    [] -> pure $ EEmptySExpression (Just expr)
normalizeExpression expr@(Parse.Symbol _ symbol) =
  pure $ EIdentifier (Just expr) (T.pack symbol)

normalizeFunctionDefinition ::
     ('[ Eff.Error Error, Eff.Reader FilePath, Eff.Reader ExprCtxt] :>> effs)
  => SM.Expression
  -> Identifier
  -> [SM.Expression]
  -> SM.Expression
  -> [SM.Expression]
  -> Eff.Eff effs (FunctionDefinition (Maybe SM.Expression))
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
     ('[ Eff.Error Error, Eff.Reader FilePath, Eff.Reader ExprCtxt] :>> effs)
  => SM.Expression
  -> Eff.Eff effs SMStatement
normalizeStatement expr@(Parse.SExpression _ items) =
  pushCtxt expr $
  case items of
    Parse.Symbol _ special:args ->
      case unhygenisizeIdentifier special of
        "::" ->
          case args of
            [Parse.Symbol _ fnName, Parse.SExpression _ (Parse.Symbol _ "list":constraints), typeDef] ->
              STypeSignature <$>
              (TypeSignature (Just expr) (T.pack fnName) <$>
               traverse normalizeExpression constraints <*>
               normalizeExpression typeDef)
            _ ->
              throwNormalizeError
                "`::` takes three arguments: 1) a function name, 2) a list of constraints, and 3) a type."
        "=" ->
          case args of
            fnSpecifier:body:whereDefs -> do
              (fnName, arguments) <-
                case fnSpecifier of
                  Parse.Symbol _ fnName -> pure (fnName, [])
                  Parse.SExpression _ (Parse.Symbol _ fnName:args') ->
                    pure (fnName, args')
                  _ ->
                    throwNormalizeError
                      "A function head specifier takes 1) a function name, followed by argument patterns."
              SFunctionDefinition <$>
                normalizeFunctionDefinition
                  expr
                  (T.pack fnName)
                  arguments
                  body
                  whereDefs
            _ ->
              throwNormalizeError
                "`=` takes 1) a function head specifier 2) and an expression, followed by `where` bindings."
        "begin" ->
          let normalizedStmts = traverse normalizeStatement args
           in STopLevel . TopLevel (Just expr) <$> normalizedStmts
        "class" ->
          case args of
            Parse.SExpression _ (Parse.Symbol _ "list":classConstraints):className:sigs ->
              let normalizedSigs =
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
                  (TypeclassDefinition (Just expr) <$>
                   normalizeExpression className <*>
                   traverse normalizeExpression classConstraints <*>
                   normalizedSigs)
            _ ->
              throwNormalizeError
                "`class` takes a constraint list, a type constructor, and type signatures!"
        "data" ->
          case args of
            typeDef:rest ->
              let (constructors, derivedConstraints) =
                    case last rest of
                      Parse.SExpression _ (Parse.Symbol _ "list":xs) ->
                        (init rest, xs)
                      _ -> (rest, [])
               in SDataDeclaration <$>
                  (DataDeclaration (Just expr) <$>
                   (normalizeExpression typeDef >>= \case
                      EFunctionApplication typeConstructor ->
                        pure $ TypeConstructor (Just expr) typeConstructor
                      EIdentifier _ properType ->
                        pure $ ProperType (Just expr) properType
                      _ ->
                        pushCtxt typeDef $
                        throwNormalizeError "Invalid type constructor!") <*>
                   traverse normalizeExpression constructors <*>
                   traverse normalizeExpression derivedConstraints)
            _ ->
              throwNormalizeError
                "`data` takes a type constructor followed by type constructors, optionally followed by a list of constraints to derive!"
        "newtype" ->
          case args of
            typeDef:wrappedType:rest ->
              let derivedConstraints =
                    case rest of
                      [] -> pure []
                      [Parse.SExpression _ (Parse.Symbol _ "list":xs)] ->
                        traverse normalizeExpression xs
                      _ ->
                        throwNormalizeError
                          "Invalid list of constraints to derive!"
               in SNewtypeDeclaration <$>
                  (NewtypeDeclaration (Just expr) <$>
                   (normalizeExpression typeDef >>= \case
                      EFunctionApplication typeConstructor ->
                        pure $ TypeConstructor (Just expr) typeConstructor
                      EIdentifier _ properType ->
                        pure $ ProperType (Just expr) properType
                      _ ->
                        pushCtxt typeDef $
                        throwNormalizeError "Invalid type constructor!") <*>
                   normalizeExpression wrappedType <*>
                   derivedConstraints)
            _ ->
              throwNormalizeError
                "`newtype` takes two or three arguments: 1) a type constructor, 2) a type, and optionally 3) a list of constraints to derive!"
        "import" ->
          case args of
            [Parse.Symbol _ moduleName, importSpec] ->
              SRestrictedImport <$>
              (RestrictedImport (Just expr) (T.pack moduleName) <$>
               normalizeImportSpec importSpec)
            _ ->
              throwNormalizeError
                "`import` takes exactly two arguments: 1) a module name and 2) an import specification."
        "importm" ->
          case args of
            [Parse.Symbol _ moduleName, macroImportSpec] ->
              SMacroImport . MacroImport (Just expr) (T.pack moduleName) <$>
              normalizeMacroImportSpec macroImportSpec
            _ ->
              throwNormalizeError
                "`importm` takes exactly two arguments: 1) a module name and 2) a list of macro names."
        "importq" ->
          case args of
            [Parse.Symbol _ moduleName, Parse.Symbol _ alias, importSpec] ->
              SQualifiedImport <$>
              (QualifiedImport (Just expr) (T.pack moduleName) (T.pack alias) <$>
               normalizeImportSpec importSpec)
            _ ->
              throwNormalizeError
                "`importq` takes exactly three arguments: 1) a module name, 2) a module name, and 3) an import specification."
        "instance" ->
          case args of
            Parse.SExpression _ (Parse.Symbol _ "list":instanceConstraints):instanceName:defs ->
              let normalizedDefs =
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
                  (TypeclassInstance (Just expr) <$>
                   normalizeExpression instanceName <*>
                   traverse normalizeExpression instanceConstraints <*>
                   normalizedDefs)
            _ ->
              throwNormalizeError
                "`instance` takes a constraint list, an expression, and a list of function definitions!"
        "pragma" ->
          case args of
            [Parse.LiteralString _ pragma] ->
              pure $ SPragma (Pragma (Just expr) (T.pack pragma))
            _ ->
              throwNormalizeError
                "`pragma` takes exactly one argument: a string literal."
        "=macro" ->
          case args of
            Parse.Symbol _ macroName:argument:body:whereBindings ->
              SMacroDefinition . MacroDefinition (Just expr) <$>
              normalizeFunctionDefinition
                expr
                (T.pack macroName)
                [argument]
                body
                whereBindings
            _ ->
              throwNormalizeError
                "`=macro` takes 1) an identifier, 2) an argument list, 3) an expression, and 4) a list of statements."
        "module" ->
          case args of
            [Parse.Symbol _ moduleName] ->
              pure $ SModuleDeclaration (Just expr) (T.pack moduleName)
            _ ->
              throwNormalizeError
                "`module` takes exactly one argument: a module name."
        "raw" ->
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
        "type" ->
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
    _ -> throwNormalizeError "Invalid statement!"
  where
    normalizeMacroImportSpec importSpec =
      case importSpec of
        Parse.SExpression _ (Parse.Symbol _ "list":macroImportList) ->
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
        Parse.SExpression _ (Parse.Symbol _ "list":importList) ->
          normalizeImportList importList
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
     (SM.Expression -> Eff.Eff '[ Eff.Reader ExprCtxt, Eff.Reader FilePath, Eff.Error Error] a)
  -> SM.Expression
  -> a
unsafeNormalize f =
  Eff.runPureEff .
  unsafeRunError renderError . Eff.runReader (FilePath "") . withExprCtxt . f
