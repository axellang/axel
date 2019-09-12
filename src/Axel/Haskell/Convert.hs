-- TODO Integrate with the effects system used everywhere else (convert `error` to `throwError`, etc.)
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC
  -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

module Axel.Haskell.Convert where

import Prelude hiding (putStrLn)

import qualified Axel.AST as AST
import Axel.Denormalize (denormalizeExpression, denormalizeStatement)
import Axel.Eff.Console (putStrLn)
import qualified Axel.Eff.Console as Effs (Console)
import Axel.Eff.Error (Error(ConvertError), fatal)
import qualified Axel.Eff.FileSystem as Effs (FileSystem)
import qualified Axel.Eff.FileSystem as FS (writeFile)
import qualified Axel.Parse.AST as Parse
import Axel.Parse.AST (toAxel)
import qualified Axel.Sourcemap as SM (Expression)
import Axel.Utils.List (removeOut, stablyGroupAllWith, unsafeHead)
import Axel.Utils.String (replace)
import Axel.Utils.Tuple (flattenAnnotations, unannotated)

import Control.Category ((>>>))
import Control.Lens.Extras (is)
import Control.Lens.Operators ((%~), (^.))

import Data.Data.Lens (biplate, uniplate)

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem

import qualified Data.List.NonEmpty as NE (toList)

import qualified Language.Haskell.Exts as HSE

renderRaw :: (HSE.Pretty a) => a -> String
renderRaw = escapeNewlines . escapeQuotes . HSE.prettyPrintWithMode ppMode
  where
    ppMode = HSE.defaultMode {HSE.layout = HSE.PPNoLayout}
    escapeQuotes = replace "\"" "\\\\\\\""
    escapeNewlines = replace "\n" "\\n"

unsupportedExpr :: (HSE.Pretty a) => a -> AST.SMExpression
unsupportedExpr = AST.ERawExpression Nothing . renderRaw

unsupportedStmt :: (HSE.Pretty a) => a -> AST.SMStatement
unsupportedStmt = AST.SRawStatement Nothing . renderRaw

class ToExpr a where
  toExpr :: a b -> AST.SMExpression

class ToStmts a where
  toStmts :: a b -> [AST.SMStatement]

toId :: (ToExpr a) => a b -> String
toId x =
  let AST.EIdentifier _ sym = toExpr x
   in sym

instance ToExpr HSE.Name where
  toExpr (HSE.Ident _ name) = AST.EIdentifier Nothing name
  toExpr (HSE.Symbol _ name) = AST.EIdentifier Nothing name

instance ToExpr HSE.ModuleName where
  toExpr (HSE.ModuleName _ name) = AST.EIdentifier Nothing name

instance ToExpr HSE.CName where
  toExpr (HSE.VarName _ name) = toExpr name
  toExpr (HSE.ConName _ name) = toExpr name

instance ToExpr HSE.SpecialCon where
  toExpr HSE.UnitCon {} = AST.EIdentifier Nothing "Unit"
  toExpr HSE.ListCon {} = AST.EIdentifier Nothing "List"
  toExpr HSE.FunCon {} = AST.EIdentifier Nothing "->"
  toExpr (HSE.TupleCon _ _ arity) =
    AST.EIdentifier Nothing $ replicate arity ','
  toExpr HSE.Cons {} = AST.EIdentifier Nothing ":"
  toExpr expr@HSE.UnboxedSingleCon {} = unsupportedExpr expr
  toExpr HSE.ExprHole {} = AST.EIdentifier Nothing "_"

instance ToExpr HSE.QName where
  toExpr (HSE.Qual _ moduleName name) =
    AST.EIdentifier Nothing $ toId moduleName <> "." <> toId name
  toExpr (HSE.UnQual _ name) = toExpr name
  toExpr (HSE.Special _ specialCon) = toExpr specialCon

instance ToExpr HSE.TyVarBind where
  toExpr (HSE.UnkindedVar _ name) = toExpr name
  toExpr expr@HSE.KindedVar {} = unsupportedExpr expr

instance ToExpr HSE.MaybePromotedName where
  toExpr expr@HSE.PromotedName {} = unsupportedExpr expr
  toExpr (HSE.UnpromotedName _ name) = toExpr name

instance ToExpr HSE.Promoted where
  toExpr expr@HSE.PromotedInteger {} = unsupportedExpr expr
  toExpr expr@HSE.PromotedString {} = unsupportedExpr expr
  toExpr (HSE.PromotedCon _ _ con) = AST.EIdentifier Nothing $ '\'' : toId con
  toExpr expr@HSE.PromotedList {} = unsupportedExpr expr
  toExpr expr@HSE.PromotedTuple {} = unsupportedExpr expr
  toExpr expr@HSE.PromotedUnit {} = unsupportedExpr expr

instance ToExpr HSE.Type where
  toExpr expr@HSE.TyForall {} = unsupportedExpr expr
  toExpr (HSE.TyFun _ tyA tyB) =
    AST.EFunctionApplication $
    AST.FunctionApplication
      Nothing
      (AST.EIdentifier Nothing "->")
      [toExpr tyA, toExpr tyB]
  toExpr (HSE.TyTuple _ _ tys) =
    AST.EFunctionApplication $
    AST.FunctionApplication
      Nothing
      (AST.EIdentifier Nothing ",")
      (map toExpr tys)
  toExpr expr@HSE.TyUnboxedSum {} = unsupportedExpr expr
  toExpr (HSE.TyList _ ty) =
    AST.EFunctionApplication $
    AST.FunctionApplication Nothing (AST.EIdentifier Nothing "[]") [toExpr ty]
  toExpr expr@HSE.TyParArray {} = unsupportedExpr expr
  toExpr (HSE.TyApp _ tyA tyB) =
    AST.EFunctionApplication $
    AST.FunctionApplication Nothing (toExpr tyA) [toExpr tyB]
  toExpr (HSE.TyVar _ x) = toExpr x
  toExpr (HSE.TyCon _ x) = toExpr x
  toExpr (HSE.TyParen _ x) = toExpr x
  toExpr (HSE.TyInfix _ tyA mpn tyB) =
    AST.EFunctionApplication $
    AST.FunctionApplication Nothing (toExpr mpn) [toExpr tyA, toExpr tyB]
  toExpr expr@HSE.TyKind {} = unsupportedExpr expr
  toExpr (HSE.TyPromoted _ promoted) = toExpr promoted
  toExpr expr@HSE.TyEquals {} = unsupportedExpr expr
  toExpr expr@HSE.TySplice {} = unsupportedExpr expr
  toExpr expr@HSE.TyBang {} = unsupportedExpr expr
  toExpr (HSE.TyWildCard _ name) =
    AST.EIdentifier Nothing $ "_" <> maybe mempty toId name
  toExpr expr@HSE.TyQuasiQuote {} = unsupportedExpr expr

instance ToExpr HSE.ModuleHead where
  toExpr (HSE.ModuleHead _ name _ _) = toExpr name

instance ToStmts HSE.ModulePragma where
  toStmts (HSE.LanguagePragma _ pragmas) =
    map
      (\pragma -> AST.SPragma . AST.Pragma Nothing $ "LANGUAGE " <> toId pragma)
      pragmas
  toStmts stmt@HSE.OptionsPragma {} = [unsupportedStmt stmt]
  toStmts stmt@HSE.AnnModulePragma {} = [unsupportedStmt stmt]

instance ToStmts HSE.ImportDecl where
  toStmts stmt@(HSE.ImportDecl _ _ _ _ _ _ _ (Just (HSE.ImportSpecList _ True _))) =
    [unsupportedStmt stmt]
  toStmts stmt@(HSE.ImportDecl _ moduleName isQualified _ _ _ alias spec) =
    [ let moduleId = toId moduleName
       in if isQualified
            then case alias of
                   Just aliasName ->
                     AST.SQualifiedImport $
                     AST.QualifiedImport
                       Nothing
                       moduleId
                       (toId aliasName)
                       (importSpecListToExpr spec)
                   Nothing -> unsupportedStmt stmt
            else case alias of
                   Nothing ->
                     case importSpecListToExpr spec of
                       AST.ImportAll _ ->
                         AST.SUnrestrictedImport Nothing moduleId
                       AST.ImportOnly _ imports ->
                         AST.SRestrictedImport $
                         AST.RestrictedImport
                           Nothing
                           moduleId
                           (AST.ImportOnly Nothing imports)
                   Just _ -> unsupportedStmt stmt
    ]
    where
      importSpecListToExpr Nothing = AST.ImportAll Nothing
      importSpecListToExpr (Just (HSE.ImportSpecList _ False importSpecs)) =
        AST.ImportOnly Nothing $
        map
          (\case
             HSE.IVar _ name -> AST.ImportItem Nothing (toId name)
             HSE.IAbs _ _ name -> AST.ImportItem Nothing (toId name)
             HSE.IThingAll _ name -> AST.ImportType Nothing (toId name) [".."]
             HSE.IThingWith _ name items ->
               AST.ImportType Nothing (toId name) (map toId items))
          importSpecs

instance ToStmts HSE.Module where
  toStmts (HSE.Module _ moduleHead pragmas imports decls) =
    concat
      [ concatMap toStmts pragmas
      , case moduleHead of
          Just moduleId -> [AST.SModuleDeclaration Nothing (toId moduleId)]
          Nothing -> []
      , concatMap toStmts imports
      , concatMap toStmts decls
      ]

instance ToExpr HSE.QualConDecl where
  toExpr (HSE.QualConDecl _ _ _ conDecl) =
    case conDecl of
      HSE.ConDecl _ name args ->
        AST.EFunctionApplication $
        AST.FunctionApplication
          Nothing
          (AST.EIdentifier Nothing $ toId name)
          (map toExpr args)
      HSE.InfixConDecl _ argA name argB ->
        AST.EFunctionApplication $
        AST.FunctionApplication
          Nothing
          (AST.EIdentifier Nothing $ toId name)
          [toExpr argA, toExpr argB]
      HSE.RecDecl {} -> unsupportedExpr conDecl

instance ToExpr HSE.Literal where
  toExpr (HSE.Char _ char _) = AST.ELiteral $ AST.LChar Nothing char
  toExpr (HSE.String _ string _) = AST.ELiteral $ AST.LString Nothing string
  toExpr (HSE.Int _ int _) = AST.ELiteral $ AST.LInt Nothing (fromInteger int)
  toExpr expr@HSE.Frac {} = unsupportedExpr expr
  toExpr expr@HSE.PrimInt {} = unsupportedExpr expr
  toExpr expr@HSE.PrimWord {} = unsupportedExpr expr
  toExpr expr@HSE.PrimFloat {} = unsupportedExpr expr
  toExpr expr@HSE.PrimDouble {} = unsupportedExpr expr
  toExpr expr@HSE.PrimChar {} = unsupportedExpr expr
  toExpr expr@HSE.PrimString {} = unsupportedExpr expr

instance ToExpr HSE.Pat where
  toExpr (HSE.PVar _ name) = toExpr name
  toExpr (HSE.PLit _ _ literal) = toExpr literal
  toExpr expr@HSE.PNPlusK {} = unsupportedExpr expr
  toExpr (HSE.PInfixApp _ patA name patB) =
    AST.EFunctionApplication $
    AST.FunctionApplication Nothing (toExpr name) [toExpr patA, toExpr patB]
  toExpr (HSE.PApp _ name pats) =
    AST.EFunctionApplication $
    AST.FunctionApplication Nothing (toExpr name) (map toExpr pats)
  toExpr (HSE.PTuple _ _ pats) =
    AST.EFunctionApplication $
    AST.FunctionApplication
      Nothing
      (AST.EIdentifier Nothing ",")
      (map toExpr pats)
  toExpr expr@HSE.PUnboxedSum {} = unsupportedExpr expr
  toExpr (HSE.PList _ pats) =
    AST.EFunctionApplication $
    AST.FunctionApplication
      Nothing
      (AST.EIdentifier Nothing "list")
      (map toExpr pats)
  toExpr (HSE.PParen _ pat) = toExpr pat
  toExpr expr@HSE.PRec {} = unsupportedExpr expr
  toExpr expr@HSE.PAsPat {} = unsupportedExpr expr
  toExpr HSE.PWildCard {} = AST.EIdentifier Nothing "_"
  toExpr expr@HSE.PIrrPat {} = unsupportedExpr expr
  toExpr expr@HSE.PatTypeSig {} = unsupportedExpr expr
  toExpr expr@HSE.PViewPat {} = unsupportedExpr expr
  toExpr expr@HSE.PRPat {} = unsupportedExpr expr
  toExpr expr@HSE.PXTag {} = unsupportedExpr expr
  toExpr expr@HSE.PXETag {} = unsupportedExpr expr
  toExpr expr@HSE.PXPcdata {} = unsupportedExpr expr
  toExpr expr@HSE.PXPatTag {} = unsupportedExpr expr
  toExpr expr@HSE.PXRPats {} = unsupportedExpr expr
  toExpr expr@HSE.PSplice {} = unsupportedExpr expr
  toExpr expr@HSE.PQuasiQuote {} = unsupportedExpr expr
  toExpr expr@HSE.PBangPat {} = unsupportedExpr expr

declHeadToTyDef :: HSE.DeclHead a -> AST.TypeDefinition (Maybe SM.Expression)
declHeadToTyDef (HSE.DHead _ name) = AST.ProperType Nothing $ toId name
declHeadToTyDef HSE.DHInfix {} =
  error "Postfix type declarations not supported!"
declHeadToTyDef (HSE.DHParen _ dh) = declHeadToTyDef dh
declHeadToTyDef (HSE.DHApp _ dh tvb) =
  AST.TypeConstructor Nothing $
  case dh of
    HSE.DHInfix _ tvb' name ->
      AST.FunctionApplication Nothing (toExpr name) [toExpr tvb', toExpr tvb]
    _ ->
      AST.FunctionApplication
        Nothing
        (tyDefToExpr $ declHeadToTyDef dh)
        [toExpr tvb]

tyDefToExpr ::
     AST.TypeDefinition (Maybe SM.Expression)
  -> AST.Expression (Maybe SM.Expression)
tyDefToExpr (AST.TypeConstructor _ tyCon) = AST.EFunctionApplication tyCon
tyDefToExpr (AST.ProperType _ ty) = AST.EIdentifier Nothing ty

exprToTyDef ::
     AST.Expression (Maybe SM.Expression)
  -> AST.TypeDefinition (Maybe SM.Expression)
exprToTyDef (AST.EIdentifier _ identifier) = AST.ProperType Nothing identifier
exprToTyDef (AST.EFunctionApplication funApp) =
  AST.TypeConstructor Nothing funApp

toFunApp ::
     AST.Expression (Maybe SM.Expression)
  -> AST.FunctionApplication (Maybe SM.Expression)
toFunApp (AST.EFunctionApplication funApp) = funApp
toFunApp (AST.EIdentifier _ sym) =
  AST.FunctionApplication Nothing (AST.EIdentifier Nothing sym) []

bindsToFunDefs ::
     Maybe (HSE.Binds a) -> [AST.FunctionDefinition (Maybe SM.Expression)]
bindsToFunDefs Nothing = []
bindsToFunDefs (Just (HSE.BDecls _ decls)) =
  map
    (\decl ->
       case toStmts decl of
         [AST.SFunctionDefinition funDef] -> funDef)
    decls
bindsToFunDefs (Just HSE.IPBinds {}) =
  error "Implicit parameters not supported!"

altToClause ::
     HSE.Alt a
  -> ( AST.Expression (Maybe SM.Expression)
     , AST.Expression (Maybe SM.Expression))
altToClause (HSE.Alt _ pat rhs _) = (toExpr pat, toExpr rhs)

instance ToExpr HSE.QOp where
  toExpr (HSE.QVarOp _ name) = toExpr name
  toExpr (HSE.QConOp _ name) = toExpr name

instance ToExpr HSE.Exp where
  toExpr (HSE.Var _ name) = toExpr name
  toExpr expr@HSE.OverloadedLabel {} = unsupportedExpr expr
  toExpr expr@HSE.IPVar {} = unsupportedExpr expr
  toExpr (HSE.Con _ name) = toExpr name
  toExpr (HSE.Lit _ lit) = toExpr lit
  toExpr (HSE.InfixApp _ a op b) =
    if toId op == "$"
      then AST.EFunctionApplication $
           AST.FunctionApplication Nothing (toExpr a) [toExpr b]
      else AST.EFunctionApplication $
           AST.FunctionApplication
             Nothing
             (AST.EIdentifier Nothing "applyInfix")
             [toExpr a, toExpr op, toExpr b]
  toExpr (HSE.App _ (HSE.App _ f a) b) =
    AST.EFunctionApplication $
    AST.FunctionApplication Nothing (toExpr f) [toExpr a, toExpr b]
  toExpr (HSE.App _ f x) =
    AST.EFunctionApplication $
    AST.FunctionApplication Nothing (toExpr f) [toExpr x]
  toExpr expr@HSE.NegApp {} = unsupportedExpr expr
  toExpr (HSE.Lambda _ args body) =
    AST.ELambda $ AST.Lambda Nothing (map toExpr args) (toExpr body)
  toExpr (HSE.Let _ binds body) =
    AST.ELetBlock $ AST.LetBlock Nothing (bindsToClauses binds) (toExpr body)
  toExpr (HSE.If _ cond ifTrue ifFalse) =
    AST.EIfBlock $
    AST.IfBlock Nothing (toExpr cond) (toExpr ifTrue) (toExpr ifFalse)
  toExpr expr@HSE.MultiIf {} = unsupportedExpr expr
  toExpr (HSE.Case _ expr matches) =
    AST.ECaseBlock $
    AST.CaseBlock Nothing (toExpr expr) (map altToClause matches)
  toExpr (HSE.Do _ stmts) =
    AST.EFunctionApplication $
    AST.FunctionApplication
      Nothing
      (AST.EIdentifier Nothing "do'")
      (map handleStmt stmts)
    where
      handleStmt (HSE.Generator _ pat expr) =
        AST.EFunctionApplication $
        AST.FunctionApplication
          Nothing
          (AST.EIdentifier Nothing "<-")
          [toExpr pat, toExpr expr]
      handleStmt (HSE.Qualifier _ expr) = toExpr expr
      handleStmt (HSE.LetStmt _ binds) =
        AST.EFunctionApplication $
        AST.FunctionApplication
          Nothing
          (AST.EIdentifier Nothing "let")
          (map handleClause $ bindsToClauses binds)
      handleClause (var, val) =
        AST.EFunctionApplication $ AST.FunctionApplication Nothing var [val]
  toExpr expr@HSE.MDo {} = unsupportedExpr expr
  toExpr (HSE.Tuple _ _ exps) =
    AST.EFunctionApplication $
    AST.FunctionApplication
      Nothing
      (AST.EIdentifier Nothing $ replicate (length exps) ',')
      (map toExpr exps)
  toExpr expr@HSE.UnboxedSum {} = unsupportedExpr expr
  toExpr expr@HSE.TupleSection {} = unsupportedExpr expr
  toExpr (HSE.List _ items) =
    AST.EFunctionApplication $
    AST.FunctionApplication
      Nothing
      (AST.EIdentifier Nothing "list")
      (map toExpr items)
  toExpr expr@HSE.ParArray {} = unsupportedExpr expr
  toExpr (HSE.Paren _ expr) = toExpr expr
  toExpr expr@HSE.LeftSection {} = unsupportedExpr expr
  toExpr expr@HSE.RightSection {} = unsupportedExpr expr
  toExpr expr@HSE.RecConstr {} = unsupportedExpr expr
  toExpr expr@HSE.RecUpdate {} = unsupportedExpr expr
  toExpr expr@HSE.EnumFrom {} = unsupportedExpr expr
  toExpr expr@HSE.EnumFromTo {} = unsupportedExpr expr
  toExpr expr@HSE.EnumFromThen {} = unsupportedExpr expr
  toExpr expr@HSE.EnumFromThenTo {} = unsupportedExpr expr
  toExpr expr@HSE.ListComp {} = unsupportedExpr expr
  toExpr expr@HSE.ParComp {} = unsupportedExpr expr
  toExpr expr@HSE.ParArrayComp {} = unsupportedExpr expr
  toExpr expr@HSE.ParArrayFromTo {} = unsupportedExpr expr
  toExpr expr@HSE.ParArrayFromThenTo {} = unsupportedExpr expr
  toExpr expr@HSE.ExpTypeSig {} = unsupportedExpr expr
  toExpr expr@HSE.VarQuote {} = unsupportedExpr expr
  toExpr expr@HSE.TypQuote {} = unsupportedExpr expr
  toExpr expr@HSE.BracketExp {} = unsupportedExpr expr
  toExpr expr@HSE.SpliceExp {} = unsupportedExpr expr
  toExpr expr@HSE.QuasiQuote {} = unsupportedExpr expr
  toExpr expr@HSE.TypeApp {} = unsupportedExpr expr
  toExpr expr@HSE.XTag {} = unsupportedExpr expr
  toExpr expr@HSE.XETag {} = unsupportedExpr expr
  toExpr expr@HSE.XPcdata {} = unsupportedExpr expr
  toExpr expr@HSE.XExpTag {} = unsupportedExpr expr
  toExpr expr@HSE.XChildTag {} = unsupportedExpr expr
  toExpr expr@HSE.CorePragma {} = unsupportedExpr expr
  toExpr expr@HSE.SCCPragma {} = unsupportedExpr expr
  toExpr expr@HSE.GenPragma {} = unsupportedExpr expr
  toExpr expr@HSE.Proc {} = unsupportedExpr expr
  toExpr expr@HSE.LeftArrApp {} = unsupportedExpr expr
  toExpr expr@HSE.RightArrApp {} = unsupportedExpr expr
  toExpr expr@HSE.LeftArrHighApp {} = unsupportedExpr expr
  toExpr expr@HSE.RightArrHighApp {} = unsupportedExpr expr
  toExpr expr@HSE.LCase {} = unsupportedExpr expr

instance ToExpr HSE.Rhs where
  toExpr (HSE.UnGuardedRhs _ expr) = toExpr expr
  toExpr expr@HSE.GuardedRhss {} = unsupportedExpr expr

bindsToClauses ::
     HSE.Binds a
  -> [( AST.Expression (Maybe SM.Expression)
      , AST.Expression (Maybe SM.Expression))]
bindsToClauses (HSE.BDecls _ decls) =
  map (\(HSE.PatBind _ pat body _) -> (toExpr pat, toExpr body)) decls

instance ToStmts HSE.Match where
  toStmts (HSE.Match _ fn pats body whereBinds) =
    [ AST.SFunctionDefinition $
      AST.FunctionDefinition
        Nothing
        (toId fn)
        (map toExpr pats)
        (toExpr body)
        (bindsToFunDefs whereBinds)
    ]
  toStmts (HSE.InfixMatch _ pat fn pats body whereBinds) =
    [ AST.SFunctionDefinition $
      AST.FunctionDefinition
        Nothing
        (toId fn)
        (toExpr pat : map toExpr pats)
        (toExpr body)
        (bindsToFunDefs whereBinds)
    ]

instance ToExpr HSE.InstHead where
  toExpr (HSE.IHCon _ name) = toExpr name
  toExpr (HSE.IHInfix _ ty name) =
    AST.EFunctionApplication $
    AST.FunctionApplication Nothing (toExpr ty) [toExpr name]
  toExpr (HSE.IHParen _ instHead) = toExpr instHead
  toExpr (HSE.IHApp _ instHead ty) =
    AST.EFunctionApplication $
    AST.FunctionApplication Nothing (toExpr instHead) [toExpr ty]

instance ToExpr HSE.InstRule where
  toExpr (HSE.IRule _ _ Nothing instHead) = toExpr instHead
  toExpr (HSE.IParen _ rule) = toExpr rule

instance ToStmts HSE.Decl where
  toStmts (HSE.TypeDecl _ declHead ty) =
    [ AST.STypeSynonym $
      AST.TypeSynonym
        Nothing
        (tyDefToExpr $ declHeadToTyDef declHead)
        (toExpr ty)
    ]
  toStmts stmt@HSE.TypeFamDecl {} = [unsupportedStmt stmt]
  toStmts stmt@HSE.ClosedTypeFamDecl {} = [unsupportedStmt stmt]
  toStmts (HSE.DataDecl _ dataOrNew _ declHead cases _) =
    [ case dataOrNew of
        HSE.NewType _ ->
          AST.SNewtypeDeclaration $
          AST.NewtypeDeclaration
            Nothing
            (declHeadToTyDef declHead)
            (toFunApp $ toExpr $ unsafeHead cases)
        HSE.DataType _ ->
          AST.SDataDeclaration $
          AST.DataDeclaration
            Nothing
            (declHeadToTyDef declHead)
            (map (toFunApp . toExpr) cases)
    ]
  toStmts stmt@HSE.GDataDecl {} = [unsupportedStmt stmt]
  toStmts stmt@HSE.DataFamDecl {} = [unsupportedStmt stmt]
  toStmts stmt@HSE.TypeInsDecl {} = [unsupportedStmt stmt]
  toStmts stmt@HSE.DataInsDecl {} = [unsupportedStmt stmt]
  toStmts stmt@HSE.GDataInsDecl {} = [unsupportedStmt stmt]
  toStmts (HSE.ClassDecl _ ctxt declHead _ decls) =
    [ AST.STypeclassDefinition $
      AST.TypeclassDefinition
        Nothing
        (tyDefToExpr $ declHeadToTyDef declHead)
        (contextToExprs ctxt)
        (maybe [] (map classDeclToTySig) decls)
    ]
  toStmts (HSE.InstDecl _ _ rule decls) =
    [ AST.STypeclassInstance $
      AST.TypeclassInstance
        Nothing
        (toExpr rule)
        (maybe [] (map instDeclToFunDef) decls)
    ]
  toStmts stmt@HSE.DerivDecl {} = [unsupportedStmt stmt]
  toStmts stmt@HSE.InfixDecl {} = [unsupportedStmt stmt]
  toStmts (HSE.TypeSig _ names ty) =
    map
      (\name ->
         AST.STypeSignature $ AST.TypeSignature Nothing (toId name) (toExpr ty))
      names
  toStmts stmt@HSE.PatSynSig {} = [unsupportedStmt stmt]
  toStmts (HSE.FunBind _ cases) = concatMap toStmts cases
  toStmts (HSE.PatBind _ fn body whereBinds) =
    [ AST.SFunctionDefinition $
      AST.FunctionDefinition
        Nothing
        (toId fn)
        []
        (toExpr body)
        (bindsToFunDefs whereBinds)
    ]
  toStmts stmt@HSE.ForImp {} = [unsupportedStmt stmt]
  toStmts stmt@HSE.ForExp {} = [unsupportedStmt stmt]
  toStmts stmt@HSE.RulePragmaDecl {} = [unsupportedStmt stmt]
  toStmts stmt@HSE.DeprPragmaDecl {} = [unsupportedStmt stmt]
  toStmts stmt@HSE.WarnPragmaDecl {} = [unsupportedStmt stmt]
  toStmts stmt@HSE.InlineSig {} = [unsupportedStmt stmt]
  toStmts stmt@HSE.InlineConlikeSig {} = [unsupportedStmt stmt]
  toStmts stmt@HSE.SpecSig {} = [unsupportedStmt stmt]
  toStmts stmt@HSE.SpecInlineSig {} = [unsupportedStmt stmt]
  toStmts stmt@HSE.InstSig {} = [unsupportedStmt stmt]
  toStmts stmt@HSE.AnnPragma {} = [unsupportedStmt stmt]
  toStmts stmt@HSE.MinimalPragma {} = [unsupportedStmt stmt]
  toStmts stmt@HSE.RoleAnnotDecl {} = [unsupportedStmt stmt]
  toStmts stmt@HSE.CompletePragma {} = [unsupportedStmt stmt]

instDeclToFunDef ::
     HSE.InstDecl a -> AST.FunctionDefinition (Maybe SM.Expression)
instDeclToFunDef (HSE.InsDecl _ decl) =
  case unsafeHead $ toStmts decl of
    AST.SFunctionDefinition funDef -> funDef
instDeclToFunDef HSE.InsType {} = error "Type families not supported!"
instDeclToFunDef HSE.InsData {} = error "Type families not supported!"
instDeclToFunDef HSE.InsGData {} = error "Type families not supported!"

classDeclToTySig :: HSE.ClassDecl a -> AST.TypeSignature (Maybe SM.Expression)
classDeclToTySig (HSE.ClsDecl _ decl) =
  case toStmts decl of
    [AST.STypeSignature tySig] -> tySig
classDeclToTySig HSE.ClsDataFam {} = error "Type families not supported!"
classDeclToTySig HSE.ClsTyFam {} = error "Type families not supported!"
classDeclToTySig HSE.ClsTyDef {} = error "Type families not supported!"
classDeclToTySig HSE.ClsDefSig {} = error "Default signatures not supported!"

instance ToExpr HSE.Asst where
  toExpr (HSE.ClassA _ name tys) =
    AST.EFunctionApplication $
    AST.FunctionApplication Nothing (toExpr name) (map toExpr tys)
  toExpr (HSE.WildCardA _ _) = AST.EIdentifier Nothing "_"

contextToExprs ::
     Maybe (HSE.Context a) -> [AST.Expression (Maybe SM.Expression)]
contextToExprs Nothing = []
contextToExprs (Just (HSE.CxSingle _ asst)) = [toExpr asst]
contextToExprs (Just (HSE.CxTuple _ assts)) = map toExpr assts
contextToExprs (Just (HSE.CxEmpty _)) = []

convertFile ::
     ( Sem.Member (Sem.Embed IO) effs
     , Sem.Members '[ Effs.Console, Sem.Error Error, Effs.FileSystem] effs
     )
  => FilePath
  -> FilePath
  -> Sem.Sem effs String
convertFile path newPath = do
  parsedModule <-
    Sem.embed (HSE.parseFile path) >>=
    (\case
       HSE.ParseOk parsedModule -> pure parsedModule
       HSE.ParseFailed _ err -> Sem.throw $ ConvertError path err)
  putStrLn $ "Writing " <> newPath <> "..."
  let newContents =
        unlines $
        map toAxel $
        groupFunctionDefinitions $
        flattenFunctionApplications $ toStmts parsedModule
  FS.writeFile newPath newContents
  pure newPath

flattenFunctionApplications :: [AST.SMStatement] -> [AST.SMStatement]
flattenFunctionApplications = map (biplate %~ (uniplate %~ handleExpr))
  where
    handleExpr :: AST.SMExpression -> AST.SMExpression
    handleExpr (AST.EFunctionApplication (AST.FunctionApplication ann (AST.EFunctionApplication (AST.FunctionApplication _ fn args)) args')) =
      AST.EFunctionApplication $ AST.FunctionApplication ann fn (args <> args')
    handleExpr x = x

groupFunctionDefinitions :: [AST.SMStatement] -> [SM.Expression]
groupFunctionDefinitions =
  let findFnName (AST.SFunctionDefinition fnDef) = Just $ fnDef ^. AST.name
      findFnName (AST.STypeSignature tySig) = Just $ tySig ^. AST.name
      findFnName _ = Nothing
      extractTySig ::
           ([AST.SMStatement], Maybe String)
        -> (([AST.SMStatement], [AST.SMStatement]), Maybe String)
      extractTySig = unannotated %~ removeOut (is AST._STypeSignature)
      transformFnDef (AST.SFunctionDefinition fnDef) =
        let whereBindings =
              case map
                     (denormalizeStatement . AST.SFunctionDefinition)
                     (fnDef ^. AST.whereBindings) of
                [] -> []
                xs -> [Parse.SExpression Nothing xs]
         in Parse.SExpression Nothing $
            [ Parse.SExpression Nothing $
              map denormalizeExpression (fnDef ^. AST.arguments)
            , denormalizeExpression (fnDef ^. AST.body)
            ] <>
            whereBindings
      transformFnDef _ = fatal "groupFunctionDefinitions" "0001"
   in stablyGroupAllWith findFnName >>>
      map (flattenAnnotations . extractTySig . (unannotated %~ NE.toList)) >>>
      concatMap
        (\(stmts, (tySigs, maybeFnName)) ->
           case maybeFnName of
             Nothing -> map denormalizeStatement stmts
             Just fnName ->
               case tySigs of
                 [AST.STypeSignature tySig] ->
                   [ Parse.SExpression Nothing $
                     Parse.Symbol Nothing "def" :
                     Parse.Symbol Nothing fnName :
                     denormalizeExpression (tySig ^. AST.typeDefinition) :
                     map transformFnDef stmts
                   ]
                 _ ->
                   error $
                   "Multiple type signatures found for: `" <> fnName <> "`!")
