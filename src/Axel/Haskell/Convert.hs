-- TODO Integrate with the effects system used everywhere else (convert `error` to `throwError`, etc.)
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC
  -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

module Axel.Haskell.Convert where

import Axel.Prelude

import qualified Axel.AST as AST
import Axel.Denormalize (denormalizeExpression, denormalizeStatement)
import Axel.Eff.Console (putStrLn)
import qualified Axel.Eff.Console as Effs (Console)
import Axel.Eff.Error (Error(ConvertError), fatal)
import qualified Axel.Eff.FileSystem as Effs (FileSystem)
import qualified Axel.Eff.FileSystem as FS
import qualified Axel.Parse.AST as Parse
import Axel.Pretty (prettifyProgram)
import qualified Axel.Sourcemap as SM
import Axel.Utils.List (removeOut, stablyGroupAllWith)
import Axel.Utils.Tuple (flattenAnnotations, unannotated)

import Control.Category ((>>>))
import Control.Lens (op)
import Control.Lens.Extras (is)
import Control.Lens.Operators ((%~), (^.))

import Data.Data.Lens (biplate, uniplate)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem

import qualified Data.List.NonEmpty as NE

import qualified Language.Haskell.Exts as HSE

renderRaw :: (HSE.Pretty a) => a -> Text
renderRaw =
  escapeNewlines . escapeQuotes . T.pack . HSE.prettyPrintWithMode ppMode
  where
    ppMode = HSE.defaultMode {HSE.layout = HSE.PPNoLayout}
    escapeQuotes = T.replace "\"" "\\\\\\\""
    escapeNewlines = T.replace "\n" "\\n"

unsupportedExpr :: (HSE.Pretty a) => a -> AST.SMExpression
unsupportedExpr = AST.ERawExpression Nothing . renderRaw

unsupportedStmt :: (HSE.Pretty a) => a -> AST.SMStatement
unsupportedStmt = AST.SRawStatement Nothing . renderRaw

class ToExpr a where
  toExpr :: a b -> AST.SMExpression

class ToStmts a where
  toStmts :: a b -> [AST.SMStatement]

toId :: (ToExpr a) => a b -> Text
toId x =
  let AST.EIdentifier _ sym = toExpr x
   in sym

convertFile ::
     (Sem.Members '[ Effs.Console, Effs.FileSystem, Sem.Error Error, Effs.FileSystem] effs)
  => FilePath
  -> FilePath
  -> Sem.Sem effs FilePath
convertFile path newPath = do
  originalContents <- FS.readFile path
  parsedModule <-
    case HSE.parse @(HSE.Module HSE.SrcSpanInfo) (T.unpack originalContents) of
      HSE.ParseOk parsedModule -> pure parsedModule
      HSE.ParseFailed _ err -> Sem.throw $ ConvertError path (T.pack err)
  putStrLn $ "Writing " <> op FilePath newPath <> "..."
  let newContents =
        prettifyProgram $
        groupFunctionDefinitions $ flattenFunctionApplications []
  FS.writeFile newPath newContents
  _ <-
    error
      "Haskell to Axel conversion is in construction, see https://app.gitkraken.com/glo/board/Wunz108ztxUAEpyt/card/XYmtRxPb5QAPb0H9."
  pure newPath

-- convertFile path newPath = do
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
           ([AST.SMStatement], Maybe Text)
        -> (([AST.SMStatement], [AST.SMStatement]), Maybe Text)
      extractTySig = unannotated %~ removeOut (is AST._STypeSignature)
      transformFnDef (AST.SFunctionDefinition fnDef) =
        let whereBindings =
              case map denormalizeStatement (fnDef ^. AST.whereBindings) of
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
           fromMaybe (map denormalizeStatement stmts) $ do
             fnName <- maybeFnName
             case tySigs of
               [] -> Nothing
               [AST.STypeSignature tySig] ->
                 Just
                   [ Parse.SExpression Nothing $
                     Parse.Symbol Nothing "def" :
                     Parse.Symbol Nothing (T.unpack fnName) :
                     denormalizeExpression (tySig ^. AST.typeDefinition) :
                     map transformFnDef stmts
                   ]
               _ ->
                 error $
                 "Multiple type signatures found for: `" <> fnName <> "`!")
