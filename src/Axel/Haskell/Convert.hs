-- TODO Integrate with the effects system used everywhere else (convert `error` to `throwError`, etc.)
{-# OPTIONS_GHC
  -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

module Axel.Haskell.Convert where

import Axel.Prelude

import qualified Axel.AST as AST
import Axel.Denormalize (denormalizeStatement)
import Axel.Eff.Console (putStrLn)
import qualified Axel.Eff.Console as Effs (Console)
import Axel.Eff.Error (Error(ConvertError))
import qualified Axel.Eff.FileSystem as Effs (FileSystem)
import qualified Axel.Eff.FileSystem as FS
import Axel.Pretty (prettifyProgram)

import Control.Lens ((%~), op)

import Data.Data.Lens (biplate, uniplate)
import qualified Data.Text as T

import Effectful ((:>>))
import qualified Effectful as Eff
import qualified Effectful.Error.Static as Eff

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
     ('[ Effs.Console, Effs.FileSystem, Eff.Error Error, Effs.FileSystem] :>> effs)
  => FilePath
  -> FilePath
  -> Eff.Eff effs FilePath
convertFile path newPath = do
  originalContents <- FS.readFile path
  parsedModule <-
    case HSE.parse @(HSE.Module HSE.SrcSpanInfo) (T.unpack originalContents) of
      HSE.ParseOk parsedModule -> pure parsedModule
      HSE.ParseFailed _ err -> Eff.throwError $ ConvertError path (T.pack err)
  putStrLn $ "Writing " <> op FilePath newPath <> "..."
  let newContents =
        prettifyProgram $
        map denormalizeStatement $ flattenFunctionApplications []
  FS.writeFile newPath newContents
  _ <-
    error
      "Haskell to Axel conversion is in construction, see https://app.gitkraken.com/glo/board/Wunz108ztxUAEpyt/card/XYmtRxPb5QAPb0H9."
  pure newPath

flattenFunctionApplications :: [AST.SMStatement] -> [AST.SMStatement]
flattenFunctionApplications = map (biplate %~ (uniplate %~ handleExpr))
  where
    handleExpr :: AST.SMExpression -> AST.SMExpression
    handleExpr (AST.EFunctionApplication (AST.FunctionApplication ann (AST.EFunctionApplication (AST.FunctionApplication _ fn args)) args')) =
      AST.EFunctionApplication $ AST.FunctionApplication ann fn (args <> args')
    handleExpr x = x
