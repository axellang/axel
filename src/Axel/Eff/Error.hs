{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Axel.Eff.Error where

import Axel.Prelude

import Axel.Parse.AST (getAnn, toAxel)
import qualified Axel.Sourcemap as SM
import Axel.Utils.Text (Renderer)

import Control.Monad ((>=>))

import qualified Data.Text as T

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem

data Error where
  ConvertError :: FilePath -> Text -> Error
  MacroError :: FilePath -> SM.Expression -> Text -> Error
  NormalizeError :: FilePath -> Text -> [SM.Expression] -> Error
  ParseError :: FilePath -> Text -> Error
  ProjectError :: Text -> Error

mkFileErrorMsg :: FilePath -> Text -> Text
mkFileErrorMsg (FilePath filePath) err =
  "While compiling '" <> filePath <> "':\n\n" <> err

renderErrorContext :: SM.Expression -> Text
renderErrorContext expr =
  let sourcePosition =
        case getAnn expr of
          Just sp -> ", at " <> SM.renderSourcePosition sp
          Nothing -> ""
   in toAxel expr <> sourcePosition

renderError :: Renderer Error
renderError (ConvertError filePath err) = mkFileErrorMsg filePath err
renderError (MacroError filePath ctxt err) =
  mkFileErrorMsg filePath $
  "While expanding " <> renderErrorContext ctxt <> ":\n\n" <> err
renderError (NormalizeError filePath err context) =
  mkFileErrorMsg filePath $
  err <> "\n\n" <> "Context:\n" <> T.unlines (map renderErrorContext context)
renderError (ParseError filePath err) = mkFileErrorMsg filePath err
renderError (ProjectError err) = err

-- | This should ONLY be used for truly impossible situations.
fatal :: Text -> Text -> a
fatal context message = error $ "[FATAL] " <> context <> " - " <> message

unsafeRunError ::
     Renderer e -> Sem.Sem (Sem.Error e ': effs) a -> Sem.Sem effs a
unsafeRunError render =
  Sem.runError >=> either (errorWithoutStackTrace . render) pure -- TODO Don't(?) use `error(WithoutStackTrace)` directly
