{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Eff.Error where

import Axel.Parse.AST (getAnn, toAxel)
import qualified Axel.Sourcemap as SM
import Axel.Utils.String (Renderer)

import Control.Monad ((>=>))

import Data.Semigroup ((<>))

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem

data Error where
  ConvertError :: FilePath -> String -> Error
  MacroError :: FilePath -> SM.Expression -> String -> Error
  NormalizeError :: FilePath -> String -> [SM.Expression] -> Error
  ParseError :: FilePath -> String -> Error
  ProjectError :: String -> Error

mkFileErrorMsg :: FilePath -> String -> String
mkFileErrorMsg filePath err = "While compiling '" <> filePath <> "':\n\n" <> err

renderErrorContext :: SM.Expression -> String
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
  err <> "\n\n" <> "Context:\n" <> unlines (map renderErrorContext context)
renderError (ParseError filePath err) = mkFileErrorMsg filePath err
renderError (ProjectError err) = err

fatal :: String -> String -> a
fatal context message = error $ "[FATAL] " <> context <> " - " <> message

unsafeRunError ::
     Renderer e -> Sem.Sem (Sem.Error e ': effs) a -> Sem.Sem effs a
unsafeRunError render =
  Sem.runError >=> either (errorWithoutStackTrace . render) pure -- TODO Don't(?) use `error(WithoutStackTrace)` directly
