{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Axel.Eff.Error where

import Axel.Parse.AST (Expression, toAxel)

import Control.Monad ((>=>))

import Data.Semigroup ((<>))

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem

data Error where
  ConvertError :: FilePath -> String -> Error
  MacroError :: FilePath -> Expression ann -> String -> Error
  NormalizeError :: FilePath -> String -> [Expression ann] -> Error
  ParseError :: FilePath -> String -> Error
  ProjectError :: String -> Error

mkFileErrorMsg :: FilePath -> String -> String
mkFileErrorMsg filePath err = "While compiling '" <> filePath <> "':\n\n" <> err

-- | Render an error in human-readable format.
class RenderError a where
  renderError :: a -> String

instance RenderError String where
  renderError :: String -> String
  renderError = id

instance RenderError Error where
  renderError :: Error -> String
  renderError (ConvertError filePath err) = mkFileErrorMsg filePath err
  renderError (MacroError filePath ctxt err) =
    mkFileErrorMsg filePath $
    "While expanding " <> toAxel ctxt <> ":\n\n" <> err
  renderError (NormalizeError filePath err context) =
    mkFileErrorMsg filePath $
    err <> "\n\n" <> "Context:\n" <> unlines (map toAxel context)
  renderError (ParseError filePath err) = mkFileErrorMsg filePath err
  renderError (ProjectError err) = err

fatal :: String -> String -> a
fatal context message = error $ "[FATAL] " <> context <> " - " <> message

unsafeRunError ::
     (RenderError e) => Sem.Sem (Sem.Error e ': effs) a -> Sem.Sem effs a
unsafeRunError =
  Sem.runError >=> either (errorWithoutStackTrace . renderError) pure -- TODO Don't(?) use `error(WithoutStackTrace)` directly
