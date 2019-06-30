{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Error where

import Axel.Parse.AST (Expression, toAxel)

import Control.Monad ((>=>))
import Control.Monad.Freer (type (~>), Eff, LastMember, send)
import Control.Monad.Freer.Error (runError)
import qualified Control.Monad.Freer.Error as Effs (Error)

import Data.Semigroup ((<>))

data Error ann
  = ConvertError String
  | EvalError String
  | MacroError String
  | NormalizeError String [Expression ann]
  | ParseError String
  | ProjectError String

instance Show (Error ann) where
  show :: Error ann -> String
  show (ConvertError err) = err
  show (EvalError err) = err
  show (MacroError err) = err
  show (NormalizeError err context) =
    "error:\n" <> err <> "\n\n" <> "context:\n" <> unlines (map toAxel context)
  show (ParseError err) = err
  show (ProjectError err) = err

fatal :: String -> String -> a
fatal context message = error $ "[FATAL] " <> context <> " - " <> message

runEff :: (Show e, LastMember IO effs) => Eff (Effs.Error e ': effs) ~> Eff effs
runEff =
  runError >=> \case
    Left err -> send $ ioError $ userError $ show err
    Right x -> pure x
