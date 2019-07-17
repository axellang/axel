{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Error where

import Axel.Parse.AST (Expression, toAxel)

import Control.Monad ((>=>))
import Control.Monad.Freer (type (~>), Eff)
import qualified Control.Monad.Freer as Effs (run)
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

unsafeRunEff :: (Show e) => Eff (Effs.Error e ': effs) ~> Eff effs
unsafeRunEff = runError >=> either (error . show) pure -- TODO Don't(?) use `error` directly

unsafeIgnoreError :: (Show b) => Eff '[ Effs.Error b] a -> a
unsafeIgnoreError x =
  case Effs.run (runError x) of
    Left err -> error $ show err
    Right result -> result
