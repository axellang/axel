{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}

module Axel.Error where

import Axel.Parse.AST (Expression, toAxel)

import Data.Semigroup ((<>))

import Text.Parsec (ParseError)

data Error
  = MacroError String
  | NormalizeError String
                   [Expression]
  | ParseError ParseError

instance Show Error where
  show :: Error -> String
  show (MacroError err) = err
  show (NormalizeError err context) =
    "error:\n" <> err <> "\n\n" <> "context:\n" <> unlines (map toAxel context)
  show (ParseError err) = show err

fatal :: String -> String -> a
fatal context message = error $ "[FATAL] " <> context <> " - " <> message
