{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Axel.Error where

import Axel.Parse.AST (Expression, toAxel)

import Control.Monad.Except (ExceptT, MonadError(throwError), runExceptT)

import Data.Semigroup ((<>))

import Text.Parsec (ParseError)

data Error
  = MacroError String
  | NormalizeError String
                   [Expression]
  | ParseError ParseError
  | ProjectError String

instance Show Error where
  show :: Error -> String
  show (MacroError err) = err
  show (NormalizeError err context) =
    "error:\n" <> err <> "\n\n" <> "context:\n" <> unlines (map toAxel context)
  show (ParseError err) = show err
  show (ProjectError err) = show err

fatal :: String -> String -> a
fatal context message = error $ "[FATAL] " <> context <> " - " <> message

toIO :: (MonadError IOError m) => ExceptT Error m a -> m a
toIO f = do
  result :: Either Error a <- runExceptT f
  case result of
    Left err -> throwError $ userError $ show err
    Right x -> pure x
