{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Axel.Parse.AST where

import Axel.Utils.Maybe (foldMUntilNothing)
import Axel.Utils.Recursion
  ( ZipperRecursive(zipperBottomUpTraverse, zipperTopDownTraverse)
  , ZipperTraverse
  , bottomUpFmap
  )
import Axel.Utils.String (handleStringEscapes)
import Axel.Utils.Zipper (unsafeDown, unsafeUp)

import Control.Lens.TH (makePrisms)

import Data.Data (Data)
import Data.Generics.Uniplate.Data ()
import Data.Generics.Uniplate.Zipper
  ( Zipper
  , fromZipper
  , hole
  , replaceHole
  , right
  , zipper
  )
import Data.Hashable (Hashable)
import Data.Semigroup ((<>))

import GHC.Generics (Generic)

-- TODO `Expression` should probably be `Traversable`, use recursion schemes, etc.
--      We should provide `toFix` and `fromFix` functions for macros to take advantage of.
--      (Maybe all macros have the argument automatically `fromFix`-ed to make consumption simpler?)
data Expression ann
  = LiteralChar ann Char
  | LiteralInt ann Int
  | LiteralString ann String
  | SExpression ann [Expression ann]
  | Symbol ann String
  deriving (Eq, Data, Functor, Generic, Show)

makePrisms ''Expression

instance (Hashable ann) => Hashable (Expression ann)

-- TODO Derive this automatically.
getAnn :: Expression ann -> ann
getAnn (LiteralChar ann _) = ann
getAnn (LiteralInt ann _) = ann
getAnn (LiteralString ann _) = ann
getAnn (SExpression ann _) = ann
getAnn (Symbol ann _) = ann

instance (Data ann) => ZipperRecursive (Expression ann) where
  zipperBottomUpTraverse :: forall m. ZipperTraverse m (Expression ann)
  zipperBottomUpTraverse f = fmap fromZipper . go . zipper
    where
      go ::
           Zipper (Expression ann) (Expression ann)
        -> m (Zipper (Expression ann) (Expression ann))
      go z = do
        let recurse =
              case hole z of
                LiteralChar _ _ -> pure
                LiteralInt _ _ -> pure
                LiteralString _ _ -> pure
                SExpression _ [] -> pure
                SExpression _ _ ->
                  fmap unsafeUp . foldMUntilNothing right go . unsafeDown
                Symbol _ _ -> pure
        z' <- recurse z
        x <- f z'
        pure $ replaceHole x z'
  zipperTopDownTraverse :: forall m. ZipperTraverse m (Expression ann)
  zipperTopDownTraverse f = fmap fromZipper . go . zipper
    where
      go ::
           Zipper (Expression ann) (Expression ann)
        -> m (Zipper (Expression ann) (Expression ann))
      go z = do
        x <- f z
        let z' = replaceHole x z
        let recurse =
              case x of
                LiteralChar _ _ -> pure
                LiteralInt _ _ -> pure
                LiteralString _ _ -> pure
                SExpression _ [] -> pure
                SExpression _ _ ->
                  fmap unsafeUp . foldMUntilNothing right go . unsafeDown
                Symbol _ _ -> pure
        recurse z'

bottomUpFmapSplicing ::
     (Data ann)
  => (Expression ann -> [Expression ann])
  -> Expression ann
  -> Expression ann
bottomUpFmapSplicing f =
  bottomUpFmap $ \case
    SExpression ann' xs -> SExpression ann' $ concatMap f xs
    x -> x

toAxel :: Expression ann -> String
toAxel (LiteralChar _ x) = ['{', x, '}']
toAxel (LiteralInt _ x) = show x
toAxel (LiteralString _ xs) = "\"" <> handleStringEscapes xs <> "\""
toAxel (SExpression _ (Symbol _ "applyInfix":xs)) =
  "{" <> unwords (map toAxel xs) <> "}"
toAxel (SExpression _ (Symbol _ "list":xs)) =
  "[" <> unwords (map toAxel xs) <> "]"
toAxel (SExpression _ [Symbol _ "quote", x]) = '\'' : toAxel x
toAxel (SExpression _ [Symbol _ "quasiquote", x]) = '`' : toAxel x
toAxel (SExpression _ [Symbol _ "unquote", x]) = '~' : toAxel x
toAxel (SExpression _ [Symbol _ "unquoteSplicing", x]) = "~@" <> toAxel x
toAxel (SExpression _ xs) = "(" <> unwords (map toAxel xs) <> ")"
toAxel (Symbol _ x) = x

-- TODO Derive this with Template Haskell (it's currently very brittle)
quoteExpression :: (ann -> Expression ann) -> Expression ann -> Expression ann
quoteExpression quoteAnn (LiteralChar ann x) =
  SExpression
    ann
    [Symbol ann "AST.LiteralChar", quoteAnn ann, LiteralChar ann x]
quoteExpression quoteAnn (LiteralInt ann x) =
  SExpression ann [Symbol ann "AST.LiteralInt", quoteAnn ann, LiteralInt ann x]
quoteExpression quoteAnn (LiteralString ann x) =
  SExpression
    ann
    [Symbol ann "AST.LiteralString", quoteAnn ann, LiteralString ann x]
quoteExpression quoteAnn (SExpression ann xs) =
  SExpression
    ann
    [ Symbol ann "AST.SExpression"
    , quoteAnn ann
    , SExpression ann (Symbol ann "list" : map (quoteExpression quoteAnn) xs)
    ]
quoteExpression quoteAnn (Symbol ann x) =
  SExpression ann [Symbol ann "AST.Symbol", quoteAnn ann, LiteralString ann x]

-- | This allows splice-unquoting of both `[Expression]`s and `SExpression`s,
--   without requiring special syntax for each.
class ToExpressionList a where
  type Annotation a
  toExpressionList :: a -> [Expression (Annotation a)]

instance ToExpressionList [Expression ann] where
  type Annotation [Expression ann] = ann
  toExpressionList :: [Expression ann] -> [Expression ann]
  toExpressionList = id

-- | Because we do not have a way to statically ensure an `SExpression` is passed
--   (and not another one of `Expression`'s constructors instead),
--   we will error at compile-time if a macro attempts to splice-unquote inappropriately.
instance ToExpressionList (Expression ann) where
  type Annotation (Expression ann) = ann
  toExpressionList :: Expression ann -> [Expression ann]
  toExpressionList (SExpression _ xs) = xs
  toExpressionList x =
    error
      (toAxel x <>
       " cannot be splice-unquoted, because it is not an s-expression!")
