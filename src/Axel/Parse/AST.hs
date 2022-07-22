{-# LANGUAGE TemplateHaskell #-}

-- | The definition of and utilities for Axel's Abstract Syntax Tree (AST).
module Axel.Parse.AST where

import Axel.Prelude

import Axel.Utils.Maybe (foldMUntilNothing)
import Axel.Utils.Recursion
  ( Traverse
  , ZipperRecursive(zipperBottomUpTraverse, zipperTopDownTraverse)
  , bottomUpFmap
  )
import Axel.Utils.Text (handleCharEscapes)
import Axel.Utils.Zipper (unsafeDown, unsafeUp)

import Control.Lens ((<|))
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
import Data.MonoTraversable (oconcatMap)
import qualified Data.Text as T

import GHC.Generics (Generic)

-- TODO `Expression` should probably be `Traversable`, use recursion schemes, etc.
--      We should provide `toFix` and `fromFix` functions for macros to take advantage of.
--      (Maybe all macros have the argument automatically `fromFix`-ed to make consumption simpler?)
-- NOTE We're using `String` instead of `Text` so that we don't have to rely
--      on `Axel.Prelude` in user-facing code.
-- | An Axel form's AST. All Axel code is parsed into nested applications of this data structure.
data Expression ann
  = LiteralChar ann Char -- ^ A character literal, e.g. @#\a@.
  | LiteralInt ann Int -- ^ An integer literal, e.g. @123@.
  | LiteralFloat ann Float -- ^ A floating point literal, e.g. @1.23@.
  | LiteralString ann String -- ^ A string literal, e.g @"test"@.
  | SExpression ann [Expression ann] -- ^ A parenthesized list of expressions, e.g. @(foo a b c)@.
  | Symbol ann String -- ^ An identifier, e.g. @foo@.
  deriving (Eq, Data, Functor, Generic, Show)

makePrisms ''Expression

instance (Hashable ann) => Hashable (Expression ann)

-- TODO Derive this automatically.
-- | Get the source metadata from an 'Expression'.
getAnn :: Expression ann -> ann
getAnn (LiteralChar ann _) = ann
getAnn (LiteralFloat ann _) = ann
getAnn (LiteralInt ann _) = ann
getAnn (LiteralString ann _) = ann
getAnn (SExpression ann _) = ann
getAnn (Symbol ann _) = ann

instance (Data ann) => ZipperRecursive (Expression ann) where
  zipperBottomUpTraverse ::
       forall m.
       Traverse m (Zipper (Expression ann) (Expression ann)) (Expression ann)
  zipperBottomUpTraverse f = fmap fromZipper . go . zipper
    where
      go ::
           Zipper (Expression ann) (Expression ann)
        -> m (Zipper (Expression ann) (Expression ann))
      go z = do
        let recurse =
              case hole z of
                LiteralChar _ _ -> pure
                LiteralFloat _ _ -> pure
                LiteralInt _ _ -> pure
                LiteralString _ _ -> pure
                SExpression _ [] -> pure
                SExpression _ _ ->
                  fmap unsafeUp . foldMUntilNothing right go . unsafeDown
                Symbol _ _ -> pure
        z' <- recurse z
        x <- f z'
        pure $ replaceHole x z'
  zipperTopDownTraverse ::
       forall m.
       Traverse m (Zipper (Expression ann) (Expression ann)) (Expression ann)
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
                LiteralFloat _ _ -> pure
                LiteralInt _ _ -> pure
                LiteralString _ _ -> pure
                SExpression _ [] -> pure
                SExpression _ _ ->
                  fmap unsafeUp . foldMUntilNothing right go . unsafeDown
                Symbol _ _ -> pure
        recurse z'

-- | Modify an 'Expression' tree bottom-up, replacing each node with multiple 'Expression's.
--   This works with every expression in an Axel program, including top-level statements.
--   Also see the methods in 'Axel.Utils.Recursion'.
bottomUpFmapSplicing ::
     (Data ann)
  => (Expression ann -> [Expression ann])
  -> Expression ann
  -> Expression ann
bottomUpFmapSplicing f =
  bottomUpFmap $ \case
    SExpression ann' xs -> SExpression ann' $ oconcatMap f xs
    x -> x

toAxel :: Expression ann -> Text
toAxel (LiteralChar _ x) = "#\\" <> T.singleton x
toAxel (LiteralFloat _ x) = showText x
toAxel (LiteralInt _ x) = showText x
toAxel (LiteralString _ xs) = "\"" <> handleCharEscapes (T.pack xs) <> "\""
toAxel (SExpression _ (Symbol _ "applyInfix":xs)) =
  "{" <> T.unwords (map toAxel xs) <> "}"
toAxel (SExpression _ (Symbol _ "list":xs)) =
  "[" <> T.unwords (map toAxel xs) <> "]"
toAxel (SExpression _ [Symbol _ "quote", x]) = '\'' <| toAxel x
toAxel (SExpression _ [Symbol _ "quasiquote", x]) = '`' <| toAxel x
toAxel (SExpression _ [Symbol _ "unquote", x]) = '~' <| toAxel x
toAxel (SExpression _ [Symbol _ "unquoteSplicing", x]) = "~@" <> toAxel x
toAxel (SExpression _ xs) = "(" <> T.unwords (map toAxel xs) <> ")"
toAxel (Symbol _ x) = T.pack x

-- NOTE We're using `String` instead of `Text` so that we don't have to rely
--      on `Axel.Prelude` in user-facing code.
-- | NOTE This is for internal use; you likely don't need to use this directly.
--
--   Convert an 'Expression' to valid Axel source.
toAxel' :: Expression ann -> String
toAxel' = T.unpack . toAxel

-- TODO Derive this with Template Haskell (it's currently very brittle)
-- | Quote an 'Expression' (this is what the @quote@ special form uses under-the-hood).
quoteExpression ::
     (ann -> Expression ann) -- ^ Process the source metadata attached to the expression.
                           --   The returned 'Expression', when rendered via 'toAxel'',
                           --   must evaluate to a valid Axel expression.
  -> Expression ann -- ^ The expression to quote.
  -> Expression ann
quoteExpression quoteAnn (LiteralChar ann x) =
  SExpression
    ann
    [Symbol ann "AST.LiteralChar", quoteAnn ann, LiteralChar ann x]
quoteExpression quoteAnn (LiteralFloat ann x) =
  SExpression
    ann
    [Symbol ann "AST.LiteralFloat", quoteAnn ann, LiteralFloat ann x]
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

-- | NOTE This is for internal use; you likely don't need to use this directly.
--
--   This allows splice-unquoting of both `[Expression]`s and `SExpression`s,
--   without requiring special syntax for each.
class ToExpressionList a where
  type Annotation a
  toExpressionList :: a -> [Expression (Annotation a)]

instance ToExpressionList [Expression ann] where
  type Annotation [Expression ann] = ann
  toExpressionList :: [Expression ann] -> [Expression ann]
  toExpressionList = id

-- | NOTE This is for internal use; you likely don't need to use this directly.
--
--   Because we do not have a way to statically ensure an `SExpression` is passed
--   (and not another one of `Expression`'s constructors instead),
--   we will error at compile-time if a macro attempts to splice-unquote inappropriately.
instance ToExpressionList (Expression ann) where
  type Annotation (Expression ann) = ann
  toExpressionList :: Expression ann -> [Expression ann]
  toExpressionList (SExpression _ xs) = xs
  toExpressionList x =
    error $
    toAxel x <> " cannot be splice-unquoted, because it is not an s-expression!"
