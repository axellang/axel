{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Axel.Sourcemap where

import Axel.Eff.Lens (assign, modifying)
import Axel.Eff.Loop (breakLoop)
import qualified Axel.Eff.Loop as Effs (runLoop)
import qualified Axel.Parse.AST as Parse
  ( Expression(LiteralInt, LiteralString, SExpression, Symbol)
  , quoteExpression
  )
import Axel.Utils.Foldable (intercalate)
import Axel.Utils.Tuple (Annotated, annotate, annotation, unannotate)

import Control.Lens.Operators ((^.))
import Control.Lens.TH (makeFieldsNoPrefix, makeWrapped)
import Control.Monad (forM_, when)
import qualified Control.Monad.Freer as Effs (run)
import Control.Monad.Freer.State (get)
import qualified Control.Monad.Freer.State as Effs (evalState)

import Data.Data (Data)
import Data.Map (Map)

data Position =
  Position
    { _line :: Int
    , _column :: Int
    }
  deriving (Data, Eq, Show)

makeFieldsNoPrefix ''Position

type SourcePosition = (FilePath, Position)

renderSourcePosition :: SourcePosition -> String
renderSourcePosition (filePath, position) =
  let filePath' =
        if filePath == ""
          then "<unknown>"
          else filePath
   in filePath' <> ":" <> show (position ^. line) <> ":" <>
      show (position ^. column)

type SourceMetadata = Maybe SourcePosition

newtype Output =
  Output [Annotated SourceMetadata String]
  deriving (Eq, Show)

deriving instance Semigroup Output

deriving instance Monoid Output

makeWrapped ''Output

raw :: Output -> String
raw (Output output) = concatMap unannotate output

unassociated :: String -> Output
unassociated x = Output [annotate Nothing x]

type Expression = Parse.Expression SourceMetadata

isCompoundExpressionWrapperHead :: Expression -> Bool
isCompoundExpressionWrapperHead (Parse.Symbol _ "begin") = True
isCompoundExpressionWrapperHead _ = False

wrapCompoundExpressions :: [Expression] -> Expression
wrapCompoundExpressions stmts =
  Parse.SExpression Nothing (Parse.Symbol Nothing "begin" : stmts)

unwrapCompoundExpressions :: Expression -> [Expression]
unwrapCompoundExpressions (Parse.SExpression _ (Parse.Symbol _ "begin":stmts)) =
  stmts
unwrapCompoundExpressions _ =
  error "unwrapCompoundExpressions must be passed a top-level program!"

data Bracket
  = CurlyBraces
  | DoubleQuotes
  | Parentheses
  | SingleQuotes
  | SquareBrackets

surround :: Bracket -> Output -> Output
surround bracket x =
  let (open, closed) =
        case bracket of
          CurlyBraces -> ("{", "}")
          DoubleQuotes -> ("\"", "\"")
          Parentheses -> ("(", ")")
          SingleQuotes -> ("'", "'")
          SquareBrackets -> ("[", "]")
   in unassociated open <> x <> unassociated closed

data Delimiter
  = Commas
  | Newlines
  | Pipes
  | Semicolons
  | Spaces

delimit :: Delimiter -> [Output] -> Output
delimit delimiter = intercalate (unassociated $ renderDelimiter delimiter)
  where
    renderDelimiter Commas = ","
    renderDelimiter Newlines = "\n"
    renderDelimiter Pipes = "|"
    renderDelimiter Semicolons = ";"
    renderDelimiter Spaces = " "

renderBlock :: [Output] -> Output
renderBlock = surround CurlyBraces . delimit Semicolons

-- | Given a position in some transpiled output, find the corresponding
--   metadata in the original source.
--   Behavior is undefined if `column transPos == 0`.
--   TODO Make algorithm functional (assuming this can be cleanly done so).
findOriginalPosition ::
     forall ann. [Annotated ann String] -> Position -> Maybe ann
findOriginalPosition output transPos =
  Effs.run $
  Effs.evalState (Position {_line = 1, _column = 0}) $
  Effs.runLoop $ do
    forM_ output $ \chunk ->
      forM_ (unannotate chunk) $ \char -> do
        if char == '\n'
          then do
            assign @Position column 0
            modifying @Position line succ
          else modifying @Position column succ
        get >>= \newSrcPos ->
          when (newSrcPos == transPos) $ breakLoop (Just $ chunk ^. annotation)
    pure Nothing

-- TODO Derive this with Template Haskell (it's currently very brittle)
quoteString :: String -> Expression
quoteString = Parse.LiteralString Nothing

-- TODO Derive this with Template Haskell (it's currently very brittle)
quotePosition :: Position -> Expression
quotePosition sourcePosition =
  Parse.SExpression
    Nothing
    [ Parse.Symbol Nothing "SM.Position"
    , Parse.LiteralInt Nothing (sourcePosition ^. line)
    , Parse.LiteralInt Nothing (sourcePosition ^. column)
    ]

-- TODO Derive this with Template Haskell (it's currently very brittle)
quote2Tuple :: (a -> Expression, b -> Expression) -> (a, b) -> Expression
quote2Tuple (quoterA, quoterB) (a, b) =
  Parse.SExpression Nothing [Parse.Symbol Nothing ",", quoterA a, quoterB b]

-- TODO Derive this with Template Haskell (it's currently very brittle)
quoteMaybe :: (a -> Expression) -> Maybe a -> Expression
quoteMaybe _ Nothing = Parse.Symbol Nothing "Nothing"
quoteMaybe quoter (Just x) =
  Parse.SExpression Nothing [Parse.Symbol Nothing "Just", quoter x]

quoteSourceMetadata :: Maybe SourcePosition -> Expression
quoteSourceMetadata = quoteMaybe $ quote2Tuple (quoteString, quotePosition)

quoteSMExpression :: Expression -> Expression
quoteSMExpression = Parse.quoteExpression quoteSourceMetadata

-- | Keys are the module file paths, and values are (the module name, the transpiled output).
type ModuleInfo = Map FilePath (String, Maybe Output)
