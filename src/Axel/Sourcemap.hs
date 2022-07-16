{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Axel.Sourcemap where

import Axel.Prelude

import Axel.Eff.Lens (assign, modifying)
import Axel.Eff.Loop (breakLoop)
import qualified Axel.Eff.Loop as Effs (runLoop)
import qualified Axel.Parse.AST as Parse
  ( Expression(LiteralInt, LiteralString, SExpression, Symbol)
  , quoteExpression
  )
import Axel.Utils.List (unsafeHead)
import Axel.Utils.Text (Renderer)
import Axel.Utils.Tuple (Annotated, annotate, annotation, unannotate)

import Control.Lens ((|>))
import Control.Lens.Operators ((^.))
import Control.Lens.TH (makeFieldsNoPrefix, makeWrapped)
import Control.Monad (when)

import Data.Data (Data)
import Data.Foldable (for_)
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.MonoTraversable (oconcatMap, ofor_)
import qualified Data.Text as T

import GHC.Generics (Generic)

import qualified Polysemy as Sem
import qualified Polysemy.State as Sem

data Position =
  Position
    { _line :: Int
    , _column :: Int
    }
  deriving (Data, Eq, Generic, Show)

makeFieldsNoPrefix ''Position

instance Hashable Position

-- NOTE We're using `String` instead of `FilePath` so that we don't have to rely
-- on `Axel.Prelude` or `-XOverloadedStrings` in user-facing code.
type SourcePosition = (String, Position)

renderSourcePosition :: Renderer SourcePosition
renderSourcePosition (filePath, position) =
  let filePath' =
        if filePath == ""
          then "<unknown>"
          else T.pack filePath
   in filePath' <>
      ":" <> showText (position ^. line) <> ":" <> showText (position ^. column)

type SourceMetadata = Maybe SourcePosition

newtype Output =
  Output [Annotated SourceMetadata Text]
  deriving (Eq, Show)

deriving instance Semigroup Output

deriving instance Monoid Output

makeWrapped ''Output

raw :: Output -> Text
raw (Output output) = oconcatMap unannotate output

unassociated :: Text -> Output
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
  let (startMetadata, endMetadata) =
        case x of
          Output [] -> (Nothing, Nothing)
          Output xs -> (unsafeHead xs ^. annotation, last xs ^. annotation)
      (open, closed) =
        case bracket of
          CurlyBraces -> ("{", "}")
          DoubleQuotes -> ("\"", "\"")
          Parentheses -> ("(", ")")
          SingleQuotes -> ("'", "'")
          SquareBrackets -> ("[", "]")
   in Output [annotate startMetadata open] <>
      x <> Output [annotate endMetadata closed]

data Delimiter
  = Commas
  | Newlines
  | Pipes
  | Semicolons
  | Spaces

delimit :: Delimiter -> [Output] -> Output
delimit delimiter =
  Output .
  tryInit . -- Remove unneeded final delimiter
  concatMap
    (\(Output x) ->
       let metadata =
             case x of
               [] -> Nothing
               (x':_) -> x' ^. annotation
        in x |> annotate metadata (renderDelimiter delimiter))
  where
    tryInit :: [a] -> [a]
    tryInit [] = []
    tryInit xs = init xs
    renderDelimiter :: Delimiter -> Text
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
--
--   TODO Make algorithm functional (assuming this can be cleanly done so).
findOriginalPosition ::
     forall ann. [Annotated ann Text] -> Position -> Maybe ann
findOriginalPosition output transPos =
  Sem.run $
  Sem.evalState (Position {_line = 1, _column = 0}) $
  Effs.runLoop $ do
    for_ output $ \chunk ->
      ofor_ (unannotate chunk) $ \char -> do
        if char == '\n'
          then do
            assign @Position column 0
            modifying @Position line succ
          else modifying @Position column succ
        Sem.get >>= \newSrcPos ->
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
quoteMaybe _ Nothing = Parse.Symbol Nothing "GHCPrelude.Nothing"
quoteMaybe quoter (Just x) =
  Parse.SExpression Nothing [Parse.Symbol Nothing "GHCPrelude.Just", quoter x]

quoteSourceMetadata :: Maybe SourcePosition -> Expression
quoteSourceMetadata = quoteMaybe $ quote2Tuple (quoteString, quotePosition)

quoteSMExpression :: Expression -> Expression
quoteSMExpression = Parse.quoteExpression quoteSourceMetadata

-- | Keys are the module file paths, and values are (the module name, the transpiled output).
type ModuleInfo = Map FilePath (Text, Maybe Output)
