module Axel.Pretty where

import Axel.Prelude

import Axel.Parse (unhygenisizeIdentifier)
import Axel.Parse.AST
import qualified Axel.Sourcemap as SM
import Axel.Utils.Foldable (mapWithPrev)
import Axel.Utils.Recursion (bottomUpFmap)
import Axel.Utils.Text (handleCharEscapes)

import Control.Lens (ala, under)

import Data.Maybe (fromMaybe)
import Data.Semigroup (Max(Max))
import qualified Data.Text as T
import Data.Text.Lens (unpacked)
import qualified Prettyprinter as P
import Prettyprinter ((<+>))
import qualified Prettyprinter.Render.Text as P

render :: P.Doc a -> Text
render = P.renderStrict . P.layoutSmart (P.LayoutOptions P.Unbounded)

maximumExpressionLength :: Int
maximumExpressionLength = 60

columnWidth :: Text -> Int
columnWidth = ala Max foldMap . map T.length . T.lines

sexp :: Bool -> [P.Doc a] -> P.Doc a
sexp _ [] = mempty
sexp _ [x] = x
sexp canBalance (x:xs) =
  let align cons = x `cons` xs
      oneLiner = align (\y ys -> y <+> P.hsep ys)
      balanced = align (\y ys -> y <+> P.align (P.vsep ys))
      multiLiner = align (\y ys -> P.align (P.vsep (y : ys)))
      shortEnough doc = columnWidth (render doc) <= maximumExpressionLength
   in if | shortEnough oneLiner -> oneLiner
         | canBalance && shortEnough balanced -> balanced
         | otherwise -> multiLiner

privilegedFormToAxelPretty :: [Expression ann] -> P.Doc a
privilegedFormToAxelPretty (Symbol _ "data":name:rest) =
  "data" <+> sexp True (toAxelPretty name : map toAxelPretty rest)
privilegedFormToAxelPretty (Symbol _ "def":name:tySig:rest) =
  "def" <+>
  P.align
    (P.vsep
       [ toAxelPretty name <+> toAxelPretty tySig
       , sexp True (map toAxelPretty rest)
       ])
privilegedFormToAxelPretty (Symbol _ "defmacro":name:rest) =
  "defmacro" <+> sexp True (toAxelPretty name : map toAxelPretty rest)
privilegedFormToAxelPretty (Symbol _ "::":name:rest) =
  "::" <+> sexp True (toAxelPretty name : map toAxelPretty rest)
privilegedFormToAxelPretty (Symbol _ "=":name:args:rest) =
  "=" <+>
  sexp True ((toAxelPretty name <+> toAxelPretty args) : map toAxelPretty rest)
privilegedFormToAxelPretty (Symbol _ "=macro":name:args:rest) =
  "=macro" <+>
  sexp True ((toAxelPretty name <+> toAxelPretty args) : map toAxelPretty rest)
privilegedFormToAxelPretty (Symbol _ "let":bindings:rest) =
  "let" <+> sexp True (toAxelPretty bindings : map toAxelPretty rest)
privilegedFormToAxelPretty (Symbol _ "if":bindings:rest) =
  "if" <+> sexp True (toAxelPretty bindings : map toAxelPretty rest)
privilegedFormToAxelPretty (Symbol _ "case":bindings:rest) =
  "case" <+> sexp True (toAxelPretty bindings : map toAxelPretty rest)
privilegedFormToAxelPretty (Symbol _ "import":name:rest) =
  "import" <+> sexp True (toAxelPretty name : map toAxelPretty rest)
privilegedFormToAxelPretty (Symbol _ "importq":name:rest) =
  "importq" <+> sexp True (toAxelPretty name : map toAxelPretty rest)
privilegedFormToAxelPretty (Symbol _ "importm":name:rest) =
  "importm" <+> sexp True (toAxelPretty name : map toAxelPretty rest)
privilegedFormToAxelPretty xs = sexp True $ map toAxelPretty xs

toAxelPretty :: Expression ann -> P.Doc a
toAxelPretty (LiteralChar _ x) = "#\\" <> P.pretty x
toAxelPretty (LiteralFloat _ x) = P.pretty x
toAxelPretty (LiteralInt _ x) = P.pretty x
toAxelPretty (LiteralString _ x) =
  P.dquotes $ P.pretty (under unpacked handleCharEscapes x)
toAxelPretty (SExpression _ (Symbol _ "applyInfix":xs)) =
  P.braces $ sexp True (map toAxelPretty xs)
toAxelPretty (SExpression _ (Symbol _ "list":xs)) =
  P.brackets $ sexp False (map toAxelPretty xs)
toAxelPretty (SExpression _ [Symbol _ "quote", x]) = "\'" <> toAxelPretty x
toAxelPretty (SExpression _ [Symbol _ "quasiquote", x]) = "`" <> toAxelPretty x
toAxelPretty (SExpression _ [Symbol _ "unquote", x]) = "~" <> toAxelPretty x
toAxelPretty (SExpression _ [Symbol _ "unquoteSplicing", x]) =
  "~@" <> toAxelPretty x
toAxelPretty (Symbol _ x) = P.pretty x
toAxelPretty (SExpression _ xs) = P.parens $ privilegedFormToAxelPretty xs

spaceStatements :: [SM.Expression] -> [P.Doc a]
spaceStatements =
  mapWithPrev $ \prev x ->
    let isImport (SExpression _ (Symbol _ id':_)) =
          id' `elem` ["import", "importq", "importm"]
        isImport _ = False
        needsSpacing = not $ isImport x && maybe False isImport prev
        maybeSpacer =
          if needsSpacing
            then P.line
            else mempty
        unhygenisize =
          bottomUpFmap $ \case
            Symbol ann symbol -> Symbol ann $ unhygenisizeIdentifier symbol
            node -> node
     in toAxelPretty (unhygenisize x) <> maybeSpacer

prettifyProgram :: [SM.Expression] -> Text
prettifyProgram = render . P.vsep . spaceStatements
