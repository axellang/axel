module Axel.Pretty where

import Axel.Prelude

import Axel.Parse.AST
import Axel.Utils.Foldable (mapWithPrev)
import Axel.Utils.Text (handleCharEscapes)

import Control.Lens (ala, under)

import Data.Maybe (fromMaybe)
import Data.Semigroup (Max(Max), (<>))
import qualified Data.Text as T
import Data.Text.Lens (unpacked)
import qualified Data.Text.Prettyprint.Doc as P
import Data.Text.Prettyprint.Doc ((<+>))
import qualified Data.Text.Prettyprint.Doc.Render.Text as P

render :: P.Doc a -> Text
render = P.renderStrict . P.layoutSmart (P.LayoutOptions P.Unbounded)

maximumExpressionLength :: Int
maximumExpressionLength = 60

columnWidth :: Text -> Int
columnWidth = ala Max foldMap . map T.length . T.lines

sexp :: [P.Doc a] -> P.Doc a
sexp [] = mempty
sexp [x] = x
sexp (x:xs) =
  let align cons = x `cons` xs
      oneLiner = align (\y ys -> y <+> P.hsep ys)
      balanced = align (\y ys -> y <+> P.align (P.vsep ys))
      multiLiner = align (\y ys -> P.align (P.vsep (y : ys)))
      shortEnough doc = columnWidth (render doc) <= maximumExpressionLength
   in if | shortEnough oneLiner -> oneLiner
         | shortEnough balanced -> balanced
         | otherwise -> multiLiner

privilegedFormToAxelPretty :: [Expression ann] -> P.Doc a
privilegedFormToAxelPretty (Symbol _ "data":name:rest) =
  "data" <+> sexp (toAxelPretty name : map toAxelPretty rest)
privilegedFormToAxelPretty (Symbol _ "def":name:tySig:rest) =
  "def" <+>
  P.align
    (P.vsep
       [ (toAxelPretty name <+> toAxelPretty tySig)
       , sexp (map toAxelPretty rest)
       ])
privilegedFormToAxelPretty (Symbol _ "defmacro":name:rest) =
  "defmacro" <+> sexp (toAxelPretty name : map toAxelPretty rest)
privilegedFormToAxelPretty (Symbol _ "::":name:rest) =
  "::" <+> sexp (toAxelPretty name : map toAxelPretty rest)
privilegedFormToAxelPretty (Symbol _ "=":name:args:rest) =
  "=" <+>
  sexp ((toAxelPretty name <+> toAxelPretty args) : map toAxelPretty rest)
privilegedFormToAxelPretty (Symbol _ "=macro":name:args:rest) =
  "=macro" <+>
  sexp ((toAxelPretty name <+> toAxelPretty args) : map toAxelPretty rest)
privilegedFormToAxelPretty (Symbol _ "let":bindings:rest) =
  "let" <+> sexp (toAxelPretty bindings : map toAxelPretty rest)
privilegedFormToAxelPretty (Symbol _ "if":bindings:rest) =
  "if" <+> sexp (toAxelPretty bindings : map toAxelPretty rest)
privilegedFormToAxelPretty (Symbol _ "case":bindings:rest) =
  "case" <+> sexp (toAxelPretty bindings : map toAxelPretty rest)
privilegedFormToAxelPretty xs = sexp $ map toAxelPretty xs

toAxelPretty :: Expression ann -> P.Doc a
toAxelPretty (LiteralChar _ x) = "#\\" <> P.pretty x
toAxelPretty (LiteralInt _ x) = P.pretty x
toAxelPretty (LiteralString _ x) =
  P.dquotes $ P.pretty (under unpacked handleCharEscapes x)
toAxelPretty (SExpression _ (Symbol _ "applyInfix":xs)) =
  P.braces $ sexp (map toAxelPretty xs)
toAxelPretty (SExpression _ (Symbol _ "list":xs)) =
  P.brackets $ sexp (map toAxelPretty xs)
toAxelPretty (SExpression _ [Symbol _ "quote", x]) = "\'" <> toAxelPretty x
toAxelPretty (SExpression _ [Symbol _ "quasiquote", x]) = "`" <> toAxelPretty x
toAxelPretty (SExpression _ [Symbol _ "unquote", x]) = "~" <> toAxelPretty x
toAxelPretty (SExpression _ [Symbol _ "unquoteSplicing", x]) =
  "~@" <> toAxelPretty x
toAxelPretty (Symbol _ x) = P.pretty x
toAxelPretty (SExpression _ xs) = P.parens $ privilegedFormToAxelPretty xs

spaceStatements :: [Expression ann] -> [P.Doc a]
spaceStatements =
  mapWithPrev $ \prev x ->
    let isImport (SExpression _ (Symbol _ id':_)) =
          id' `elem` ["import", "importq", "importm"]
        isImport _ = False
        needsSpacing = not $ isImport x && fromMaybe False (isImport <$> prev)
        maybeSpacer =
          if needsSpacing
            then P.line
            else mempty
     in toAxelPretty x <> maybeSpacer

prettifyProgram :: [Expression ann] -> Text
prettifyProgram = render . P.vsep . spaceStatements
