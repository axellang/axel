module Axel.Pretty where

import Axel.Prelude

import Axel.Parse.AST
import Axel.Utils.Text (handleCharEscapes)

import Control.Lens (under)

import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Text.Lens (unpacked)

import qualified Data.Text.Prettyprint.Doc as P
import Data.Text.Prettyprint.Doc ((<+>))
import qualified Data.Text.Prettyprint.Doc.Render.Text as P

render :: P.Doc a -> Text
render = P.renderStrict . P.layoutSmart (P.LayoutOptions P.Unbounded)

maximumExpressionLength :: Int
maximumExpressionLength = 28

sexp :: [Expression ann] -> P.Doc a
sexp [] = mempty
sexp [x] = toAxelPretty x
sexp (x:xs) =
  let align f = toAxelPretty x <+> f (map toAxelPretty xs)
      oneLiner = align P.hsep
      multiLiner = align (P.align . P.vsep)
   in if T.length (render oneLiner) > maximumExpressionLength
        then multiLiner
        else oneLiner

toAxelPretty :: Expression ann -> P.Doc a
toAxelPretty (LiteralChar _ x) = "#\\" <> P.pretty x
toAxelPretty (LiteralInt _ x) = P.pretty x
toAxelPretty (LiteralString _ x) =
  P.dquotes $ P.pretty (under unpacked handleCharEscapes x)
toAxelPretty (SExpression _ (Symbol _ "applyInfix":xs)) = P.braces $ sexp xs
toAxelPretty (SExpression _ (Symbol _ "list":xs)) = P.brackets $ sexp xs
toAxelPretty (SExpression _ [Symbol _ "quote", x]) = "\'" <> toAxelPretty x
toAxelPretty (SExpression _ [Symbol _ "quasiquote", x]) = "`" <> toAxelPretty x
toAxelPretty (SExpression _ [Symbol _ "unquote", x]) = "~" <> toAxelPretty x
toAxelPretty (SExpression _ [Symbol _ "unquoteSplicing", x]) =
  "~@" <> toAxelPretty x
toAxelPretty (SExpression _ xs) = P.parens $ sexp xs
toAxelPretty (Symbol _ x) = P.pretty x

stmtToAxelPretty :: Expression ann -> P.Doc a
stmtToAxelPretty stmt@(SExpression _ (Symbol _ "def":_)) =
  P.line <> toAxelPretty stmt
stmtToAxelPretty x = toAxelPretty x

prettifyProgram :: [Expression ann] -> Text
prettifyProgram = render . P.vsep . map stmtToAxelPretty
