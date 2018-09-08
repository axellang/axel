{-# LANGUAGE ScopedTypeVariables #-}

module Axel.Haskell.Prettify where

import Language.Haskell.Exts.Parser (ParseResult(ParseFailed, ParseOk), parse)
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax (Module)

prettifyHaskell :: String -> String
prettifyHaskell input =
  case parse input of
    ParseOk (ast :: Module SrcSpanInfo) -> prettyPrint ast
    ParseFailed _ _ -> input
