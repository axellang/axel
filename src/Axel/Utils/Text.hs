{-# LANGUAGE TemplateHaskell #-}

module Axel.Utils.Text where

import Axel.Prelude

import Control.Lens.Combinators (_head)
import Control.Lens.Operators ((%~))

import qualified Data.ByteString.Lazy as B
import Data.Char (chr, ord, toUpper)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import GHC.Exts (IsString(fromString))

import Language.Haskell.TH.Quote (QuasiQuoter(QuasiQuoter))

capitalize :: Text -> Text
capitalize = _head %~ toUpper

-- Adapted from http://hackage.haskell.org/package/string-quote-0.0.1/docs/src/Data-String-Quote.html#s.
s :: QuasiQuoter
s =
  QuasiQuoter
    ((\a -> [|fromString a|]) . filter (/= '\r'))
    (error "Cannot use s as a pattern")
    (error "Cannot use s as a type")
    (error "Cannot use s as a dec")

handleCharEscapes :: Text -> Text
handleCharEscapes =
  T.concatMap $ \case
    '\\' -> "\\\\"
    c -> T.singleton c

-- TODO This renders very poorly in e.g. Fira Code Mono.
bold :: Text -> Text
bold = T.map boldCharacter
  where
    boldRanges = [('a', 'z', '𝗮'), ('A', 'Z', '𝗔'), ('0', '9', '𝟬')]
    boldDelta x =
      foldl
        (\acc (rangeStart, rangeEnd, boldStart) ->
           if x `elem` [rangeStart .. rangeEnd]
             then ord boldStart - ord rangeStart
             else acc)
        0
        boldRanges
    boldCharacter x = chr $ (+ boldDelta x) $ ord x

indent :: Int -> Text -> Text
indent width = T.unlines . map (T.replicate width " " <>) . T.lines

type Renderer a = a -> Text

encodeUtf8Lazy :: Text -> B.ByteString
encodeUtf8Lazy = B.fromStrict . T.encodeUtf8

decodeUtf8Lazy :: B.ByteString -> Text
decodeUtf8Lazy = T.decodeUtf8 . B.toStrict
