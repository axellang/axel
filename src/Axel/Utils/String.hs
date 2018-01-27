module Axel.Utils.String where

import qualified Data.Text as T (pack, replace, unpack)

replace :: String -> String -> String -> String
replace needle replacement haystack =
  T.unpack $ T.replace (T.pack needle) (T.pack replacement) (T.pack haystack)
