module Axel.Utils.Display where

import Axel.Prelude

import Axel.Sourcemap (Bracket, Delimiter)
import qualified Axel.Sourcemap as SM

surround :: Bracket -> Text -> Text
surround bracket x = SM.raw $ SM.surround bracket $ SM.unassociated x

delimit :: Delimiter -> [Text] -> Text
delimit delimiter xs = SM.raw $ SM.delimit delimiter $ map SM.unassociated xs

renderPragma :: Text -> Text
renderPragma pragma = "{-# " <> pragma <> " #-}"
