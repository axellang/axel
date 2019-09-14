module Axel.Utils.Debug where

import Axel.Prelude

import qualified Data.Text as T

import Debug.Trace (trace, traceShow)

{-# ANN module ("HLint: ignore Avoid restricted module" :: String)
        #-}

unsafeTee :: Text -> Text
unsafeTee x = trace (T.unpack x) x

unsafeTeeS :: (Show a) => a -> a
unsafeTeeS x = traceShow x x

unsafeTee' :: (a -> Text) -> a -> a
unsafeTee' f x = trace (T.unpack $ f x) x

unsafeTeeS' :: (Show b) => (a -> b) -> a -> a
unsafeTeeS' f x = traceShow (f x) x
