module Axel.Utils.Debug where

import Debug.Trace (trace, traceShow)

{-# ANN module "HLint: ignore Avoid restricted module" #-}

unsafeTee :: String -> String
unsafeTee x = trace x x

unsafeTeeS :: Show a => a -> a
unsafeTeeS x = traceShow x x

unsafeTee' :: (a -> String) -> a -> a
unsafeTee' f x = trace (f x) x

unsafeTeeS' :: Show b => (a -> b) -> a -> a
unsafeTeeS' f x = traceShow (f x) x
