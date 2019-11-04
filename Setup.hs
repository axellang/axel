-- Adapted from https://github.com/achirkin/easytensor/blob/4cc9a381ad09c7a013a121cc02231f0ac2de649d/dimensions/Setup.hs.
-- This is necessary in case users install Axel through Cabal, and fixes a Haddock bug raised by `polysemy-plugin`.
-- This module disables some errors and warnings during the Haddock pass (caused by compiler plugins and hs-boot).
module Main where

import Data.Semigroup ((<>))

import Distribution.Simple
import Distribution.Simple.Setup

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks {confHook = \a -> confHook simpleUserHooks a . tweakFlags}

tweakFlags :: ConfigFlags -> ConfigFlags
tweakFlags flags =
  flags {configProgramArgs = addHaddockArgs (configProgramArgs flags)}

addHaddockArgs :: [(String, [String])] -> [(String, [String])]
addHaddockArgs [] = [("haddock", newHaddockGhcArgs)]
addHaddockArgs (("haddock", args):otherProgsArgs) =
  ("haddock", args <> newHaddockGhcArgs) : otherProgsArgs
addHaddockArgs (progArgs:otherProgsArgs) =
  progArgs : addHaddockArgs otherProgsArgs

newHaddockGhcArgs :: [String]
newHaddockGhcArgs =
  [ "--optghc=-fdefer-type-errors"
  , "--optghc=-fno-warn-deferred-type-errors"
  , "--optghc=-fno-warn-missing-home-modules"
  ]
