{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Eff.Ghci where

import Control.Monad (void)

import qualified Polysemy as Sem
import qualified Polysemy.Reader as Sem

import Language.Haskell.Ghcid (startGhci, stopGhci)
import qualified Language.Haskell.Ghcid as Ghci

data Ghci m a where
  Exec :: Ghci.Ghci -> String -> Ghci m [String]
  Start :: Ghci m Ghci.Ghci
  Stop :: Ghci.Ghci -> Ghci m ()

Sem.makeSem ''Ghci

runGhci ::
     (Sem.Member (Sem.Embed IO) effs)
  => Sem.Sem (Ghci ': effs) a
  -> Sem.Sem effs a
runGhci =
  Sem.interpret $ \case
    Exec ghci command -> Sem.embed $ Ghci.exec ghci command
    Start ->
      Sem.embed $
      fst <$>
      startGhci
        "stack repl --no-load --ghc-options='-ignore-dot-ghci'" -- We don't want to load any of the project's modules
                                                                -- unless a macro explicitly requires them
                                                                -- (they might not even have been compiled yet!)
        Nothing
        mempty
    Stop ghci -> Sem.embed $ stopGhci ghci

addFiles ::
     (Sem.Member Ghci effs) => Ghci.Ghci -> [FilePath] -> Sem.Sem effs [String]
addFiles ghci filePaths =
  exec ghci $ ":add " <> unwords filePaths -- TODO What if a file path contains a space?

enableJsonErrors :: (Sem.Member Ghci effs) => Ghci.Ghci -> Sem.Sem effs ()
enableJsonErrors ghci = void $ exec ghci ":set -ddump-json"

withGhci ::
     (Sem.Member Ghci effs)
  => Sem.Sem (Sem.Reader Ghci.Ghci ': effs) a
  -> Sem.Sem effs a
withGhci x = do
  ghci <- start
  enableJsonErrors ghci
  result <- Sem.runReader ghci x
  stop ghci
  pure result
