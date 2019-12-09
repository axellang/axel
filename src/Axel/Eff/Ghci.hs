{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Axel.Eff.Ghci where

import Axel.Prelude

import Control.Lens (op)
import Control.Monad (void)

import qualified Data.Text as T

import Language.Haskell.Ghcid (startGhci, stopGhci)
import qualified Language.Haskell.Ghcid as Ghci

import qualified Polysemy as Sem
import qualified Polysemy.Reader as Sem

data Ghci m a where
  Exec :: Ghci.Ghci -> Text -> Ghci m [Text]
  Start :: Ghci m Ghci.Ghci
  Stop :: Ghci.Ghci -> Ghci m ()

Sem.makeSem ''Ghci

runGhci ::
     (Sem.Member (Sem.Embed IO) effs)
  => Sem.Sem (Ghci ': effs) a
  -> Sem.Sem effs a
runGhci =
  Sem.interpret $ \case
    Exec ghci command ->
      Sem.embed $ map T.pack <$> Ghci.exec ghci (T.unpack command)
    Start ->
      Sem.embed $
      fst <$>
      startGhci
        "cabal repl --repl-options='-ignore-dot-ghci'" -- We don't want to load any of the project's modules
                                                       -- unless a macro explicitly requires them
                                                       -- (they might not even have been compiled yet!)
        Nothing
        mempty
    Stop ghci -> Sem.embed $ stopGhci ghci

addFiles ::
     (Sem.Member Ghci effs) => Ghci.Ghci -> [FilePath] -> Sem.Sem effs [Text]
addFiles ghci filePaths =
  exec ghci $ ":add " <> T.unwords (map (op FilePath) filePaths) -- TODO What if a file path contains a space?

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
