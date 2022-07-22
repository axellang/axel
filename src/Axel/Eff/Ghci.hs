{-# LANGUAGE TemplateHaskell #-}

module Axel.Eff.Ghci where

import Axel.Prelude

import Control.Lens (op)
import Control.Monad (void)

import qualified Data.Text as T

import Effectful ((:>), Eff, Effect, IOE, liftIO)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.TH (makeEffect)

import Language.Haskell.Ghcid (startGhci, stopGhci)
import qualified Language.Haskell.Ghcid as Ghci

data Ghci :: Effect where
  Exec :: Ghci.Ghci -> Text -> Ghci m [Text]
  Start :: Ghci m Ghci.Ghci
  Stop :: Ghci.Ghci -> Ghci m ()

makeEffect ''Ghci

runGhci :: (IOE :> effs) => Eff (Ghci ': effs) a -> Eff effs a
runGhci =
  interpret $ \_ ->
    \case
      Exec ghci command ->
        liftIO $ map T.pack <$> Ghci.exec ghci (T.unpack command)
      Start ->
        liftIO $
        fst <$>
        startGhci
          "cabal repl --repl-options='-ignore-dot-ghci'" -- We don't want to load any of the project's modules
                                                       -- unless a macro explicitly requires them
                                                       -- (they might not even have been compiled yet!)
          Nothing
          mempty
      Stop ghci -> liftIO $ stopGhci ghci

addFiles :: (Ghci :> effs) => Ghci.Ghci -> [FilePath] -> Eff effs [Text]
addFiles ghci filePaths =
  exec ghci $ ":add " <> T.unwords (map (op FilePath) filePaths) -- TODO What if a file path contains a space?

enableJsonErrors :: (Ghci :> effs) => Ghci.Ghci -> Eff effs ()
enableJsonErrors ghci = void $ exec ghci ":set -ddump-json"

withGhci :: (Ghci :> effs) => Eff (Reader Ghci.Ghci ': effs) a -> Eff effs a
withGhci x = do
  ghci <- start
  enableJsonErrors ghci
  result <- runReader ghci x
  stop ghci
  pure result
