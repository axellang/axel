{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Axel.Eff.Process where

import Axel.Prelude

import qualified Axel.Utils.Text as T (decodeUtf8Lazy, encodeUtf8Lazy)

import qualified Polysemy as Sem

import Data.Kind (Type)
import Data.Singletons (Sing, SingI, sing)
import Data.Singletons.TH (singletons)
import qualified Data.Text as T

import qualified System.Environment (getArgs)
import System.Exit (ExitCode)
import qualified System.Process.Typed as P
  ( byteStringInput
  , readProcess
  , runProcess
  , setStdin
  , shell
  )

$(singletons
    [d|
  
  data StreamSpecification = CreateStreams
                           | InheritStreams
  |])

type family StreamsHandler (a :: StreamSpecification) (f :: Type -> Type) :: Type

type instance StreamsHandler 'CreateStreams f =
     Text -> f (ExitCode, Text, Text)

type instance StreamsHandler 'InheritStreams f = f ExitCode

type ProcessRunner' (streamSpec :: StreamSpecification) f
   = forall streamsHandler. (streamsHandler ~ StreamsHandler streamSpec f) =>
                              streamsHandler

type ProcessRunnerPrimitive (streamSpec :: StreamSpecification) (f :: Type -> Type)
   = FilePath -> [Text] -> ProcessRunner' streamSpec f

type ProcessRunner (streamSpec :: StreamSpecification) (f :: Type -> Type)
   = (SingI streamSpec) =>
       ProcessRunner' streamSpec f

data Process m a where
  GetArgs :: Process m [Text]
  RunProcessCreatingStreams :: Text -> Text -> Process m (ExitCode, Text, Text)
  RunProcessInheritingStreams :: Text -> Process m ExitCode

Sem.makeSem ''Process

runProcess ::
     (Sem.Member (Sem.Embed IO) effs)
  => Sem.Sem (Process ': effs) a
  -> Sem.Sem effs a
runProcess =
  Sem.interpret $ \case
    GetArgs -> Sem.embed $ map T.pack <$> System.Environment.getArgs
    RunProcessCreatingStreams cmd stdin ->
      Sem.embed $ do
        let stdinStream = P.byteStringInput (T.encodeUtf8Lazy stdin)
        let config = P.setStdin stdinStream $ P.shell (T.unpack cmd)
        (exitCode, stdout, stderr) <- P.readProcess config
        pure (exitCode, T.decodeUtf8Lazy stdout, T.decodeUtf8Lazy stderr)
    RunProcessInheritingStreams cmd ->
      Sem.embed $ P.runProcess (P.shell $ T.unpack cmd)

execProcess ::
     forall (streamSpec :: StreamSpecification) effs. (Sem.Member Process effs)
  => Text
  -> ProcessRunner streamSpec (Sem.Sem effs)
execProcess cmd =
  case sing :: Sing streamSpec of
    SCreateStreams -> runProcessCreatingStreams cmd
    SInheritStreams -> runProcessInheritingStreams cmd
