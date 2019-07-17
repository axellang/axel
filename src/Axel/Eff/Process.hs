{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Eff.Process where

import Control.Monad.Freer (type (~>), Eff, LastMember, Member, interpretM)
import Control.Monad.Freer.TH (makeEffect)

import qualified Data.ByteString.Lazy.Char8 as B (pack, unpack)
import Data.Kind (Type)
import Data.Singletons (Sing, SingI, sing)
import Data.Singletons.TH (singletons)

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
     String -> f (ExitCode, String, String)

type instance StreamsHandler 'InheritStreams f = f ExitCode

type ProcessRunner' (streamSpec :: StreamSpecification) f
   = forall streamsHandler. (streamsHandler ~ StreamsHandler streamSpec f) =>
                              streamsHandler

type ProcessRunnerPrimitive (streamSpec :: StreamSpecification) (f :: Type -> Type)
   = FilePath -> [String] -> ProcessRunner' streamSpec f

type ProcessRunner (streamSpec :: StreamSpecification) (f :: Type -> Type)
   = (SingI streamSpec) =>
       ProcessRunner' streamSpec f

data Process r where
  GetArgs :: Process [String]
  RunProcessCreatingStreams
    :: String -> String -> Process (ExitCode, String, String)
  RunProcessInheritingStreams :: String -> Process ExitCode

makeEffect ''Process

runEff :: (LastMember IO effs) => Eff (Process ': effs) ~> Eff effs
runEff =
  interpretM
    (\case
       GetArgs -> System.Environment.getArgs
       RunProcessCreatingStreams cmd stdin -> do
         let stdinStream = P.byteStringInput (B.pack stdin)
         let config = P.setStdin stdinStream $ P.shell cmd
         (exitCode, stdout, stderr) <- P.readProcess config
         pure (exitCode, B.unpack stdout, B.unpack stderr)
       RunProcessInheritingStreams cmd -> P.runProcess (P.shell cmd))

runProcess ::
     forall (streamSpec :: StreamSpecification) effs. (Member Process effs)
  => String
  -> ProcessRunner streamSpec (Eff effs)
runProcess cmd =
  case sing :: Sing streamSpec of
    SCreateStreams -> runProcessCreatingStreams cmd
    SInheritStreams -> runProcessInheritingStreams cmd
