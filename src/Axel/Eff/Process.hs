{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Eff.Process where

import Control.Monad.Freer
  ( type (~>)
  , Eff
  , LastMember
  , Member
  , interpretM
  )
import Control.Monad.Freer.TH (makeEffect)

import Data.Singletons (Sing, SingI, sing)
import Data.Singletons.TH (singletons)

import qualified System.Environment (getArgs)
import System.Exit (ExitCode)
import qualified System.Process (readProcessWithExitCode)
import qualified System.Process.Typed (proc, runProcess)

$(singletons
    [d|

  data StreamSpecification = CreateStreams
                           | InheritStreams
  |])

type family StreamsHandler (a :: StreamSpecification) (f :: * -> *) :: *

type instance StreamsHandler 'CreateStreams f =
     String -> f (ExitCode, String, String)

type instance StreamsHandler 'InheritStreams f = f ExitCode

type ProcessRunner' (streamSpec :: StreamSpecification) f
   = forall streamsHandler. (streamsHandler ~ StreamsHandler streamSpec f) =>
                              streamsHandler

type ProcessRunnerPrimitive (streamSpec :: StreamSpecification) (f :: * -> *)
   = FilePath -> [String] -> ProcessRunner' streamSpec f

type ProcessRunner (streamSpec :: StreamSpecification) (f :: * -> *)
   = (SingI streamSpec) =>
       ProcessRunner' streamSpec f

data Process r where
  GetArgs :: Process [String]
  RunProcessCreatingStreams
    :: FilePath -> [String] -> String -> Process (ExitCode, String, String)
  RunProcessInheritingStreams :: FilePath -> [String] -> Process ExitCode

makeEffect ''Process

runEff :: (LastMember IO effs) => Eff (Process ': effs) ~> Eff effs
runEff =
  interpretM
    (\case
       GetArgs -> System.Environment.getArgs
       RunProcessCreatingStreams cmd args stdin ->
         System.Process.readProcessWithExitCode cmd args stdin
       RunProcessInheritingStreams cmd args ->
         System.Process.Typed.runProcess (System.Process.Typed.proc cmd args))

runProcess ::
     forall (streamSpec :: StreamSpecification) effs. (Member Process effs)
  => FilePath
  -> [String]
  -> ProcessRunner streamSpec (Eff effs)
runProcess cmd args =
  case sing :: Sing streamSpec of
    SCreateStreams -> runProcessCreatingStreams cmd args
    SInheritStreams -> runProcessInheritingStreams cmd args
