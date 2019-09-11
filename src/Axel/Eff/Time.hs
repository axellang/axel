{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Eff.Time where

import Axel.Eff (unsafeEmbedIO)
import qualified Axel.Eff.Console as Effs

import qualified Data.Time as Time

import qualified Polysemy as Sem

data Time m a where
  GetCurrentTime :: Time m Time.UTCTime

Sem.makeSem ''Time

runTime ::
     (Sem.Member (Sem.Embed IO) effs)
  => Sem.Sem (Time ': effs) a
  -> Sem.Sem effs a
runTime =
  Sem.interpret $ \case
    GetCurrentTime -> Sem.embed Time.getCurrentTime

-- | Only use for debugging purposes.
reportTime ::
     (Sem.Members '[ Effs.Console, Time] effs)
  => String
  -> Sem.Sem effs a
  -> Sem.Sem effs a
reportTime message x = do
  startTime <- getCurrentTime
  result <- x
  endTime <- getCurrentTime
  let timeDelta =
        let Time.UTCTime currentDay _ = startTime
            timeDiff = Time.diffUTCTime endTime startTime
            utcDelta = Time.UTCTime currentDay (realToFrac timeDiff)
         in Time.formatTime Time.defaultTimeLocale "%S%Q" utcDelta
  Effs.putStrLn $ "\nACTION: " <> message <> "\nTIME: " <> timeDelta <> "\n"
  pure result

unsafeReportTime ::
     String
  -> Sem.Sem (Time ': Effs.Console ': Sem.Embed IO ': effs) a
  -> Sem.Sem effs a
unsafeReportTime message =
  unsafeEmbedIO . Effs.runConsole . runTime . reportTime message
