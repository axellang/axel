{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Axel.Eff.Time where

import Axel.Prelude

import qualified Axel.Eff.Console as Effs
import Axel.Eff.Unsafe (unsafeEmbedIO)

import qualified Data.Text as T
import qualified Data.Time as Time

import qualified Polysemy as Sem

{-# ANN module
          ("HLint: ignore Avoid restricted function" :: String)
        #-}

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
  => Text
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
         in T.pack $ Time.formatTime Time.defaultTimeLocale "%S%Q" utcDelta
  Effs.putStrLn $ "\nACTION: " <> message <> "\nTIME: " <> timeDelta <> "\n"
  pure result

unsafeReportTime ::
     Text
  -> Sem.Sem (Time ': Effs.Console ': Sem.Embed IO ': effs) a
  -> Sem.Sem effs a
unsafeReportTime message =
  unsafeEmbedIO . Effs.runConsole . runTime . reportTime message
