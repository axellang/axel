{- HLINT ignore "Avoid restricted function" -}
{-# LANGUAGE TemplateHaskell #-}

module Axel.Eff.Time where

import Axel.Prelude

import Axel.Eff ((:>>))
import qualified Axel.Eff.Console as Effs
import Axel.Eff.Unsafe (unsafeEmbedIO)

import qualified Data.Text as T
import qualified Data.Time as Time

import Effectful ((:>))
import qualified Effectful as Eff
import qualified Effectful.Dispatch.Dynamic as Eff
import qualified Effectful.TH as Eff

data Time :: Eff.Effect where
  GetCurrentTime :: Time m Time.UTCTime

Eff.makeEffect ''Time

runTime :: (Eff.IOE :> effs) => Eff.Eff (Time ': effs) a -> Eff.Eff effs a
runTime =
  Eff.interpret $ \_ ->
    \case
      GetCurrentTime -> Eff.liftIO Time.getCurrentTime

-- | Only use for debugging purposes.
reportTime ::
     ('[ Effs.Console, Time] :>> effs)
  => Text
  -> Eff.Eff effs a
  -> Eff.Eff effs a
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
  -> Eff.Eff (Time ': Effs.Console ': Eff.IOE ': effs) a
  -> Eff.Eff effs a
unsafeReportTime message =
  unsafeEmbedIO . Effs.runConsole . runTime . reportTime message
