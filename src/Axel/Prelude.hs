{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Axel.Prelude
  ( module Axel.Prelude
  , module Data.Text
  , module Prelude
  ) where

import Control.Lens (Wrapped)

import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Generics (Generic)

import Prelude hiding
  ( FilePath
  , appendFile
  , error
  , errorWithoutStackTrace
  , putStr
  , putStrLn
  , readFile
  , writeFile
  )
import qualified Prelude

newtype FilePath =
  FilePath Text
  deriving (Eq, Generic, Ord, Monoid, Semigroup, Show, Data, Hashable)

instance Wrapped FilePath

showText :: (Show a) => a -> Text
showText = T.pack . show

error :: Text -> a
error = Prelude.error . T.unpack

errorWithoutStackTrace :: Text -> a
errorWithoutStackTrace = Prelude.errorWithoutStackTrace . T.unpack
