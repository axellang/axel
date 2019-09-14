module Axel.Utils.FilePath where

import Axel.Prelude

import Control.Lens ((%~), _Wrapping', op, op)

import Data.Function ((&))
import qualified Data.Text as T
import Data.Text.Lens (unpacked)

import qualified System.FilePath as FilePath

(</>) :: FilePath -> FilePath -> FilePath
(FilePath prefix) </> (FilePath suffix) =
  FilePath . T.pack $ (FilePath.</>) (T.unpack prefix) (T.unpack suffix)

(<.>) :: FilePath -> Text -> FilePath
(FilePath prefix) <.> suffix =
  FilePath . T.pack $ (FilePath.<.>) (T.unpack prefix) (T.unpack suffix)

stripExtension :: Text -> FilePath -> Maybe FilePath
stripExtension ext =
  (_Wrapping' FilePath . unpacked) $ FilePath.stripExtension (T.unpack ext)

replaceExtension :: FilePath -> Text -> FilePath
replaceExtension filePath newExt =
  filePath & _Wrapping' FilePath . unpacked %~
  flip FilePath.replaceExtension (T.unpack newExt)

takeFileName :: FilePath -> FilePath
takeFileName = _Wrapping' FilePath . unpacked %~ FilePath.takeFileName

takeBaseName :: FilePath -> FilePath
takeBaseName = _Wrapping' FilePath . unpacked %~ FilePath.takeBaseName

takeDirectory :: FilePath -> FilePath
takeDirectory = _Wrapping' FilePath . unpacked %~ FilePath.takeDirectory

splitDirectories :: FilePath -> [FilePath]
splitDirectories =
  map (FilePath . T.pack) . FilePath.splitDirectories . (T.unpack . op FilePath)

joinPath :: [FilePath] -> FilePath
joinPath =
  (FilePath . T.pack) . FilePath.joinPath . map (T.unpack . op FilePath)
