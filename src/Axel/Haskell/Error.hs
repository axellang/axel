{-# LANGUAGE TemplateHaskell #-}

module Axel.Haskell.Error where

import Axel.Prelude

import Axel.Sourcemap
  ( ModuleInfo
  , Position(Position)
  , SourcePosition
  , renderSourcePosition
  )
import qualified Axel.Sourcemap as SM
import Axel.Utils.FilePath (replaceExtension)
import Axel.Utils.Json (_Int)
import Axel.Utils.Text (encodeUtf8Lazy, indent)

import Control.Lens.Operators ((^.), (^?))
import Control.Lens.TH (makeFieldsNoPrefix)
import Control.Lens.Tuple (_1, _2)
import Control.Monad (join)

import qualified Data.Aeson as Json
import Data.Aeson.Lens (_String, key)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

data GhcError =
  GhcError
    { _message :: Text
    , _sourcePosition :: SourcePosition
    }
  deriving (Show)

makeFieldsNoPrefix ''GhcError

processStackOutputLine :: ModuleInfo -> Text -> [Text]
processStackOutputLine moduleInfo line =
  fromMaybe [line] (tryProcessGhcOutput moduleInfo line)

tryProcessGhcOutput :: ModuleInfo -> Text -> Maybe [Text]
tryProcessGhcOutput moduleInfo line = do
  let obj `viewText` field = obj ^? field . _String
  jsonLine <- Json.decode' @Json.Value (encodeUtf8Lazy line)
  msg <- jsonLine `viewText` key "doc"
  pure $ fromMaybe [msg] $ do
    jsonSpan <- jsonLine ^? key "span"
    startPosition <-
      Position <$> (jsonSpan ^? key "startLine" . _Int) <*>
      (jsonSpan ^? key "startCol" . _Int)
    filePath <- jsonSpan `viewText` key "file"
    let haskellSourcePosition = (T.unpack filePath, startPosition)
    let maybeAxelError =
          toAxelError moduleInfo $ GhcError msg haskellSourcePosition
    let haskellError =
          "\n" <> indent 4 msg <>
          "The above message is in terms of the generated Haskell, at " <>
          renderSourcePosition haskellSourcePosition <>
          ".\nIt couldn't be mapped to an Axel location (did a macro lose a sourcemapping annotation along the way?).\n"
    pure [fromMaybe haskellError maybeAxelError]

toAxelError :: ModuleInfo -> GhcError -> Maybe Text
toAxelError moduleInfo ghcError = do
  let haskellPath = ghcError ^. sourcePosition . _1
  let haskellPosition = ghcError ^. sourcePosition . _2
  let axelPath = replaceExtension (FilePath $ T.pack haskellPath) "axel"
  let positionHint startPos = "at " <> renderSourcePosition startPos
  SM.Output transpiledOutput <- M.lookup axelPath moduleInfo >>= snd
  axelSourcePosition <-
    join $ SM.findOriginalPosition transpiledOutput haskellPosition
  pure $ "\n" <> indent 4 (ghcError ^. message) <>
    "The above message is in terms of the generated Haskell, " <>
    positionHint (haskellPath, haskellPosition) <>
    ".\nTry checking " <>
    positionHint axelSourcePosition <>
    ".\nIf the Axel code at that position doesn't seem related, something may have gone wrong during a macro expansion.\n"
