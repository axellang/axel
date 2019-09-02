{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Axel.Haskell.Error where

import Axel.Haskell.FilePath (haskellPathToAxelPath)
import Axel.Macros (ModuleInfo)
import Axel.Sourcemap (Position(Position), SourcePosition, renderSourcePosition)
import qualified Axel.Sourcemap as SM (Output(Output), findOriginalPosition)
import Axel.Utils.Json (_Int)
import Axel.Utils.String (indent)

import Control.Lens.Operators ((^.), (^?))
import Control.Lens.TH (makeFieldsNoPrefix)
import Control.Lens.Tuple (_1, _2)
import Control.Monad (join)

import qualified Data.Aeson as Json (Value, decode')
import Data.Aeson.Lens (_String, key)
import qualified Data.ByteString.Lazy.Char8 as BL (pack)
import qualified Data.Map as M (lookup)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (unpack)

data GhcError =
  GhcError
    { _message :: String
    , _sourcePosition :: SourcePosition
    }
  deriving (Show)

makeFieldsNoPrefix ''GhcError

processErrors :: ModuleInfo -> String -> String
processErrors moduleInfo stackOutput =
  unlines $ concatMap (processStackOutputLine moduleInfo) $ lines stackOutput

processStackOutputLine :: ModuleInfo -> String -> [String]
processStackOutputLine moduleInfo line =
  fromMaybe [line] (tryProcessGhcOutput moduleInfo line)

tryProcessGhcOutput :: ModuleInfo -> String -> Maybe [String]
tryProcessGhcOutput moduleInfo line = do
  let obj `viewStr` field = T.unpack <$> (obj ^? field . _String)
  jsonLine <- Json.decode' @Json.Value (BL.pack line)
  msg <- jsonLine `viewStr` key "doc"
  pure $ fromMaybe [msg] $ do
    jsonSpan <- jsonLine ^? key "span"
    startPosition <-
      Position <$> (jsonSpan ^? key "startLine" . _Int) <*>
      (jsonSpan ^? key "startCol" . _Int)
    filePath <- jsonSpan `viewStr` key "file"
    let haskellSourcePosition = (filePath, startPosition)
    let maybeAxelError =
          toAxelError moduleInfo $ GhcError msg haskellSourcePosition
    let haskellError =
          "\n" <> indent 4 msg <>
          "The above message is in terms of the generated Haskell, at " <>
          renderSourcePosition haskellSourcePosition <>
          ".\nIt couldn't be mapped to an Axel location (did a macro lose a sourcemapping annotation along the way?).\n"
    pure [fromMaybe haskellError maybeAxelError]

toAxelError :: ModuleInfo -> GhcError -> Maybe String
toAxelError moduleInfo ghcError = do
  let haskellPath = ghcError ^. sourcePosition . _1
  let haskellPosition = ghcError ^. sourcePosition . _2
  let axelPath = haskellPathToAxelPath haskellPath
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
