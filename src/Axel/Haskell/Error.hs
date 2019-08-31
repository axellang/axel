{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Axel.Haskell.Error where

import Axel.Haskell.FilePath (haskellPathToAxelPath)
import Axel.Macros (ModuleInfo)
import Axel.Sourcemap (SourcePosition(SourcePosition), renderSourcePosition)
import qualified Axel.Sourcemap as SM (Output(Output), findOriginalPosition)
import Axel.Utils.Debug
import Axel.Utils.Json (_Int)

import Control.Lens.Operators ((^.), (^?))
import Control.Lens.TH (makeFieldsNoPrefix)
import Control.Lens.Tuple (_1, _2)

import qualified Data.Aeson as Json (Value, decode')
import Data.Aeson.Lens (_String, key)
import qualified Data.ByteString.Lazy.Char8 as BL (pack)
import qualified Data.Map as M (lookup)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (unpack)

data SourceSpan =
  SourceSpan
    { _start :: SourcePosition
    , _end :: SourcePosition
    }
  deriving (Show)

makeFieldsNoPrefix ''SourceSpan

data GhcError =
  GhcError
    { _message :: String
    , _transpiledSpan :: (FilePath, SourceSpan)
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
  jsonSpan <- jsonLine ^? key "span"
  startPosition <-
    SourcePosition <$> (jsonSpan ^? key "startLine" . _Int) <*>
    (jsonSpan ^? key "startCol" . _Int)
  endPosition <-
    SourcePosition <$> (jsonSpan ^? key "endLine" . _Int) <*>
    (jsonSpan ^? key "endCol" . _Int)
  filePath <- jsonSpan `viewStr` key "file"
  let sourceSpan = SourceSpan startPosition endPosition
  let maybeAxelError =
        toAxelError moduleInfo $ GhcError msg (filePath, sourceSpan)
  let haskellError =
        "\n\nAt position " <> filePath <> ":" <>
        renderSourcePosition startPosition <>
        ":\n" <>
        msg <>
        "\n\n"
  pure $ [fromMaybe haskellError maybeAxelError]

toAxelError :: ModuleInfo -> GhcError -> Maybe String
toAxelError moduleInfo ghcError = do
  let haskellPath = ghcError ^. transpiledSpan . _1
  let haskellPosition = ghcError ^. transpiledSpan . _2 . start
  let axelPath = haskellPathToAxelPath haskellPath
  let positionHint filePath startPos = do
        pure $ "at " <> filePath <> ":" <> renderSourcePosition startPos
  haskellPositionHint <- positionHint haskellPath haskellPosition
  SM.Output transpiledOutput <- M.lookup axelPath moduleInfo >>= snd
  axelPosition <-
    SM.findOriginalPosition transpiledOutput haskellPosition >>= id
  axelPositionHint <- positionHint axelPath axelPosition
  pure $ "\n\n" <> ghcError ^. message <>
    "\n\nThe above message is in terms of the generated Haskell, " <>
    haskellPositionHint <>
    ".\nTry checking " <>
    axelPositionHint <>
    ".\nIf the Axel code at that position doesn't seem related, something may have gone wrong during a macro expansion.\n\n"
