{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Axel.Haskell.Error where

import Axel.Haskell.FilePath (haskellPathToAxelPath)
import Axel.Macros (ModuleInfo)
import Axel.Sourcemap (SourcePosition(SourcePosition))
import qualified Axel.Sourcemap as SM (Output(Output), findOriginalPosition)
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
  unlines $ map (processStackOutputLine moduleInfo) $ lines stackOutput

processStackOutputLine :: ModuleInfo -> String -> String
processStackOutputLine moduleInfo line =
  fromMaybe line (tryProcessGhcOutput moduleInfo line)

tryProcessGhcOutput :: ModuleInfo -> String -> Maybe String
tryProcessGhcOutput moduleInfo line = do
  let obj `viewStr` field = T.unpack <$> (obj ^? field . _String)
  jsonLine <- Json.decode' @Json.Value (BL.pack line)
  msg <- jsonLine `viewStr` key "doc"
  let maybeAxelError = do
        sourceSpan <-
          do jsonSpan <- jsonLine ^? key "span"
             startPosition <-
               SourcePosition <$> (jsonSpan ^? key "startLine" . _Int) <*>
               (jsonSpan ^? key "startCol" . _Int)
             endPosition <-
               SourcePosition <$> (jsonSpan ^? key "endLine" . _Int) <*>
               (jsonSpan ^? key "endCol" . _Int)
             filePath <- jsonSpan `viewStr` key "file"
             pure (filePath, SourceSpan startPosition endPosition)
        toAxelError moduleInfo $ GhcError msg sourceSpan
  pure $ fromMaybe msg maybeAxelError

toAxelError :: ModuleInfo -> GhcError -> Maybe String
toAxelError moduleInfo ghcError = do
  SM.Output transpiledOutput <-
    M.lookup
      (haskellPathToAxelPath $ ghcError ^. transpiledSpan . _1)
      moduleInfo >>=
    snd
  let findPosition field =
        SM.findOriginalPosition
          transpiledOutput
          (ghcError ^. transpiledSpan . _2 . field)
  origStartPosition <- findPosition start
  origEndPosition <- findPosition end
  pure $ "\n\n\n\n" <> ghcError ^. message <>
    "\n\nThis error message is in terms of the transpiled output.\nIn terms of the Axel code, however, I'm pretty sure this error is originally in the file '" <>
    ghcError ^.
    transpiledSpan .
    _1 <>
    "'.\nCheck from position " <>
    show origStartPosition <>
    " to position " <>
    show origEndPosition <>
    ".\n\n"
