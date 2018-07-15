{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Axel.GHC where

import Prelude
  ( Bool
  , Either(Left, Right)
  , Eq((==))
  , FilePath
  , IO
  , Int
  , Maybe
  , String
  , ($)
  , (&&)
  , (.)
  , any
  , concatMap
  , error
  , filter
  , fmap
  , map
  , pure
  )

import Axel.Error (Error(MacroError), fatal)

import Control.Lens.TH (makeFieldsNoPrefix)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as B (pack)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, mapMaybe)

import ErrUtils as GHC
  ( Severity(SevDump, SevError, SevFatal, SevInfo, SevInteractive,
         SevOutput, SevWarning)
  )

import GHC.Generics (Generic)

import Language.Haskell.Exts
  ( Decl(DataDecl, FunBind, InstDecl, PatBind, TypeDecl)
  , DeclHead(DHApp, DHInfix, DHParen, DHead)
  , InstHead(IHApp, IHCon, IHInfix, IHParen)
  , InstRule(IParen, IRule)
  , Match(InfixMatch, Match)
  , Module(Module)
  , Name(Ident, Symbol)
  , ParseResult(ParseFailed, ParseOk)
  , Pat(PVar)
  , SrcSpan(SrcSpan, srcSpanEndColumn, srcSpanEndLine, srcSpanFilename,
        srcSpanStartColumn, srcSpanStartLine)
  , SrcSpanInfo(srcInfoSpan)
  , parseFile
  , prettyPrint
  )

import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)

deriving instance Eq GHC.Severity

deriving instance Generic GHC.Severity

instance FromJSON GHC.Severity

data GHCSpan = GHCSpan
  { endCol :: Int
  , endLine :: Int
  , file :: String
  , startCol :: Int
  , startLine :: Int
  } deriving (Generic)

instance FromJSON GHCSpan

data GHCLogEntry = GHCLogEntry
  { doc :: String
  , severity :: GHC.Severity
  , span :: Maybe GHCSpan
  } deriving (Generic)

makeFieldsNoPrefix ''GHCLogEntry

instance FromJSON GHCLogEntry

newtype JSONLog =
  JSONLog String

ghcSpanToHSESpan :: GHCSpan -> SrcSpan
ghcSpanToHSESpan ghcLogEntry =
  SrcSpan
  { srcSpanFilename = normalizeFile $ file ghcLogEntry
  , srcSpanEndColumn = endCol ghcLogEntry
  , srcSpanEndLine = endLine ghcLogEntry
  , srcSpanStartColumn = startCol ghcLogEntry
  , srcSpanStartLine = startLine ghcLogEntry
  }
  where
    normalizeFile filePath =
      case filePath of
        '.':'/':normalizedFilePath -> normalizedFilePath
        _ -> filePath

isUnknownIdentifierError :: GHCLogEntry -> Bool
isUnknownIdentifierError ghcLogEntry =
  severity ghcLogEntry == SevError &&
  any (`isPrefixOf` doc ghcLogEntry) unknownErrors
  where
    unknownErrors :: [String]
    unknownErrors =
      ["Variable not in scope: ", "Not in scope: type constructor or class "]

declName :: Decl a -> String
declName =
  let nameToString :: Name a -> String
      nameToString (Ident _ x) = x
      nameToString (Symbol _ x) = x
      extractNameFromDeclHead :: DeclHead a -> String
      extractNameFromDeclHead (DHApp _ head _) = extractNameFromDeclHead head
      extractNameFromDeclHead (DHead _ name) = nameToString name
      extractNameFromDeclHead (DHInfix _ _ name) = nameToString name
      extractNameFromDeclHead (DHParen _ head) = extractNameFromDeclHead head
      extractNameFromInstHead :: InstHead a -> String
      extractNameFromInstHead (IHCon _ qName) = prettyPrint qName
      extractNameFromInstHead (IHApp _ head _) = extractNameFromInstHead head
      extractNameFromInstHead (IHInfix _ _ qName) = prettyPrint qName
      extractNameFromInstHead (IHParen _ head) = extractNameFromInstHead head
      extractNameFromInstRule :: InstRule a -> String
      extractNameFromInstRule (IParen _ rule) = extractNameFromInstRule rule
      extractNameFromInstRule (IRule _ _ _ instHead) =
        extractNameFromInstHead instHead
      extractNameFromMatch :: Match a -> String
      extractNameFromMatch (InfixMatch _ _ name _ _ _) = nameToString name
      extractNameFromMatch (Match _ name _ _ _) = nameToString name
  in \case
       DataDecl _ _ _ declHead _ _ -> extractNameFromDeclHead declHead
       FunBind _ matches ->
         case matches of
           match:_ -> extractNameFromMatch match
           [] -> error "1"
       InstDecl _ _ instRule _ -> extractNameFromInstRule instRule
       PatBind _ (PVar _ name) _ _ -> nameToString name
       TypeDecl _ declHead _ -> extractNameFromDeclHead declHead
       _ -> error "TODO: 0001"

-- TODO Rename to `ghcCompile`
buildWithGHC :: FilePath -> IO (Either (JSONLog, String) String)
buildWithGHC filePath = do
  (exitCode, stdout, stderr) <-
    readProcessWithExitCode
      "stack"
      ["--resolver", "lts-12.0", "ghc", "--", "-v0", "-ddump-json", filePath]
      ""
  case exitCode of
    ExitSuccess -> pure $ Right stdout
    ExitFailure _ -> pure $ Left (JSONLog stdout, stderr)

-- TODO Rename to `ghcInterpret`
runWithGHC :: (MonadError Error m, MonadIO m) => FilePath -> m String
runWithGHC filePath = do
  (exitCode, stdout, stderr) <-
    liftIO $ readProcessWithExitCode "stack" ["runghc", filePath] ""
  case exitCode of
    ExitSuccess -> pure stdout
    ExitFailure _ -> throwError $ MacroError stderr

extractInvalidDefinitionNames :: FilePath -> JSONLog -> IO [String]
extractInvalidDefinitionNames filePath (JSONLog ghcLog) = do
  parseResult <- parseFile filePath
  parsedDecls <-
    case parseResult of
      ParseFailed _ _ -> fatal "extractInvalidDefinitionNames" "0001"
      ParseOk (Module _ _ _ _ decls) -> pure decls
      _ -> fatal "extractInvalidDefinitionNames" "0002"
  let ghcLogEntries = fromMaybe [] $ decode (B.pack ghcLog) :: [GHCLogEntry]
  let errorSpans =
        mapMaybe (fmap ghcSpanToHSESpan . span) $
        filter isUnknownIdentifierError ghcLogEntries
  let invalidDecls =
        concatMap
          (\errorSpan ->
             filter
               (any $ \hseSpan -> srcInfoSpan hseSpan == errorSpan)
               parsedDecls)
          errorSpans
  pure $ map declName invalidDecls
