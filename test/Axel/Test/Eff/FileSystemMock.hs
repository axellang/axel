{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Axel.Test.Monad.FileSystemMock where

import Axel.Eff.FileSystem as Effs
import Axel.Test.MockUtils

import Control.Lens hiding (children)
import Control.Monad.Freer
import Control.Monad.Freer.Error as Effs
import Control.Monad.Freer.State as Effs

import Data.List.Split
import Data.Maybe

import System.FilePath

data FSNode
  = Directory FilePath
              [FSNode]
  | File FilePath
         String
  deriving (Eq, Show)

fsPath :: Lens' FSNode FilePath
fsPath =
  lens
    (\case
       Directory path _ -> path
       File path _ -> path)
    (\node newPath ->
       case node of
         Directory _ children -> Directory newPath children
         File _ contents -> File newPath contents)

data FileSystemState = FileSystemState
  { _fsCurrentDirectory :: FilePath
  , _fsRoot :: FSNode
  , _fsTempCounter :: Int
  } deriving (Eq, Show)

makeFieldsNoPrefix ''FileSystemState

mkFileSystemState :: [FSNode] -> FileSystemState
mkFileSystemState rootContents =
  FileSystemState
    { _fsCurrentDirectory = "/"
    , _fsRoot = Directory "/" rootContents
    , _fsTempCounter = 0
    }

lookupNode :: [FilePath] -> FSNode -> Maybe FSNode
lookupNode _ (File _ _) = Nothing
lookupNode [] _ = Nothing
lookupNode ["."] rootNode = pure rootNode
lookupNode [segment] (Directory _ rootChildren) =
  let matchingChildren =
        filter (\child -> child ^. fsPath == segment) rootChildren
   in case matchingChildren of
        [child] -> pure child
        _ -> Nothing
lookupNode (segment:xs) rootNode@(Directory _ _) = do
  child@(Directory _ _) <- lookupNode [segment] rootNode
  lookupNode xs child

deleteNode :: [FilePath] -> FSNode -> Maybe FSNode
deleteNode _ (File _ _) = Nothing
deleteNode [] _ = Nothing
deleteNode [segment] rootNode@(Directory rootPath rootChildren) = do
  child <- lookupNode [segment] rootNode
  let rootChildren' = filter (/= child) rootChildren
  pure $ Directory rootPath rootChildren'
deleteNode (segment:xs) rootNode@(Directory rootPath rootChildren) = do
  needle <- lookupNode [segment] rootNode
  rootChildren' <-
    mapM
      (\child ->
         case child of
           File _ _ -> pure child
           _ ->
             if child == needle
               then deleteNode xs child
               else pure child)
      rootChildren
  pure $ Directory rootPath rootChildren'

insertNode :: [FilePath] -> FSNode -> FSNode -> Maybe FSNode
insertNode _ _ (File _ _) = Nothing
insertNode [] _ _ = Nothing
insertNode [segment] newNode (Directory rootPath rootChildren) =
  if newNode ^. fsPath == segment
    then let rootChildren' =
               filter
                 (\child -> child ^. fsPath /= newNode ^. fsPath)
                 rootChildren
          in pure $ Directory rootPath (newNode : rootChildren')
    else Nothing
insertNode (segment:xs) newNode rootNode@(Directory rootPath rootChildren) = do
  needle <- lookupNode [segment] rootNode
  rootChildren' <-
    mapM
      (\child ->
         case child of
           File _ _ -> pure child
           _ ->
             if child == needle
               then insertNode xs newNode child
               else pure child)
      rootChildren
  pure $ Directory rootPath rootChildren'

type instance Index FSNode = FilePath

type instance IxValue FSNode = FSNode

instance Ixed FSNode where
  ix ::
       (Applicative f) => FilePath -> (FSNode -> f FSNode) -> FSNode -> f FSNode
  ix path f root =
    case lookupNode pathSegments root of
      Just node ->
        f node <&> \newNode -> fromJust $ insertNode pathSegments newNode root
      Nothing -> pure root
    where
      pathSegments :: [FilePath]
      pathSegments = splitDirectories path

instance At FSNode where
  at :: FilePath -> Lens' FSNode (Maybe FSNode)
  at path =
    let setter :: FSNode -> Maybe FSNode -> FSNode
        setter root =
          fromMaybe root . \case
            Just newNode -> insertNode pathSegments newNode root
            Nothing -> deleteNode pathSegments root
     in lens (lookupNode pathSegments) setter
    where
      pathSegments :: [FilePath]
      pathSegments = splitDirectories path

absifyPath :: FilePath -> FilePath -> FilePath
absifyPath relativePath currentDirectory =
  let absolutePath =
        case relativePath of
          '/':_ -> relativePath
          _ -> "/" </> currentDirectory </> relativePath
      normalizedSegments =
        concatMap
          (\case
             [_, ".."] -> []
             ["."] -> []
             xs -> xs) $
        chunksOf 2 $
        dropLeadingSlash $
        splitDirectories absolutePath
   in case joinPath normalizedSegments of
        "" -> "/"
        path -> path
  where
    dropLeadingSlash ("/":xs) = xs
    dropLeadingSlash xs = xs

absify ::
     (Member (Effs.State FileSystemState) effs) => FilePath -> Eff effs FilePath
absify relativePath =
  absifyPath relativePath <$> gets @FileSystemState (^. fsCurrentDirectory)

runFileSystem ::
     forall effs a. (Member (Effs.Error String) effs)
  => FileSystemState
  -> Eff (Effs.FileSystem ': effs) a
  -> Eff effs (a, FileSystemState)
runFileSystem origState = runState origState . reinterpret go
  where
    go :: FileSystem ~> Eff (Effs.State FileSystemState ': effs)
    go (CopyFile src dest) = do
      contents <- go $ ReadFile src
      go $ WriteFile contents dest
    go (CreateDirectoryIfMissing createParents relativePath) = do
      path <- absify relativePath
      parentNode <- gets @FileSystemState (^. fsRoot . at (takeDirectory path))
      case parentNode of
        Just _ ->
          modify @FileSystemState $ fsRoot . at path ?~
          Directory (takeFileName path) []
        Nothing ->
          if createParents
            then modify @FileSystemState $ fsRoot %~ \origRoot ->
                   fst
                     (foldl
                        (\(root, segments) pathSegment ->
                           let newSegments = segments <> [pathSegment]
                               newRoot =
                                 case root ^. at (joinPath newSegments) of
                                   Just _ -> root
                                   Nothing ->
                                     root & at (joinPath newSegments) ?~
                                     Directory pathSegment []
                            in (newRoot, newSegments))
                        (origRoot, [])
                        (splitDirectories path))
            else throwInterpretError
                   @FileSystemState
                   "createDirectoryIfMissing"
                   ("Missing parents for directory: " <> path)
    go (DoesDirectoryExist relativePath) = do
      path <- absify relativePath
      directoryNode <- gets @FileSystemState (^. fsRoot . at path)
      pure $
        case directoryNode of
          Just (Directory _ _) -> True
          _ -> False
    go GetCurrentDirectory = gets @FileSystemState (^. fsCurrentDirectory)
    go (GetDirectoryContents relativePath) = do
      path <- absify relativePath
      directoryNode <- gets @FileSystemState (^. fsRoot . at path)
      case directoryNode of
        Just (Directory _ children) -> pure $ map (^. fsPath) children
        _ ->
          throwInterpretError
            @FileSystemState
            "getDirectoryContents"
            ("No such directory: " <> path)
    go GetTemporaryDirectory = do
      tempCounter <- gets @FileSystemState (^. fsTempCounter)
      modify @FileSystemState $ fsTempCounter %~ (+ 1)
      let tempDirName = "/tmp" </> show tempCounter
      go $ CreateDirectoryIfMissing True tempDirName
      pure tempDirName
    go (ReadFile relativePath) = do
      path <- absify relativePath
      fileNode <- gets @FileSystemState (^. fsRoot . at path)
      case fileNode of
        Just (File _ contents) -> pure contents
        _ ->
          throwInterpretError
            @FileSystemState
            "readFile"
            ("No such file: " <> path)
    go (RemoveFile relativePath) = do
      path <- absify relativePath
      gets @FileSystemState (deleteNode (splitDirectories path) . (^. fsRoot)) >>= \case
        Just newRoot -> modify @FileSystemState $ fsRoot .~ newRoot
        Nothing ->
          throwInterpretError
            @FileSystemState
            "removeFile"
            ("No such file: " <> path)
    go (SetCurrentDirectory relativePath) = do
      path <- absify relativePath
      modify @FileSystemState $ fsCurrentDirectory .~ path
    go (WriteFile relativePath contents) = do
      path <- absify relativePath
      modify @FileSystemState $ fsRoot . at path ?~
        File (takeFileName path) contents
