{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Axel.Test.Eff.FileSystemMock where

import Axel.Prelude

import Axel.Eff.FileSystem as Effs
import Axel.Utils.FilePath

import Control.Lens hiding (children)

import Data.List.Split
import Data.Maybe
import qualified Data.Text as T

import qualified Polysemy as Sem
import qualified Polysemy.Error as Sem
import qualified Polysemy.State as Sem

import TestUtils

data FSNode
  = Directory FilePath [FSNode]
  | File FilePath Text
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

data FileSystemState =
  FileSystemState
    { _fsCurrentDirectory :: FilePath
    , _fsRoot :: FSNode
    , _fsTempCounter :: Int
    }
  deriving (Eq, Show)

makeFieldsNoPrefix ''FileSystemState

mkFileSystemState :: [FSNode] -> FileSystemState
mkFileSystemState rootContents =
  FileSystemState
    { _fsCurrentDirectory = FilePath "/"
    , _fsRoot = Directory (FilePath "/") rootContents
    , _fsTempCounter = 0
    }

lookupNode :: [FilePath] -> FSNode -> Maybe FSNode
lookupNode _ (File _ _) = Nothing
lookupNode [] _ = Nothing
lookupNode [FilePath "."] rootNode = pure rootNode
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
  ix :: FilePath -> Traversal' FSNode FSNode
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
        case T.unpack $ op FilePath relativePath of
          '/':_ -> relativePath
          _ -> FilePath "/" </> currentDirectory </> relativePath
      normalizedSegments =
        concatMap
          (\case
             [_, FilePath ".."] -> []
             [FilePath "."] -> []
             xs -> xs) $
        chunksOf 2 $
        dropLeadingSlash $
        splitDirectories absolutePath
   in case joinPath normalizedSegments of
        FilePath "" -> FilePath "/"
        path -> path
  where
    dropLeadingSlash (FilePath "/":xs) = xs
    dropLeadingSlash x = x

absify ::
     (Sem.Member (Sem.State FileSystemState) effs)
  => FilePath
  -> Sem.Sem effs FilePath
absify relativePath =
  absifyPath relativePath <$> Sem.gets (^. fsCurrentDirectory)

runFileSystem ::
     forall effs a. (Sem.Member (Sem.Error Text) effs)
  => FileSystemState
  -> Sem.Sem (Effs.FileSystem ': effs) a
  -> Sem.Sem effs (FileSystemState, a)
runFileSystem origState = Sem.runState origState . Sem.reinterpret go
  where
    go :: FileSystem m a' -> Sem.Sem (Sem.State FileSystemState ': effs) a'
    go (AppendFile relativePath addedContents) = do
      oldContents <- go $ ReadFile relativePath
      let newContents = oldContents <> addedContents
      go $ WriteFile relativePath newContents
    go (CopyFile src dest) = do
      contents <- go $ ReadFile src
      go $ WriteFile dest contents
    go (CreateDirectoryIfMissing createParents relativePath) = do
      path <- absify relativePath
      parentNode <- Sem.gets (^. fsRoot . at (takeDirectory path))
      case parentNode of
        Just _ ->
          Sem.modify $ fsRoot . at path ?~ Directory (takeFileName path) []
        Nothing ->
          if createParents
            then Sem.modify $ fsRoot %~ \origRoot ->
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
                   ("Missing parents for directory: " <> op FilePath path)
    go (DoesDirectoryExist relativePath) = do
      path <- absify relativePath
      directoryNode <- Sem.gets (^. fsRoot . at path)
      pure $
        case directoryNode of
          Just (Directory _ _) -> True
          _ -> False
    go GetCurrentDirectory = Sem.gets (^. fsCurrentDirectory)
    go (GetDirectoryContents relativePath) = do
      path <- absify relativePath
      directoryNode <- Sem.gets (^. fsRoot . at path)
      case directoryNode of
        Just (Directory _ children) -> pure $ map (^. fsPath) children
        _ ->
          throwInterpretError
            @FileSystemState
            "getDirectoryContents"
            ("No such directory: " <> op FilePath path)
    go GetTemporaryDirectory = do
      tempCounter <- Sem.gets (^. fsTempCounter)
      Sem.modify $ fsTempCounter %~ (+ 1)
      let tempDirName = FilePath "/tmp" </> FilePath (showText tempCounter)
      go $ CreateDirectoryIfMissing True tempDirName
      pure tempDirName
    go (ReadFile relativePath) = do
      path <- absify relativePath
      fileNode <- Sem.gets (^. fsRoot . at path)
      case fileNode of
        Just (File _ contents) -> pure contents
        _ ->
          throwInterpretError
            @FileSystemState
            "readFile"
            ("No such file: " <> op FilePath path)
    go (RemoveFile relativePath) = do
      path <- absify relativePath
      Sem.gets (deleteNode (splitDirectories path) . (^. fsRoot)) >>= \case
        Just newRoot -> Sem.modify $ fsRoot .~ newRoot
        Nothing ->
          throwInterpretError
            @FileSystemState
            "removeFile"
            ("No such file: " <> op FilePath path)
    go (SetCurrentDirectory relativePath) = do
      path <- absify relativePath
      Sem.modify $ fsCurrentDirectory .~ path
    go (WriteFile relativePath contents) = do
      path <- absify relativePath
      Sem.modify $ fsRoot . at path ?~ File (takeFileName path) contents
