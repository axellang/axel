{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Monad.FileSystem where

import Axel.Monad.FileSystem as FS

import Control.Lens hiding (children)
import Control.Monad.Except
import Control.Monad.Free
import Control.Monad.State.Lazy

import Data.Maybe

import System.FilePath

data MockFSNode
  = Directory FilePath
              [MockFSNode]
  | File FilePath
         String
  deriving (Eq, Show)

fsPath :: Lens' MockFSNode FilePath
fsPath =
  lens
    (\case
       Directory path _ -> path
       File path _ -> path)
    (\node newPath ->
       case node of
         Directory _ children -> Directory newPath children
         File _ contents -> File newPath contents)

data MockFSState = MockFSState
  { _fsCurrentDirectory :: FilePath
  , _fsRoot :: MockFSNode
  , _fsTempCounter :: Int
  }

makeFieldsNoPrefix ''MockFSState

deleteNode :: [FilePath] -> MockFSNode -> Maybe MockFSNode
deleteNode pathSegments root@(Directory rootPath children) =
  case pathSegments of
    [] -> Nothing
    [nodeName] -> do
      nodeToDelete <- lookupNode [nodeName] root
      let newChildren = filter (/= nodeToDelete) children
      pure $ Directory rootPath newChildren
    (nodeName:xs) -> do
      newChildren <-
        mapM
          (\child ->
             case child of
               Directory path _ ->
                 if path == nodeName
                   then deleteNode xs child
                   else pure child
               File _ _ -> pure child)
          children
      pure $ Directory rootPath newChildren
deleteNode _ (File _ _) = Nothing

insertNode :: [FilePath] -> MockFSNode -> MockFSNode -> Maybe MockFSNode
insertNode pathSegments newNode (Directory rootPath children) =
  case pathSegments of
    [] -> pure $ Directory rootPath (newNode : children)
    nodeName:xs ->
      Directory rootPath <$>
      mapM
        (\child ->
           case child of
             Directory path _ ->
               if path == nodeName
                 then insertNode xs newNode child
                 else pure child
             File _ _ -> pure child)
        children
insertNode _ _ (File _ _) = Nothing

lookupNode :: [FilePath] -> MockFSNode -> Maybe MockFSNode
lookupNode pathSegments (Directory _ children) =
  case pathSegments of
    [] -> Nothing
    nodeName:xs -> do
      childNode <-
        listToMaybe $
        filter
          (\case
             Directory path _ -> path == nodeName
             File path _ -> path == nodeName)
          children
      case xs of
        [] -> pure childNode
        _ -> lookupNode xs childNode
lookupNode _ (File _ _) = Nothing

type instance Index MockFSNode = FilePath

type instance IxValue MockFSNode = MockFSNode

instance Ixed MockFSNode where
  ix path f root =
    case lookupNode pathSegments root of
      Just node ->
        f node <&> \newNode -> fromJust $ insertNode pathSegments newNode root
      Nothing -> pure root
    where
      pathSegments :: [FilePath]
      pathSegments = splitDirectories path

instance At MockFSNode where
  at path f root =
    f node <&> \case
      Just newNode -> fromJust $ insertNode pathSegments newNode root
      Nothing ->
        maybe root (const (fromJust $ deleteNode pathSegments root)) node
    where
      pathSegments :: [FilePath]
      pathSegments = splitDirectories path
      node :: Maybe MockFSNode
      node = lookupNode pathSegments root

interpretMockFS ::
     (MonadError () m, MonadState MockFSState m)
  => FileSystemAction (m next)
  -> m next
interpretMockFS (CopyFile src dest next) = do
  origState <- get
  (_, newState) <-
    flip runMockFS origState $ do
      contents <- FS.readFile src :: Free FileSystemAction FilePath
      FS.writeFile contents dest :: Free FileSystemAction ()
  put newState
  next
interpretMockFS (CreateDirectoryIfMissing createParents path next) = do
  parentNode <- gets (^. fsRoot . at (takeDirectory path))
  case parentNode of
    Just _ -> fsRoot . at path ?= Directory (takeFileName path) []
    Nothing ->
      if createParents
        then fsRoot %= \origRoot ->
               fst
                 (foldl
                    (\(root, segments) pathSegment ->
                       let newRoot =
                             case root ^. at pathSegment of
                               Just _ -> root
                               Nothing ->
                                 root & at pathSegment ?~
                                 Directory pathSegment []
                        in (newRoot, segments <> [pathSegment]))
                    (origRoot, [])
                    (splitDirectories path))
        else throwError ()
  next
interpretMockFS (DoesDirectoryExist path next) = do
  directoryNode <- gets (^. fsRoot . at path)
  next $
    case directoryNode of
      Just (Directory _ _) -> True
      _ -> False
interpretMockFS (GetCurrentDirectory next) = do
  currentDirectory <- gets (^. fsCurrentDirectory)
  next currentDirectory
interpretMockFS (GetDirectoryContents path next) = do
  directoryNode <- gets (^. fsRoot . at path)
  case directoryNode of
    Just (Directory _ children) -> next $ map (^. fsPath) children
    _ -> throwError ()
interpretMockFS (GetTemporaryDirectory next) = do
  tempCounter <- fsTempCounter <<+= 1
  let tempDirName = "tmp" </> show tempCounter
  origState <- get
  (_, newState) <-
    flip runMockFS origState $ FS.createDirectoryIfMissing True tempDirName
  put newState
  next tempDirName
interpretMockFS (ReadFile path next) = do
  fileNode <- gets (^. fsRoot . at path)
  case fileNode of
    Just (File _ contents) -> next contents
    _ -> throwError ()
interpretMockFS (RemoveFile path next) = do
  gets (deleteNode (splitDirectories path) . (^. fsRoot)) >>= \case
    Just newRoot -> fsRoot .= newRoot
    Nothing -> throwError ()
  next
interpretMockFS (SetCurrentDirectory path next) = do
  fsCurrentDirectory .= path
  next
interpretMockFS (WriteFile contents path next) = do
  fsRoot . at path ?= File (takeFileName path) contents
  next

runMockFS ::
     (MonadError () m) => FileSystem a -> MockFSState -> m (a, MockFSState)
runMockFS program = runStateT (iterM interpretMockFS program)
