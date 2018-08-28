{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Monad.FileSystemMock where

import Axel.Monad.Console as Console
import Axel.Monad.FileSystem as FS
import Axel.Monad.Process as Proc
import Axel.Monad.Resource as Res

import Control.Lens hiding (children)
import Control.Monad.Except
import Control.Monad.State.Lazy

import Data.List.Split
import Data.Maybe

import MockUtils

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

data FSState = FSState
  { _fsCurrentDirectory :: FilePath
  , _fsRoot :: FSNode
  , _fsTempCounter :: Int
  } deriving (Eq, Show)

makeFieldsNoPrefix ''FSState

mkFSState :: [FSNode] -> FSState
mkFSState rootContents =
  FSState
    { _fsCurrentDirectory = "/"
    , _fsRoot = Directory "/" rootContents
    , _fsTempCounter = 0
    }

deleteNode :: [FilePath] -> FSNode -> Maybe FSNode
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

insertNode :: [FilePath] -> FSNode -> FSNode -> Maybe FSNode
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

lookupNode :: [FilePath] -> FSNode -> Maybe FSNode
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

type instance Index FSNode = FilePath

type instance IxValue FSNode = FSNode

instance Ixed FSNode where
  ix path f root =
    case lookupNode pathSegments root of
      Just node ->
        f node <&> \newNode -> fromJust $ insertNode pathSegments newNode root
      Nothing -> pure root
    where
      pathSegments :: [FilePath]
      pathSegments = splitDirectories path

instance At FSNode where
  at path f root =
    f node <&> \case
      Just newNode -> fromJust $ insertNode pathSegments newNode root
      Nothing ->
        maybe root (const (fromJust $ deleteNode pathSegments root)) node
    where
      pathSegments :: [FilePath]
      pathSegments = splitDirectories path
      node :: Maybe FSNode
      node = lookupNode pathSegments root

absify :: (MonadState FSState f) => FilePath -> f FilePath
absify relativePath = do
  currentDirectory <- gets (^. fsCurrentDirectory)
  let absolutePath =
        case relativePath of
          '/':_ -> relativePath
          _ -> currentDirectory </> relativePath
  let normalizedSegments =
        concatMap
          (\case
             [_, ".."] -> []
             ["."] -> []
             xs -> xs) $
        chunksOf 2 $
        splitPath absolutePath
  let absSegments =
        case normalizedSegments of
          "/":xs ->
            case xs of
              [] -> normalizedSegments
              _ -> xs
          _ -> normalizedSegments
  pure $ joinPath absSegments

newtype FileSystemT m a =
  FileSystemT (StateT FSState (ExceptT String m) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadConsole
           , MonadProcess
           , MonadResource
           )

type FileSystem = FileSystemT Identity

instance (Monad m) => MonadFileSystem (FileSystemT m) where
  copyFile src dest = do
    contents <- FS.readFile src
    FS.writeFile contents dest
  createDirectoryIfMissing createParents relativePath =
    FileSystemT $ do
      path <- (</> relativePath) <$> gets (^. fsCurrentDirectory)
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
            else throwInterpretError
                   "createDirectoryIfMissing"
                   ("Missing parents for directory: " <> path)
  doesDirectoryExist relativePath =
    FileSystemT $ do
      path <- absify relativePath
      directoryNode <- gets (^. fsRoot . at path)
      pure $
        case directoryNode of
          Just (Directory _ _) -> True
          _ -> False
  getCurrentDirectory = FileSystemT $ gets (^. fsCurrentDirectory)
  getDirectoryContents relativePath =
    FileSystemT $ do
      path <- absify relativePath
      directoryNode <- gets (^. fsRoot . at path)
      case directoryNode of
        Just (Directory _ children) -> pure $ map (^. fsPath) children
        _ ->
          throwInterpretError
            "getDirectoryContents"
            ("No such directory: " <> path)
  getTemporaryDirectory = do
    tempCounter <- FileSystemT $ fsTempCounter <<+= 1
    let tempDirName = "tmp" </> show tempCounter
    FS.createDirectoryIfMissing True tempDirName
    pure tempDirName
  readFile relativePath =
    FileSystemT $ do
      path <- absify relativePath
      fileNode <- gets (^. fsRoot . at path)
      case fileNode of
        Just (File _ contents) -> pure contents
        _ -> throwInterpretError "readFile" ("No such file: " <> path)
  removeFile relativePath =
    FileSystemT $ do
      path <- absify relativePath
      gets (deleteNode (splitDirectories path) . (^. fsRoot)) >>= \case
        Just newRoot -> fsRoot .= newRoot
        Nothing -> throwInterpretError "removeFile" ("No such file: " <> path)
  setCurrentDirectory relativePath =
    FileSystemT $ do
      path <- absify relativePath
      fsCurrentDirectory .= path
  writeFile contents relativePath =
    FileSystemT $ do
      path <- absify relativePath
      fsRoot . at path ?= File (takeFileName path) contents

runFileSystemT :: FSState -> FileSystemT m a -> ExceptT String m (a, FSState)
runFileSystemT origState (FileSystemT x) = runStateT x origState

runFileSystem :: FSState -> FileSystem a -> Either String (a, FSState)
runFileSystem origState x = runExcept $ runFileSystemT origState x
