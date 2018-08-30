{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Axel.Test.Monad.FileSystemMock where

import Axel.Monad.Console as Console
import Axel.Monad.FileSystem as FS
import Axel.Monad.Process as Proc
import Axel.Monad.Resource as Res
import Axel.Test.MockUtils
import Axel.Utils.Debug

import Control.Lens hiding (children)
import Control.Monad.Except
import Control.Monad.State.Lazy

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

absify :: (MonadState FSState f) => FilePath -> f FilePath
absify relativePath = absifyPath relativePath <$> gets (^. fsCurrentDirectory)

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

instance Eq (FileSystemT m a) where
  _ == _ = False

instance Show (FileSystemT m a) where
  show _ = "<FileSystemT>"

instance MonadTrans FileSystemT where
  lift = FileSystemT . lift . lift

instance (Monad m) => MonadFileSystem (FileSystemT m) where
  copyFile src dest = do
    contents <- FS.readFile src
    FS.writeFile contents dest
  createDirectoryIfMissing createParents relativePath =
    FileSystemT $ do
      path <- absify relativePath
      parentNode <- gets (^. fsRoot . at (takeDirectory path))
      case parentNode of
        Just _ -> fsRoot . at path ?= Directory (takeFileName path) []
        Nothing ->
          if createParents
            then fsRoot %= \origRoot ->
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
    let tempDirName = "/tmp" </> show tempCounter
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
  writeFile relativePath contents =
    FileSystemT $ do
      path <- absify relativePath
      fsRoot . at path ?= File (takeFileName path) contents

runFileSystemT :: FSState -> FileSystemT m a -> ExceptT String m (a, FSState)
runFileSystemT origState (FileSystemT x) = runStateT x origState

runFileSystem :: FSState -> FileSystem a -> Either String (a, FSState)
runFileSystem origState x = runExcept $ runFileSystemT origState x
