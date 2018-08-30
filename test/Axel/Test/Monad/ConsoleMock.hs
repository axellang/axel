{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Axel.Test.Monad.ConsoleMock where

import Axel.Monad.Console as Console
import Axel.Monad.FileSystem as FS
import Axel.Monad.Process as Proc
import Axel.Monad.Resource as Res

import Control.Lens
import Control.Monad.State.Lazy

newtype ConsoleState = ConsoleState
  { _consoleOutput :: String
  } deriving (Eq, Show)

makeFieldsNoPrefix ''ConsoleState

mkConsoleState :: ConsoleState
mkConsoleState = ConsoleState {_consoleOutput = ""}

newtype ConsoleT m a =
  ConsoleT (StateT ConsoleState m a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadTrans
           , MonadFileSystem
           , MonadProcess
           , MonadResource
           )

type Console = ConsoleT Identity

instance (Monad m) => MonadConsole (ConsoleT m) where
  putStr str = ConsoleT $ consoleOutput <>= str

runConsoleT :: ConsoleState -> ConsoleT m a -> m (a, ConsoleState)
runConsoleT origState (ConsoleT x) = runStateT x origState

runConsole :: ConsoleState -> Console a -> (a, ConsoleState)
runConsole origState x = runIdentity $ runConsoleT origState x
