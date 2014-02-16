{-# OPTIONS_GHC -cpp #-}

module Web.Skype.API (
  SkypeConnection,
  connect
) where

#if defined __MACOSX__
import Control.Monad.Trans (MonadIO)
import Control.Monad.Error.Class (MonadError)

import qualified Web.Skype.API.Carbon as Carbon

type SkypeConnection = Carbon.SkypeConnection

connect :: (Error e, MonadIO m, MonadError e m) => m SkypeConnection
connect = Carbon.connect
#else
import Control.Exception (IOException)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)

import qualified Web.Skype.API.X11 as X11

type SkypeConnection = X11.SkypeConnection

connect :: (MonadBaseControl IO m, MonadIO m, MonadError IOException m) => m SkypeConnection
connect = X11.connect
#endif
