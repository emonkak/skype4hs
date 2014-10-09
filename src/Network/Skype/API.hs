{-# OPTIONS_GHC -cpp #-}

module Network.Skype.API (
  Connection,
  connect
) where

import Control.Exception (IOException)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Network.Skype.Core

#if defined(darwin_HOST_OS)
import qualified Network.Skype.API.Carbon as API
#else
import qualified Network.Skype.API.X11 as API
#endif

type Connection = API.Connection

connect :: (MonadBaseControl IO m, MonadIO m, MonadError IOException m)
        => ApplicationName
        -> m Connection
connect = API.connect
