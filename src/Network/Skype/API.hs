{-# OPTIONS_GHC -cpp #-}

module Network.Skype.API (
  Connection,
  connect
) where

import Network.Skype.Core

#if defined(darwin_HOST_OS)
import qualified Network.Skype.API.Carbon as API
#else
import qualified Network.Skype.API.X11 as API
#endif

type Connection = API.Connection

connect :: ApplicationName -> IO API.Connection
connect = API.connect
