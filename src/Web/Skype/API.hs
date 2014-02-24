{-# OPTIONS_GHC -cpp #-}

module Web.Skype.API (
  Connection,
  connect
) where

import Web.Skype.Core

#if defined(darwin_HOST_OS)
import qualified Web.Skype.API.Carbon as API
#else
import qualified Web.Skype.API.X11 as API
#endif

type Connection = API.Connection

connect :: ApplicationName -> IO API.Connection
connect = API.connect
