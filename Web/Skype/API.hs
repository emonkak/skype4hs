{-# OPTIONS_GHC -cpp #-}

module Web.Skype.API (
  SkypeConnection,
  connect
) where

import Web.Skype.Core

#if defined __APPLE__
import qualified Web.Skype.API.Carbon as API
#else
import qualified Web.Skype.API.X11 as API
#endif

type SkypeConnection = API.SkypeConnection

connect :: ApplicationName -> IO API.SkypeConnection
connect = API.connect
