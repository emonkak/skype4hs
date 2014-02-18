{-# OPTIONS_GHC -cpp #-}

module Web.Skype.API (
  SkypeConnection,
  connect
) where

#if defined __APPLE__
import qualified Web.Skype.API.Carbon as API
#else
import qualified Web.Skype.API.X11 as API
#endif

import qualified Data.ByteString as BS

type SkypeConnection = API.SkypeConnection

connect :: BS.ByteString -> IO API.SkypeConnection
connect = API.connect
