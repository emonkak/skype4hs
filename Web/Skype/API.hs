{-# OPTIONS_GHC -cpp #-}

module Web.Skype.API (
  SkypeConnection,
  connect
) where

#if defined __MACOSX__
import qualified Web.Skype.API.Carbon as Carbon

type SkypeConnection = Carbon.SkypeConnection

connect = Carbon.connect
#else
import qualified Web.Skype.API.X11 as X11

type SkypeConnection = X11.SkypeConnection

connect = X11.connect
#endif
