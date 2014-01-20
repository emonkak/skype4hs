module Web.Skype.API.Carbon where

#include "CoreFoundation/CoreFoundation.h"

import Control.Applicative
import Control.Monad.Reader
import Data.IORef
import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.Environment
import Web.Skype.API.Carbon.CFBase
import Web.Skype.API.Carbon.CFDictionary
import Web.Skype.API.Carbon.CFNotificationCenter
import Web.Skype.API.Carbon.CFString

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

data SkypeConnection = forall observer object key value. SkypeConnection
  { skypeNotificationCenter :: CFNotificationCenterRef
  , skypeNotificationCallbacks :: IORef [CFNotificationCallbackRef]
  }

initialize = SkypeConnection <$> c_CFNotificationCenterGetDistributedCenter
                             <*> newIORef []

connect = do
  connection <- initialize

  (flip runReaderT) connection $ do
    register "SKSkypeAPINotification" onReceivedNotification
    register "SKSkypeWillQuit"        onQuitting
    register "SKSkypeBecameAvailable" onBecomeAvailable
    register "SKAvailabilityUpdate"   onUpdateAvailability
    register "SKSkypeAttachResponse"  onReceivedAttachResponse

    attach

attach = do
  center <- asks skypeNotificationCenter

  appName <- liftIO $ getProgName

  liftIO $ withCFString "test" $ \appName_ptr -> do
    c_CFShow appName_ptr

    c_CFNotificationCenterPostNotification center
                                           "SKSkypeAPIAttachRequest"
                                           nullPtr
                                           nullPtr
                                           True


register name callback = do
  center <- asks skypeNotificationCenter
  wrappedCallback <- liftIO $ wrapCFNotificationCallback callback

  liftIO $ c_CFNotificationCenterAddObserver center
                                             nullPtr
                                             wrappedCallback
                                             name
                                             nullPtr
                                             deliverImmediately

  callbacks <- asks skypeNotificationCallbacks

  liftIO $ modifyIORef callbacks (CFNotificationCallbackRef wrappedCallback :)

onReceivedNotification :: CFNotificationCallback observer object CFString value
onReceivedNotification center observer name object userInfo = print userInfo

onQuitting :: CFNotificationCallback observer object CFString value
onQuitting center observer name object userInfo = print userInfo

onBecomeAvailable :: CFNotificationCallback observer object CFString value
onBecomeAvailable center observer name object userInfo = c_CFShow userInfo

onUpdateAvailability :: CFNotificationCallback observer object CFString value
onUpdateAvailability center observer name object userInfo = c_CFShow userInfo

onReceivedAttachResponse :: CFNotificationCallback observer object CFString value
onReceivedAttachResponse center observer name object userInfo = c_CFShow userInfo
