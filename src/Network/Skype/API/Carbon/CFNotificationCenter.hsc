module Network.Skype.API.Carbon.CFNotificationCenter where

#include "CoreFoundation/CFNotificationCenter.h"

import Data.IORef
import Foreign hiding (addForeignPtrFinalizer, newForeignPtr)
import Foreign.C.Types
import Foreign.Concurrent
import Network.Skype.API.Carbon.CFDictionary
import Network.Skype.API.Carbon.CFString

data NotificationCenter = forall callback observer. NotificationCenter
  { getNotificationCenter :: ForeignPtr CFNotificationCenter
  , getCallbacks :: IORef [FunPtr callback]
  , getObserver :: ForeignPtr observer
  }

withNotificationCenter :: NotificationCenter -> (CFNotificationCenterRef -> IO a) -> IO a
withNotificationCenter (NotificationCenter center _ _) action =
  withForeignPtr center action

getDistributedCenter :: ForeignPtr a -> IO NotificationCenter
getDistributedCenter observer = do
  callbacks <- newIORef []
  center_ptr <- c_CFNotificationCenterGetDistributedCenter
  center <- newForeignPtr center_ptr $ do
    withForeignPtr observer $ c_CFNotificationCenterRemoveEveryObserver center_ptr
    readIORef callbacks >>= mapM_ freeHaskellFunPtr
  return $ NotificationCenter center callbacks observer

addObserver :: NotificationCenter
            -> CFStringRef
            -> CFNotificationCallback observer object key value
            -> IO ()
addObserver (NotificationCenter center callbacks observer) name callback =
  withForeignPtr center $ \center_ptr ->
  withForeignPtr observer $ \observer_ptr -> do
    callback_ptr <- wrapCFNotificationCallback callback
    c_CFNotificationCenterAddObserver
      center_ptr
      (castPtr observer_ptr)
      callback_ptr
      name
      nullPtr
      suspensionBehaviorDeliverImmediately
    atomicModifyIORef' callbacks (\cs -> (castFunPtr callback_ptr : cs, ()))

data CFNotificationCenter

type CFNotificationCenterRef = Ptr CFNotificationCenter

type CFNotificationCallback observer object key value =
     CFNotificationCenterRef
  -> Ptr observer
  -> CFStringRef  -- name
  -> Ptr object
  -> CFDictionaryRef key value  -- userInfo
  -> IO ()

newtype CFNotificationSuspensionBehavior = CFNotificationSuspensionBehavior CInt
  deriving (Eq, Show)

#{enum CFNotificationSuspensionBehavior, CFNotificationSuspensionBehavior
     , suspensionBehaviorDrop               = CFNotificationSuspensionBehaviorDrop
     , suspensionBehaviorCoalesce           = CFNotificationSuspensionBehaviorCoalesce
     , suspensionBehaviorHold               = CFNotificationSuspensionBehaviorHold
     , suspensionBehaviorDeliverImmediately = CFNotificationSuspensionBehaviorDeliverImmediately
};

foreign import ccall unsafe "CFNotificationCenterGetDistributedCenter"
  c_CFNotificationCenterGetDistributedCenter :: IO CFNotificationCenterRef

foreign import ccall unsafe "CFNotificationCenterAddObserver"
  c_CFNotificationCenterAddObserver :: CFNotificationCenterRef
                                    -> Ptr observer
                                    -> FunPtr (CFNotificationCallback observer object key value)
                                    -> CFStringRef
                                    -> Ptr object
                                    -> CFNotificationSuspensionBehavior
                                    -> IO ()

foreign import ccall unsafe "CFNotificationCenterRemoveObserver"
  c_CFNotificationCenterRemoveObserver :: CFNotificationCenterRef
                                       -> Ptr observer
                                       -> CFStringRef
                                       -> Ptr object
                                       -> IO ()

foreign import ccall unsafe "CFNotificationCenterRemoveEveryObserver"
  c_CFNotificationCenterRemoveEveryObserver :: CFNotificationCenterRef
                                            -> Ptr observer
                                            -> IO ()

foreign import ccall unsafe "CFNotificationCenterPostNotification"
  c_CFNotificationCenterPostNotification :: CFNotificationCenterRef
                                         -> CFStringRef
                                         -> Ptr object
                                         -> CFDictionaryRef key value
                                         -> Bool  -- deliverImmediately
                                         -> IO ()

foreign import ccall "wrapper"
  wrapCFNotificationCallback :: CFNotificationCallback observer object key value
                             -> IO (FunPtr (CFNotificationCallback observer object key value))
