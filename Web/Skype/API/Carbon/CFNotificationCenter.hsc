module Web.Skype.API.Carbon.CFNotificationCenter where

import Foreign
import Foreign.C.Types
import Web.Skype.API.Carbon.CFDictionary
import Web.Skype.API.Carbon.CFString

#include "CoreFoundation/CFNotificationCenter.h"

data CFNotificationCenter

type CFNotificationCenterRef = Ptr CFNotificationCenter

type CFNotificationCallback observer object key value =
     CFNotificationCenterRef
  -> Ptr observer
  -> CFStringRef  -- name
  -> Ptr object
  -> CFDictionaryRef key value  -- userInfo
  -> IO ()

data CFNotificationCallbackRef =
    forall observer object key value. CFNotificationCallbackRef
                                      (FunPtr (CFNotificationCallback observer object key value))

newtype CFNotificationSuspensionBehavior = CFNotificationSuspensionBehavior CInt
  deriving (Eq, Show)

#{enum CFNotificationSuspensionBehavior, CFNotificationSuspensionBehavior
     , drop               = CFNotificationSuspensionBehaviorDrop
     , coalesce           = CFNotificationSuspensionBehaviorCoalesce
     , hold               = CFNotificationSuspensionBehaviorHold
     , deliverImmediately = CFNotificationSuspensionBehaviorDeliverImmediately
};

foreign import ccall unsafe "CFNotificationCenterGetDistributedCenter"
  c_CFNotificationCenterGetDistributedCenter :: IO CFNotificationCenterRef

foreign import ccall unsafe "CFNotificationCenterAddObserver"
  c_CFNotificationCenterAddObserver :: CFNotificationCenterRef
                                    -> Ptr observer
                                    -> FunPtr (CFNotificationCallback observer object CFString value)
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
