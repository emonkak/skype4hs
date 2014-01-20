module Web.Skype.API.Carbon.CFBase where

#include "CoreFoundation/CFBase.h"

import Foreign hiding (unsafePerformIO)
import System.IO.Unsafe

type CFIndex = Int32

data CFAllocator

type CFAllocatorRef = Ptr CFAllocator

defaultAllocator :: CFAllocatorRef
defaultAllocator = unsafePerformIO $ peek ptr_CFAllocatorDefault

foreign import ccall unsafe "&kCFAllocatorDefault"
  ptr_CFAllocatorDefault :: Ptr CFAllocatorRef

foreign import ccall unsafe "CFRelease"
  c_CFRelease :: Ptr a -> IO ()
