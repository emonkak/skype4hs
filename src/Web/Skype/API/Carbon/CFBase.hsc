module Web.Skype.API.Carbon.CFBase where

#include "CoreFoundation/CFBase.h"

import Foreign hiding (unsafePerformIO)
import Foreign.C.Types
import System.IO.Unsafe

type CFIndex = Int32

data CFAllocator

type CFAllocatorRef = Ptr CFAllocator

defaultAllocator :: CFAllocatorRef
defaultAllocator = unsafePerformIO $ peek p_CFAllocatorDefault

foreign import ccall unsafe "&kCFAllocatorDefault"
  p_CFAllocatorDefault :: Ptr CFAllocatorRef

foreign import ccall unsafe "CFRelease"
  c_CFRelease :: Ptr a -> IO ()

foreign import ccall unsafe "&CFRelease"
  p_CFRelease :: FunPtr (Ptr a -> IO ())

newtype CFComparisonResult = CFComparisonResult CInt
  deriving (Eq, Show)

#{enum CFComparisonResult, CFComparisonResult
     , compareLessThan    = kCFCompareLessThan
     , compareEqualTo     = kCFCompareEqualTo
     , compareGreaterThan = kCFCompareGreaterThan
}
