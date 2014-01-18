module Web.Skype.API.Carbon where

#include "CoreFoundation/CoreFoundation.h"

import Data.String
import Foreign hiding (unsafePerformIO)
import Foreign.C.String
import Foreign.C.Types
import System.IO.Unsafe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

-- * CFBase

type CFIndex = Int32




-- * CFAllocator

data CFAllocator

type CFAllocatorRef = Ptr CFAllocator

defaultAllocator :: CFAllocatorRef
defaultAllocator = unsafePerformIO $ peek ptr_CFAllocatorDefault

foreign import ccall unsafe "&kCFAllocatorDefault"
  ptr_CFAllocatorDefault :: Ptr CFAllocatorRef

foreign import ccall unsafe "&CFRelease"
  finalizerCFRelease :: FinalizerPtr a

foreign import ccall unsafe "CFShow"
  c_CFShow :: Ptr a -> IO ()




-- * CFDictionary

data CFDictionary k v

type CFDictionaryRef k v = Ptr (CFDictionary k v)

newtype CFDictionaryKeyCallBacks =
  CFDictionaryKeyCallBacks (Ptr CFDictionaryKeyCallBacks)

newtype CFDictionaryValueCallBacks =
  CFDictionaryValueCallBacks (Ptr CFDictionaryValueCallBacks)

newCFDictionary :: [(Ptr k, Ptr v)] -> IO (CFDictionaryRef k v)
newCFDictionary elements =
  withArrayLen ks $ \len p_ks ->
  withArray    vs $ \p_vs ->
  c_CFDictionaryCreate defaultAllocator
                       p_ks
                       p_vs
                       (fromIntegral len)
                       c_CFTypeDictionaryKeyCallBacks
                       c_CFTypeDictionaryValueCallBacks
  where (ks, vs) = unzip elements

foreign import ccall unsafe "&kCFTypeDictionaryKeyCallBacks"
  c_CFTypeDictionaryKeyCallBacks :: CFDictionaryKeyCallBacks

foreign import ccall unsafe "&kCFTypeDictionaryValueCallBacks"
  c_CFTypeDictionaryValueCallBacks :: CFDictionaryValueCallBacks

foreign import ccall unsafe "CFDictionaryCreate"
  c_CFDictionaryCreate :: CFAllocatorRef
                       -> Ptr (Ptr k)
                       -> Ptr (Ptr v)
                       -> CFIndex
                       -> CFDictionaryKeyCallBacks
                       -> CFDictionaryValueCallBacks
                       -> IO (CFDictionaryRef k v)

foreign import ccall unsafe "CFDictionaryGetValue"
  c_CFDictionaryGetValue :: CFDictionaryRef k v
                         -> Ptr k
                         -> IO (Ptr v)




-- * CFString

data CFString

type CFStringRef = Ptr CFString

instance IsString CFStringRef where
  fromString = unsafeMakeConstantCFString

newtype CFStringEncoding = CFStringEncoding CInt
  deriving (Eq, Show)

#{enum CFStringEncoding, CFStringEncoding
     , macRoman      = kCFStringEncodingMacRoman
     , windowsLatin1 = kCFStringEncodingWindowsLatin1
     , isoLatin1     = kCFStringEncodingISOLatin1
     , nextStepLatin = kCFStringEncodingNextStepLatin
     , ascii         = kCFStringEncodingASCII
     , unicode       = kCFStringEncodingUnicode
     , utf8          = kCFStringEncodingUTF8
     , nonLossyASCII = kCFStringEncodingNonLossyASCII

     , utf16         = kCFStringEncodingUTF16
     , utf16BE       = kCFStringEncodingUTF16BE
     , utf16LE       = kCFStringEncodingUTF16LE

     , utf32         = kCFStringEncodingUTF32
     , utf32BE       = kCFStringEncodingUTF32BE
     , utf32LE       = kCFStringEncodingUTF32LE
 }

makeConstantCFString :: String -> IO CFStringRef
makeConstantCFString str = withCAString str $ c_CFStringMakeConstantString

unsafeMakeConstantCFString :: String -> CFStringRef
unsafeMakeConstantCFString = unsafeDupablePerformIO . makeConstantCFString

newCFString :: BS.ByteString -> IO (ForeignPtr CFString)
newCFString bs = BS.unsafeUseAsCStringLen bs $ \(ptr_char, len) -> do
  ptr <- c_CFStringCreateWithBytes defaultAllocator
                                   (castPtr ptr_char)
                                   (fromIntegral len)
                                   utf8
                                   False
  newForeignPtr finalizerCFRelease ptr

foreign import ccall unsafe "__CFStringMakeConstantString"
  c_CFStringMakeConstantString :: CString -> IO CFStringRef

foreign import ccall unsafe "CFStringCreateWithBytes"
  c_CFStringCreateWithBytes :: CFAllocatorRef  -- alloc
                            -> Ptr Word8  -- bytes
                            -> CFIndex  -- numBytes
                            -> CFStringEncoding  -- encoding
                            -> Bool  -- isExternalRepresentation
                            -> IO CFStringRef




-- * CFNotificationCenter

data CFNotificationCenter

type CFNotificationCenterRef = Ptr CFNotificationCenter

type CFNotificationCallback observer object =
      forall a. CFNotificationCenterRef
  -> Ptr observer
  -> CFStringRef  -- name
  -> Ptr object
  -> (CFDictionaryRef CFStringRef a) -- userInfo
  -> IO ()

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
                                    -> (FunPtr (CFNotificationCallback observer object))
                                    -> CFStringRef
                                    -> Ptr object
                                    -> CFNotificationSuspensionBehavior
                                    -> IO ()
