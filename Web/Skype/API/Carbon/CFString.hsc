module Web.Skype.API.Carbon.CFString where

import Data.String
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Web.Skype.API.Carbon.CFBase
import System.IO.Unsafe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

#include "CoreFoundation/CFString.h"

data CFString

type CFStringRef = Ptr CFString

instance IsString CFStringRef where
  fromString = unsafeMakeConstantString

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

makeConstantString :: String -> IO CFStringRef
makeConstantString str = withCAString str $ c_CFStringMakeConstantString

unsafeMakeConstantString :: String -> CFStringRef
unsafeMakeConstantString = unsafeDupablePerformIO . makeConstantString

newCFString :: BS.ByteString -> IO CFStringRef
newCFString bytes =
  BS.unsafeUseAsCStringLen bytes $ \(ptr_char, len) -> do
    c_CFStringCreateWithBytes defaultAllocator
                              (castPtr ptr_char)
                              (fromIntegral len)
                              utf8
                              False

withCFString :: BS.ByteString -> (CFStringRef -> IO a) -> IO a
withCFString bytes action = do
  string <- newCFString bytes
  result <- action string
  c_CFRelease string
  return result

foreign import ccall unsafe "CFShow"
  c_CFShow :: Ptr a -> IO ()

foreign import ccall unsafe "__CFStringMakeConstantString"
  c_CFStringMakeConstantString :: CString -> IO CFStringRef

foreign import ccall unsafe "CFStringCreateWithBytes"
  c_CFStringCreateWithBytes :: CFAllocatorRef  -- alloc
                            -> Ptr Word8  -- bytes
                            -> CFIndex  -- numBytes
                            -> CFStringEncoding  -- encoding
                            -> Bool  -- isExternalRepresentation
                            -> IO CFStringRef
