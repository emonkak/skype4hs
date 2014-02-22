module Web.Skype.API.Carbon.CFString where

#include "CoreFoundation/CFString.h"

import Data.String
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Web.Skype.API.Carbon.CFBase
import System.IO.Unsafe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BS

data CFString

type CFStringRef = Ptr CFString

class CFStringFactory a where
  newCFString :: a -> IO CFStringRef

  fromCFString :: CFStringRef -> IO a

instance CFStringFactory CString where
  newCFString cs = c_CFStringCreateWithCString defaultAllocator
                                               cs
                                               encodingUTF8

  fromCFString source = do
    len <- c_CFStringGetLength source
    let bufferSize = c_CFStringGetMaximumSizeForEncoding len encodingUTF8 + 1
    bufferPtr <- mallocArray $ fromIntegral bufferSize
    c_CFStringGetCString source bufferPtr bufferSize encodingUTF8
    return bufferPtr

instance CFStringFactory String where
  newCFString str = withCStringLen str $ \(ptr, len) -> do
    c_CFStringCreateWithBytes defaultAllocator
                              (castPtr ptr)
                              (fromIntegral len)
                              encodingUTF8
                              False

  fromCFString source = do
    cs <- fromCFString source
    result <- peekCString cs
    free cs
    return result

instance CFStringFactory BS.ByteString where
  newCFString bs = BS.unsafeUseAsCStringLen bs $ \(ptr, len) -> do
    c_CFStringCreateWithBytes defaultAllocator
                              (castPtr ptr)
                              (fromIntegral len)
                              encodingUTF8
                              False

  fromCFString source = fromCFString source >>= BS.unsafePackCString

instance CFStringFactory BL.ByteString where
  newCFString bs = newCFString $ BL.toStrict bs

  fromCFString source = BL.fromStrict `fmap` fromCFString source

instance IsString CFStringRef where
  fromString = unsafeMakeConstantString

newtype CFStringEncoding = CFStringEncoding CInt
  deriving (Eq, Show)

#{enum CFStringEncoding, CFStringEncoding
     , encodingMacRoman      = kCFStringEncodingMacRoman
     , encodingWindowsLatin1 = kCFStringEncodingWindowsLatin1
     , encodingISOLatin1     = kCFStringEncodingISOLatin1
     , encodingNextStepLatin = kCFStringEncodingNextStepLatin
     , encodingASCII         = kCFStringEncodingASCII
     , encodingUnicode       = kCFStringEncodingUnicode
     , encodingUTF8          = kCFStringEncodingUTF8
     , encodingNonLossyASCII = kCFStringEncodingNonLossyASCII

     , encodingUTF16         = kCFStringEncodingUTF16
     , encodingUTF16BE       = kCFStringEncodingUTF16BE
     , encodingUTF16LE       = kCFStringEncodingUTF16LE

     , encodingUTF32         = kCFStringEncodingUTF32
     , encodingUTF32BE       = kCFStringEncodingUTF32BE
     , encodingUTF32LE       = kCFStringEncodingUTF32LE
}

makeConstantString :: String -> IO CFStringRef
makeConstantString str = withCAString str $ c_CFStringMakeConstantString

unsafeMakeConstantString :: String -> CFStringRef
unsafeMakeConstantString = unsafeDupablePerformIO . makeConstantString

withCFString :: (CFStringFactory a) => a -> (CFStringRef -> IO b) -> IO b
withCFString source action = do
  string <- newCFString source
  result <- action string
  c_CFRelease string
  return result

foreign import ccall unsafe "__CFStringMakeConstantString"
  c_CFStringMakeConstantString :: CString -> IO CFStringRef

foreign import ccall unsafe "CFStringCreateWithBytes"
  c_CFStringCreateWithBytes :: CFAllocatorRef  -- alloc
                            -> Ptr Word8  -- bytes
                            -> CFIndex  -- numBytes
                            -> CFStringEncoding  -- encoding
                            -> Bool  -- isExternalRepresentation
                            -> IO CFStringRef

foreign import ccall unsafe "CFStringCreateWithCString"
  c_CFStringCreateWithCString :: CFAllocatorRef  -- alloc
                              -> CString  -- cStr
                              -> CFStringEncoding  -- encoding
                              -> IO CFStringRef

foreign import ccall unsafe "CFStringGetCString"
  c_CFStringGetCString :: CFStringRef  -- theString
                       -> CString  -- buffer
                       -> CFIndex  -- bufferSize
                       -> CFStringEncoding  -- encoding
                       -> IO Bool

foreign import ccall unsafe "CFStringGetLength"
  c_CFStringGetLength :: CFStringRef -> IO CFIndex

foreign import ccall unsafe "CFStringGetMaximumSizeForEncoding"
  c_CFStringGetMaximumSizeForEncoding :: CFIndex  -- length
                                      -> CFStringEncoding  -- encoding
                                      -> CFIndex

newtype CFStringCompareFlags = CFStringCompareFlags CInt
  deriving (Bits, Eq, Show)

#{enum CFStringCompareFlags, CFStringCompareFlags
     , compareDefault              = 0
     , compareCaseInsensitive      = kCFCompareCaseInsensitive
     , compareBackwards            = kCFCompareBackwards
     , compareAnchored             = kCFCompareAnchored
     , compareNonliteral           = kCFCompareNonliteral
     , compareLocalized            = kCFCompareLocalized
     , compareNumerically          = kCFCompareNumerically
     , compareDiacriticInsensitive = kCFCompareDiacriticInsensitive
     , compareWidthInsensitive     = kCFCompareWidthInsensitive
     , compareForcedOrdering       = kCFCompareForcedOrdering
}

foreign import ccall unsafe "CFStringCompare"
  c_CFStringCompare :: CFStringRef  -- theString1
                    -> CFStringRef  -- theString2
                    -> CFStringCompareFlags  -- compareOptions
                    -> IO CFComparisonResult
