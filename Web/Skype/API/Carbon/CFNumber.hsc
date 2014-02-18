module Web.Skype.API.Carbon.CFNumber where

#include "CoreFoundation/CFNumber.h"

import Foreign
import Foreign.C.Types
import Web.Skype.API.Carbon.CFBase

data CFNumber

type CFNumberRef = Ptr CFNumber

class CFNumberFactory a where
  fromCFNumber :: CFNumberRef -> IO a

  newCFNumber :: a -> IO CFNumberRef

instance (Integral a) => CFNumberFactory a where
  fromCFNumber number = alloca $ \(ptr :: Ptr CInt) -> do
    c_CFNumberGetValue number intType ptr
    fromIntegral `fmap` peek ptr

  newCFNumber value = alloca $ \ptr -> do
    poke ptr (fromIntegral value :: CInt)
    c_CFNumberCreate defaultAllocator intType ptr

withCFNumber :: (CFNumberFactory a) => a -> (CFNumberRef -> IO b) -> IO b
withCFNumber source action = do
  number <- newCFNumber source
  result <- action number
  c_CFRelease number
  return result

newtype CFNumberType = CFNumberType CInt
  deriving (Eq, Show)

#{enum CFNumberType, CFNumberType
     , sInt8Type     = kCFNumberSInt8Type
     , sInt16Type    = kCFNumberSInt16Type
     , sInt32Type    = kCFNumberSInt32Type
     , sInt64Type    = kCFNumberSInt64Type
     , float32Type   = kCFNumberFloat32Type
     , float64Type   = kCFNumberFloat64Type
     , charType      = kCFNumberCharType
     , shortType     = kCFNumberShortType
     , intType       = kCFNumberIntType
     , longType      = kCFNumberLongType
     , longLongType  = kCFNumberLongLongType
     , floatType     = kCFNumberFloatType
     , doubleType    = kCFNumberDoubleType
     , cfIndexType   = kCFNumberCFIndexType
     , nsIntegerType = kCFNumberNSIntegerType
     , cgFloatType   = kCFNumberCGFloatType
     , maxType       = kCFNumberMaxType
}

foreign import ccall unsafe "CFNumberCreate"
  c_CFNumberCreate :: CFAllocatorRef  -- allocator
                   -> CFNumberType  -- theType
                   -> Ptr value  -- valuePtr
                   -> IO CFNumberRef

foreign import ccall unsafe "CFNumberGetValue"
  c_CFNumberGetValue :: CFNumberRef  -- number
                     -> CFNumberType  -- theType
                     -> Ptr value  -- valuePtr
                     -> IO Bool
