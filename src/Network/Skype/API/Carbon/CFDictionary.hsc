module Network.Skype.API.Carbon.CFDictionary where

#include "CoreFoundation/CFDictionary.h"

import Foreign
import Network.Skype.API.Carbon.CFBase

data CFDictionary key value

type CFDictionaryRef key value = Ptr (CFDictionary key value)

newtype CFDictionaryKeyCallBacks =
  CFDictionaryKeyCallBacks (Ptr CFDictionaryKeyCallBacks)

newtype CFDictionaryValueCallBacks =
  CFDictionaryValueCallBacks (Ptr CFDictionaryValueCallBacks)

newCFDictionary :: [(Ptr key, Ptr value)] -> IO (CFDictionaryRef key value)
newCFDictionary elements =
  withArrayLen ks $ \len ks_p ->
  withArray    vs $ \vs_p ->
  c_CFDictionaryCreate defaultAllocator
                       ks_p
                       vs_p
                       (fromIntegral len)
                       p_CFTypeDictionaryKeyCallBacks
                       p_CFTypeDictionaryValueCallBacks
  where (ks, vs) = unzip elements

foreign import ccall unsafe "&kCFTypeDictionaryKeyCallBacks"
  p_CFTypeDictionaryKeyCallBacks :: CFDictionaryKeyCallBacks

foreign import ccall unsafe "&kCFTypeDictionaryValueCallBacks"
  p_CFTypeDictionaryValueCallBacks :: CFDictionaryValueCallBacks

foreign import ccall unsafe "CFDictionaryCreate"
  c_CFDictionaryCreate :: CFAllocatorRef
                       -> Ptr (Ptr key)
                       -> Ptr (Ptr value)
                       -> CFIndex
                       -> CFDictionaryKeyCallBacks
                       -> CFDictionaryValueCallBacks
                       -> IO (CFDictionaryRef key value)

foreign import ccall unsafe "CFDictionaryGetValue"
  c_CFDictionaryGetValue :: CFDictionaryRef key value
                         -> Ptr key
                         -> IO (Ptr value)
