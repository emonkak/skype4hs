module Web.Skype.API.Carbon where

#include "CoreFoundation/CFBase.h"
#include "CoreFoundation/CFRunLoop.h"
#include "CoreFoundation/CFString.h"
#include "Skype/Skype.h"

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM.TChan (TChan, newBroadcastTChanIO, readTChan, writeTChan)
import Control.Monad (when)
import Control.Monad.Error (Error, strMsg)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Reader (MonadReader(..), ReaderT, asks)
import Control.Monad.STM (atomically)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.String
import Foreign hiding (unsafePerformIO)
import Foreign.C.String
import Foreign.C.Types
import System.Environment (getProgName)
import System.IO.Unsafe
import Web.Skype.Core

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BS

data SkypeConnection = SkypeConnection
  { skypeDelegate :: Ptr SkypeDelegate
  , skypeNotification :: TChan Notification
  , skypeThread :: ThreadId
  }

instance MonadIO m => MonadSkype (ReaderT SkypeConnection m) where
  sendCommand command = do
    liftIO $ withCFString command c_SendSkypeCommand
    return ()

  getNotification = asks skypeNotification

connect :: (Error e, MonadIO m, MonadError e m) => m SkypeConnection
connect = do
  chan <- liftIO newBroadcastTChanIO
  lock <- liftIO newEmptyMVar

  delegate <- liftIO $ createDelegate chan lock
  delegatePtr <- liftIO malloc

  result <- liftIO $ do
    poke delegatePtr delegate

    c_SetSkypeDelegate delegatePtr
    c_ConnectToSkype

    takeMVar lock

  if result == 1
    then return SkypeConnection
      { skypeDelegate = delegatePtr
      , skypeNotification = chan
      , skypeThread = undefined
      }
    else do
      liftIO $ releaseDelegate delegate
      liftIO $ free delegatePtr
      throwError $ strMsg "Can't attach to the API client."

createDelegate :: TChan BL.ByteString -> MVar CUInt -> IO SkypeDelegate
createDelegate chan lock = do
  applicationName <- getProgName >>= newCFString
  notificationListener <- wrapSkypeNotificationReceived $ \notification -> do
    fromCFString notification >>= atomically . writeTChan chan . BL.fromStrict
  attachListener <- wrapSkypeAttachResponse $ putMVar lock

  return SkypeDelegate
    { clientApplicationName     = applicationName
    , skypeNotificationReceived = notificationListener
    , skypeAttachResponse       = attachListener
    , skypeBecameAvailable      = nullFunPtr
    , skypeBecameUnavailable    = nullFunPtr
    }

releaseDelegate :: SkypeDelegate -> IO ()
releaseDelegate delegate = do
  freePtrIfNotNull $ clientApplicationName delegate
  freeFunPtrIfNotNull $ skypeNotificationReceived delegate
  freeFunPtrIfNotNull $ skypeAttachResponse delegate
  freeFunPtrIfNotNull $ skypeBecameAvailable delegate
  freeFunPtrIfNotNull $ skypeBecameUnavailable delegate
  where
    freePtrIfNotNull ptr = when (ptr /= nullPtr) $ c_CFRelease ptr
    freeFunPtrIfNotNull ptr = when (ptr /= nullFunPtr) $ freeHaskellFunPtr ptr




-- * Skype

data SkypeDelegate = SkypeDelegate
  { clientApplicationName :: CFStringRef
  , skypeNotificationReceived :: FunPtr (CFStringRef -> IO ())
  , skypeAttachResponse :: FunPtr (CUInt -> IO ())
  , skypeBecameAvailable :: FunPtr (CFPropertyListRef -> IO ())
  , skypeBecameUnavailable :: FunPtr (CFPropertyListRef -> IO ())
  }
  deriving (Show)

instance Storable SkypeDelegate where
  sizeOf = const #{size struct SkypeDelegate}

  alignment = sizeOf

  poke ptr delegate = do
    #{poke struct SkypeDelegate, clientApplicationName} ptr     $ clientApplicationName delegate
    #{poke struct SkypeDelegate, SkypeNotificationReceived} ptr $ skypeNotificationReceived delegate
    #{poke struct SkypeDelegate, SkypeAttachResponse} ptr       $ skypeAttachResponse delegate
    #{poke struct SkypeDelegate, SkypeBecameAvailable} ptr      $ skypeBecameAvailable delegate
    #{poke struct SkypeDelegate, SkypeBecameUnavailable} ptr    $ skypeBecameUnavailable delegate

  peek ptr = do
    clientApplicationName     <- #{peek struct SkypeDelegate, clientApplicationName} ptr
    skypeNotificationReceived <- #{peek struct SkypeDelegate, SkypeNotificationReceived} ptr
    skypeAttachResponse       <- #{peek struct SkypeDelegate, SkypeAttachResponse} ptr
    skypeBecameAvailable      <- #{peek struct SkypeDelegate, SkypeBecameAvailable} ptr
    skypeBecameUnavailable    <- #{peek struct SkypeDelegate, SkypeBecameUnavailable} ptr

    return $ SkypeDelegate
      { clientApplicationName     = clientApplicationName
      , skypeNotificationReceived = skypeNotificationReceived
      , skypeAttachResponse       = skypeAttachResponse
      , skypeBecameAvailable      = skypeBecameAvailable
      , skypeBecameUnavailable    = skypeBecameUnavailable
      }

foreign import ccall "IsSkypeRunning"
  c_IsSkypeRunning :: IO Bool

foreign import ccall "IsSkypeAvailable"
  c_IsSkypeAvailable :: IO Bool

foreign import ccall "SetSkypeDelegate"
  c_SetSkypeDelegate :: Ptr SkypeDelegate -> IO ()

foreign import ccall "GetSkypeDelegate"
  c_GetSkypeDelegate :: IO (Ptr SkypeDelegate)

foreign import ccall "RemoveSkypeDelegate"
  c_RemoveSkypeDelegate :: IO ()

foreign import ccall "ConnectToSkype"
  c_ConnectToSkype :: IO ()

foreign import ccall "DisconnectFromSkype"
  c_DisconnectFromSkype :: IO ()

foreign import ccall "SendSkypeCommand"
  c_SendSkypeCommand :: CFStringRef -> IO CFStringRef

foreign import ccall "wrapper"
  wrapSkypeNotificationReceived :: (CFStringRef -> IO ())
                                -> IO (FunPtr (CFStringRef -> IO ()))

foreign import ccall "wrapper"
  wrapSkypeAttachResponse :: (CUInt -> IO ())
                          -> IO (FunPtr (CUInt -> IO ()))

foreign import ccall "wrapper"
  wrapSkypeBecameAvailable :: (CFPropertyListRef -> IO ())
                           -> IO (FunPtr (CFPropertyListRef -> IO ()))

foreign import ccall "wrapper"
  wrapSkypeBecameUnavailable :: (CFPropertyListRef -> IO ())
                             -> IO (FunPtr (CFPropertyListRef -> IO ()))




-- * CoreFoundation

type CFIndex = Int32

data CFAllocator

type CFAllocatorRef = Ptr CFAllocator

foreign import ccall "&kCFAllocatorDefault"
  ptr_CFAllocatorDefault :: Ptr CFAllocatorRef

foreign import ccall "CFRelease"
  c_CFRelease :: Ptr a -> IO ()

defaultAllocator :: CFAllocatorRef
defaultAllocator = unsafePerformIO $ peek ptr_CFAllocatorDefault

data CFPropertyList

type CFPropertyListRef = Ptr CFPropertyList

foreign import ccall "CFRunLoopRun"
  c_CFRunLoopRun :: IO ()




data CFString

type CFStringRef = Ptr CFString

class CFStringFactory a where
  newCFString :: a -> IO CFStringRef

  withCFString :: a -> (CFStringRef -> IO b) -> IO b
  withCFString source action = do
    string <- newCFString source
    result <- action string
    c_CFRelease string
    return result

  fromCFString :: CFStringRef -> IO a

instance CFStringFactory CString where
  newCFString cs = c_CFStringCreateWithCString defaultAllocator cs utf8

  fromCFString source = do
    bufferPtr <- mallocArray $ fromIntegral bufferSize
    c_CFStringGetCString source bufferPtr bufferSize utf8
    return bufferPtr
    where
      len = c_CFStringGetLength source

      bufferSize = c_CFStringGetMaximumSizeForEncoding len utf8 + 1

instance CFStringFactory String where
  newCFString str = withCStringLen str $ \(ptr, len) -> do
    c_CFStringCreateWithBytes defaultAllocator
                              (castPtr ptr)
                              (fromIntegral len)
                              utf8
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
                              utf8
                              False

  fromCFString source = fromCFString source >>= BS.unsafePackCString

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

foreign import ccall "__CFStringMakeConstantString"
  c_CFStringMakeConstantString :: CString -> IO CFStringRef

foreign import ccall "CFStringCreateWithBytes"
  c_CFStringCreateWithBytes :: CFAllocatorRef  -- | alloc
                            -> Ptr Word8  -- | bytes
                            -> CFIndex  -- | numBytes
                            -> CFStringEncoding  -- | encoding
                            -> Bool  -- | isExternalRepresentation
                            -> IO CFStringRef

foreign import ccall "CFStringCreateWithCString"
  c_CFStringCreateWithCString :: CFAllocatorRef  -- | alloc
                              -> CString  -- | cStr
                              -> CFStringEncoding  -- | encoding
                              -> IO CFStringRef

foreign import ccall "CFStringGetCString"
  c_CFStringGetCString :: CFStringRef  -- | theString
                       -> CString  -- | buffer
                       -> CFIndex  -- | bufferSize
                       -> CFStringEncoding  -- | encoding
                       -> IO Bool

foreign import ccall "CFStringGetLength"
  c_CFStringGetLength :: CFStringRef -> CFIndex

foreign import ccall "CFStringGetMaximumSizeForEncoding"
  c_CFStringGetMaximumSizeForEncoding :: CFIndex  -- | length
                                      -> CFStringEncoding  -- | encoding
                                      -> CFIndex
