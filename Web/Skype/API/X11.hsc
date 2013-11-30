{-# LANGUAGE FlexibleContexts,
             GeneralizedNewtypeDeriving #-}

module Web.Skype.API.X11 (
  Skype,
  SkypeAPI,
  connect,
  connectTo,
  runSkype,
  runEventLoop,
  withSkype
)
where

#include <X11/Xlib.h>

import Control.Applicative (Applicative)
import Control.Exception (IOException)
import Control.Exception.Lifted (catch)
import Control.Monad (mplus, when)
import Control.Monad.Error (Error(..))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.Trans (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Bits ((.&.))
import Data.List (elemIndex, unfoldr)
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid(..))
import Data.Typeable (Typeable)
import Foreign
import Foreign.C.String
import Foreign.C.Types
import System.Environment (getEnv)
import Web.Skype.Monad

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BS
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as X

newtype Skype m a = Skype (ReaderT SkypeAPI m a)
   deriving (Applicative, Functor, Monad, MonadIO)

data SkypeAPI = SkypeAPI
  { skypeDisplay :: X.Display
  , skypeWindow :: X.Window
  , skypeInstanceWindow :: X.Window
  , skypeMessageBeginAtom :: X.Atom
  , skypeMessageContinueAtom :: X.Atom
  }
  deriving (Show, Eq)

instance MonadIO m => MonadSkype (Skype m) where
  send message = Skype $ ask >>= liftIO . flip sendTo message

-- | runSkype
runSkype :: Skype m a -> SkypeAPI -> m a
runSkype (Skype skype) = runReaderT skype

-- | withSkype
withSkype :: SkypeAPI -> Skype m a -> m a
withSkype = flip runSkype

-- | getDisplayAddress
getDisplayAddress :: (MonadBaseControl IO m, MonadIO m, MonadError IOException m)
                  => m String
getDisplayAddress = liftIO (getEnv "SKYPEDISPLAY" `mplus` getEnv "DISPLAY")
                    `catch` throwError

-- | openDisplay
openDisplay :: (MonadBaseControl IO m, MonadIO m, MonadError IOException m)
            => String -> m X.Display
openDisplay address = liftIO (X.openDisplay address) `catch` throwError

-- | createWindow
createWindow :: (Error e, MonadIO m, MonadError e m)
             => X.Display -> X.Window -> m X.Window
createWindow display root = do
  let screen = X.defaultScreen display
      pixel  = X.blackPixel display screen

  window <- liftIO $ X.createSimpleWindow display root 0 0 1 1 0 pixel pixel

  if window == X.none
     then throwError $ strMsg "Can't create the window"
     else return window

-- | createAtom
createAtom :: (Error e, MonadIO m, MonadError e m)
        => X.Display -> String -> m X.Atom
createAtom display atomName = do
  atom <- liftIO $ X.internAtom display atomName True

  if atom == X.none
    then throwError $ strMsg $ "Can't create the atom: " ++ atomName
    else return atom

-- | getSkypeInstance
getSkypeInstanceWindow :: (Error e, MonadIO m, MonadError e m)
                       => X.Display -> X.Window -> m X.Window
getSkypeInstanceWindow display root = do
  instanceAtom <- createAtom display "_SKYPE_INSTANCE"

  status <- liftIO $ X.getWindowProperty32 display instanceAtom root

  case status >>= listToMaybe of
    Nothing       -> throwError $ strMsg "Skype instance is not found"
    Just property -> return $ fromIntegral property .&. 0xffffffff

-- | connect
connect :: (MonadBaseControl IO m, MonadIO m, MonadError IOException m) => m SkypeAPI
connect = getDisplayAddress >>= connectTo

-- | connectTo
connectTo :: (MonadBaseControl IO m, MonadIO m, MonadError IOException m)
          => String -> m SkypeAPI
connectTo address = do
  display <- openDisplay address

  let root = X.defaultRootWindow display

  window <- createWindow display root `catchError` closeDisplay display

  instanceWindow <- getSkypeInstanceWindow display root `catchError`
                    closeDisplayAndWindow display window

  messageBeginAtom <- createAtom display "SKYPECONTROLAPI_MESSAGE_BEGIN" `catchError`
                      closeDisplayAndWindow display window

  messageContinueAtom <- createAtom display "SKYPECONTROLAPI_MESSAGE" `catchError`
                         closeDisplayAndWindow display window

  return SkypeAPI
    { skypeDisplay             = display
    , skypeWindow              = window
    , skypeInstanceWindow      = instanceWindow
    , skypeMessageBeginAtom    = messageBeginAtom
    , skypeMessageContinueAtom = messageContinueAtom
    }
  where
    closeDisplay display error = do
      liftIO $ X.closeDisplay display
      throwError error

    closeDisplayAndWindow display window error = do
      liftIO $ X.destroyWindow display window
      liftIO $ X.closeDisplay display
      throwError error

-- | sendMessage
sendTo :: SkypeAPI -> BS.ByteString -> IO ()
sendTo api message = X.allocaXEvent $ \p_event -> do
  let (X.Display display) = skypeDisplay api

  #{poke XClientMessageEvent, type}         p_event (#{const ClientMessage} :: CInt)
  #{poke XClientMessageEvent, display}      p_event display
  #{poke XClientMessageEvent, window}       p_event $ skypeWindow api
  #{poke XClientMessageEvent, message_type} p_event $ skypeMessageBeginAtom api
  #{poke XClientMessageEvent, format}       p_event (8 :: CInt)  -- 8 bit values

  sendChunks 0 p_event $ unfoldr splitPerChunk message

  where
    sendChunks n p_event []       = return ()
    sendChunks n p_event (bs:[])  = send p_event $ bs `BS.snoc` 0
    sendChunks 0 p_event (bs:bss) = do
      send p_event bs
      #{poke XClientMessageEvent, message_type} p_event $ skypeMessageContinueAtom api
      sendChunks 1 p_event bss
    sendChunks n p_event (bs:bss) = do
      send p_event bs
      sendChunks (n + 1) p_event bss

    send p_event chunk = do
      let p_data = #{ptr XClientMessageEvent, data} p_event

      BS.useAsCStringLen chunk $ uncurry $ copyArray p_data

      let display = skypeDisplay api

      X.sendEvent display (skypeInstanceWindow api) False 0 p_event
      X.flush display

    splitPerChunk bs
      | BS.null bs = Nothing
      | otherwise  = Just $ BS.splitAt messageChunkSize bs

-- | ptrIndex
ptrIndex :: (Eq a, Storable a) => Ptr a -> a -> Int -> IO (Maybe Int)
ptrIndex p x n = go 0 x $ take n $ iterate (flip plusPtr 1) p
  where
    go acc element [] = return Nothing
    go acc element (x:xs) = do
      x' <- peek x
      if x' == element
        then return $ Just acc
        else go (acc + 1) element xs

-- | messageChunkSize
messageChunkSize :: Int
messageChunkSize = #{const sizeof(((XClientMessageEvent *) 0)->data.b) / sizeof(((XClientMessageEvent *) 0)->data.b[0])}

-- | runEventLoop
runEventLoop :: SkypeAPI -> (BL.ByteString -> IO ()) -> IO ()
runEventLoop api f = X.allocaXEvent $ loop mempty
  where
    loop :: BS.Builder -> X.XEventPtr -> IO ()
    loop builder p_event = do
      X.nextEvent (skypeDisplay api) p_event

      eventType <- #{peek XClientMessageEvent, type} p_event

      if eventType == X.clientMessage
      then do
        messageType <- #{peek XClientMessageEvent, message_type} p_event

        if messageType == (skypeMessageBeginAtom api) ||
           messageType == (skypeMessageContinueAtom api)
        then do
          let p_data = #{ptr XClientMessageEvent, data} p_event

          maybeIndex <- ptrIndex p_data (0 :: CChar) messageChunkSize

          case maybeIndex of
            Just i -> do
              bs <- BS.packCStringLen (p_data, i)
              f $ BS.toLazyByteString $ builder `mappend` BS.byteString bs
              loop mempty p_event

            Nothing -> do
              bs <- BS.packCStringLen (p_data, messageChunkSize)
              loop (builder `mappend` BS.byteString bs) p_event
        else
          loop builder p_event
      else
        loop builder p_event
