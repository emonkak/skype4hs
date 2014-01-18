module Web.Skype.API.X11 (
  SkypeConnection,
  connect,
  connectTo,
  disconnect
) where

#include <X11/Xlib.h>

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM.TChan (TChan, dupTChan, newBroadcastTChanIO, readTChan, writeTChan)
import Control.Exception (IOException)
import Control.Exception.Lifted (catch)
import Control.Monad (mplus)
import Control.Monad.Error (Error, runErrorT, strMsg)
import Control.Monad.Error.Class (MonadError, catchError, throwError)
import Control.Monad.Reader (MonadReader(..), ReaderT, asks, runReaderT)
import Control.Monad.STM (atomically)
import Control.Monad.Trans (MonadTrans, MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Bits ((.&.))
import Data.Maybe (listToMaybe)
import Data.Monoid (mappend, mempty)
import Data.Typeable (Typeable)
import Foreign
import Foreign.C.String
import Foreign.C.Types
import System.Environment (getEnv, getProgName)
import Web.Skype.Core
import Web.Skype.Command.Misc (name, protocol)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BS
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as X

data SkypeConnection = SkypeConnection
  { skypeApi :: SkypeAPI
  , skypeChannel :: SkypeChannel
  , skypeThread :: ThreadId
  }

data SkypeAPI = SkypeAPI
  { skypeDisplay :: X.Display
  , skypeWindow :: X.Window
  , skypeInstanceWindow :: X.Window
  , skypeMessageBeginAtom :: X.Atom
  , skypeMessageContinueAtom :: X.Atom
  }
  deriving (Show, Eq)

instance MonadIO m => MonadSkype (ReaderT SkypeConnection m) where
  sendCommand command = asks skypeApi >>= liftIO . flip sendTo command

  getSkypeChannel = asks skypeChannel

openDisplay :: (MonadBaseControl IO m, MonadIO m, MonadError IOException m)
            => String -> m X.Display
openDisplay address = liftIO (X.openDisplay address) `catch` throwError

getDisplayAddress :: (MonadBaseControl IO m, MonadIO m, MonadError IOException m)
                  => m String
getDisplayAddress = liftIO (getEnv "SKYPEDISPLAY" `mplus` getEnv "DISPLAY")
                    `catch` throwError

createWindow :: (Error e, MonadIO m, MonadError e m)
             => X.Display -> X.Window -> m X.Window
createWindow display root = do
  let screen = X.defaultScreen display
      pixel  = X.blackPixel display screen

  window <- liftIO $ X.createSimpleWindow display root 0 0 1 1 0 pixel pixel

  if window == X.none
     then throwError $ strMsg "Can't create the window"
     else return window

createAtom :: (Error e, MonadIO m, MonadError e m)
           => X.Display -> String -> m X.Atom
createAtom display name = do
  atom <- liftIO $ X.internAtom display name True

  if atom == X.none
    then throwError $ strMsg $ "Can't create the atom: " ++ name
    else return atom

getSkypeInstanceWindow :: (Error e, MonadIO m, MonadError e m)
                       => X.Display -> X.Window -> m X.Window
getSkypeInstanceWindow display root = do
  instanceAtom <- createAtom display "_SKYPE_INSTANCE"

  status <- liftIO $ X.getWindowProperty32 display instanceAtom root

  case status >>= listToMaybe of
    Nothing       -> throwError $ strMsg "Skype instance window is not found"
    Just property -> return $ fromIntegral property .&. 0xffffffff

connect :: (MonadBaseControl IO m, MonadIO m, MonadError IOException m)
        => m SkypeConnection
connect = connectTo =<< getDisplayAddress

connectTo :: (MonadBaseControl IO m, MonadIO m, MonadError IOException m)
          => String
          -> m SkypeConnection
connectTo address = do
  api <- createApi address
  chan <- liftIO newBroadcastTChanIO
  thread <- liftIO $ forkIO $ runEventLoop api $ atomically . writeTChan chan

  let connection = SkypeConnection
                   { skypeApi = api
                   , skypeChannel = chan
                   , skypeThread = thread
                   }

  result <- runSkype connection defaultConfig $ do
    liftIO getProgName >>= name . BC.pack
    protocol 9999

  case result of
    Left error -> disconnect connection >> throwError (strMsg $ show error)
    Right _    -> return connection

disconnect :: (MonadIO m) => SkypeConnection -> m ()
disconnect connection = liftIO $ do
  killThread $ skypeThread connection

  let api = skypeApi connection
  let display = skypeDisplay api
  let window = skypeWindow api

  X.destroyWindow display window
  X.closeDisplay display

createApi :: (MonadBaseControl IO m, MonadIO m, MonadError IOException m) => String -> m SkypeAPI
createApi address = do
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

sendTo :: SkypeAPI -> BS.ByteString -> IO ()
sendTo api message = X.allocaXEvent $ \p_event -> do
  let (X.Display display) = skypeDisplay api

  #{poke XClientMessageEvent, type}         p_event (#{const ClientMessage} :: CInt)
  #{poke XClientMessageEvent, display}      p_event display
  #{poke XClientMessageEvent, window}       p_event $ skypeWindow api
  #{poke XClientMessageEvent, message_type} p_event $ skypeMessageBeginAtom api
  #{poke XClientMessageEvent, format}       p_event (8 :: CInt)  -- 8 bit values

  case splitPerChunk message of
    []       -> return ()
    (bs:[])  -> send p_event bs
    (bs:bss) -> do
      send p_event bs

      #{poke XClientMessageEvent, message_type} p_event $ skypeMessageContinueAtom api

      mapM_ (send p_event) bss

  where
    send p_event chunk = do
      let p_data = #{ptr XClientMessageEvent, data} p_event

      BS.unsafeUseAsCStringLen chunk $ uncurry $ copyArray p_data

      let display = skypeDisplay api

      X.sendEvent display (skypeInstanceWindow api) False 0 p_event
      X.flush display

    splitPerChunk bs
      | BS.length bs == messageChunkSize = bs : BS.singleton 0 : []
      | BS.length bs < messageChunkSize  = BS.snoc bs 0 : []
      | otherwise                        = let (xs, ys) = BS.splitAt messageChunkSize bs
                                           in  xs : splitPerChunk ys

ptrIndex :: (Eq a, Storable a) => Ptr a -> a -> Int -> IO (Maybe Int)
ptrIndex p x n = go 0 x $ take n $ iterate (flip plusPtr 1) p
  where
    go acc element [] = return Nothing
    go acc element (x:xs) = do
      x' <- peek x
      if x' == element
        then return $ Just acc
        else go (acc + 1) element xs

messageChunkSize :: Int
messageChunkSize = #{const sizeof(((XClientMessageEvent *) 0)->data.b) / sizeof(((XClientMessageEvent *) 0)->data.b[0])}
{-# INLINE messageChunkSize #-}

runEventLoop :: SkypeAPI -> (BL.ByteString -> IO ()) -> IO ()
runEventLoop api action = X.allocaXEvent $ loop mempty
  where
    loop :: BS.Builder -> X.XEventPtr -> IO ()
    loop builder p_event = do
      X.nextEvent (skypeDisplay api) p_event  -- will blocking

      eventType <- #{peek XClientMessageEvent, type} p_event

      if eventType == X.clientMessage
      then do
        messageType <- #{peek XClientMessageEvent, message_type} p_event

        if messageType == skypeMessageBeginAtom api ||
           messageType == skypeMessageContinueAtom api
        then do
          let p_data = #{ptr XClientMessageEvent, data} p_event

          maybeIndex <- ptrIndex p_data (0 :: CChar) messageChunkSize

          case maybeIndex of
            Just i -> do
              bs <- BS.packCStringLen (p_data, i)
              action $ BS.toLazyByteString $ builder `mappend` BS.byteString bs
              loop mempty p_event

            Nothing -> do
              bs <- BS.packCStringLen (p_data, messageChunkSize)
              loop (builder `mappend` BS.byteString bs) p_event
        else
          loop builder p_event
      else
        loop builder p_event
