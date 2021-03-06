module Network.Skype.API.X11 (
  Connection,
  DisplayAddress,
  connect,
  connectTo
) where

#include <X11/Xlib.h>

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM.TChan (TChan, newBroadcastTChanIO, writeTChan)
import Control.Exception (IOException)
import Control.Exception.Lifted (catch)
import Control.Monad (mplus)
import Control.Monad.Error (Error, strMsg)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.STM (atomically)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl, control)
import Data.Maybe (listToMaybe)
import Data.Monoid (mappend, mempty)
import Foreign hiding (addForeignPtrFinalizer, newForeignPtr)
import Foreign.C.Types
import Foreign.Concurrent
import System.Environment (getEnv)
import Network.Skype.Command.Misc (authenticate)
import Network.Skype.Core

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Builder as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as X

data Connection = Connection
  { skypeAPI :: SkypeAPI
  , skypeNotificatonChan :: TChan Notification
  , skypeThread :: ThreadId
  }

data SkypeAPI = SkypeAPI
  { skypeDisplay :: ForeignPtr X.Display
  , skypeWindow :: X.Window
  , skypeInstanceWindow :: X.Window
  , skypeMessageBeginAtom :: X.Atom
  , skypeMessageContinueAtom :: X.Atom
  }
  deriving (Show, Eq)

type DisplayAddress = String

instance MonadIO m => MonadSkype (ReaderT Connection m) where
  sendCommand command = asks skypeAPI >>= liftIO . flip sendTo command

  getNotificationChan = asks skypeNotificatonChan

openDisplay :: (MonadBaseControl IO m, MonadIO m, MonadError IOException m)
            => DisplayAddress
            -> m (ForeignPtr X.Display)
openDisplay address = do
  display'@(X.Display display) <- liftIO (X.openDisplay address) `catch` throwError
  liftIO $ newForeignPtr display $ X.closeDisplay display'

getDisplayAddress :: (MonadBaseControl IO m, MonadIO m, MonadError IOException m)
                  => m DisplayAddress
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
        => ApplicationName
        -> m Connection
connect appName = flip connectTo appName =<< getDisplayAddress

connectTo :: (MonadBaseControl IO m, MonadIO m, MonadError IOException m)
          => DisplayAddress
          -> ApplicationName
          -> m Connection
connectTo address appName = do
  api <- createAPI address
  notificationChan <- liftIO newBroadcastTChanIO
  thread <- liftIO $ forkIO $
            runEventLoop api $ atomically . writeTChan notificationChan

  let connection = Connection
                   { skypeAPI = api
                   , skypeNotificatonChan = notificationChan
                   , skypeThread = thread
                   }

  result <- runSkype connection $ authenticate appName

  either (throwError . strMsg . show) (const $ return connection) result

createAPI :: (MonadBaseControl IO m, MonadIO m, MonadError IOException m)
          => String
          -> m SkypeAPI
createAPI address = do
  display <- openDisplay address

  withForeignPtr' display $ \display_ptr -> do
    let display' = X.Display display_ptr
    let root = X.defaultRootWindow display'

    window <- createWindow display' root

    liftIO $ addForeignPtrFinalizer display $
             X.destroyWindow display' window

    instanceWindow <- getSkypeInstanceWindow display' root

    messageBeginAtom <- createAtom display' "SKYPECONTROLAPI_MESSAGE_BEGIN"
    messageContinueAtom <- createAtom display' "SKYPECONTROLAPI_MESSAGE"

    return $ SkypeAPI
      { skypeDisplay             = display
      , skypeWindow              = window
      , skypeInstanceWindow      = instanceWindow
      , skypeMessageBeginAtom    = messageBeginAtom
      , skypeMessageContinueAtom = messageContinueAtom
      }

sendTo :: SkypeAPI -> BS.ByteString -> IO ()
sendTo api message =
  X.allocaXEvent $ \event_ptr ->
  withForeignPtr (skypeDisplay api) $ \display_ptr -> do
    #{poke XClientMessageEvent, type}         event_ptr (#{const ClientMessage} :: CInt)
    #{poke XClientMessageEvent, display}      event_ptr display_ptr
    #{poke XClientMessageEvent, window}       event_ptr $ skypeWindow api
    #{poke XClientMessageEvent, message_type} event_ptr $ skypeMessageBeginAtom api
    #{poke XClientMessageEvent, format}       event_ptr (8 :: CInt)  -- 8 bit values

    let display = X.Display display_ptr

    case splitPerChunk message of
      []       -> return ()
      (bs:[])  -> send display event_ptr bs
      (bs:bss) -> do
        send display event_ptr bs

        #{poke XClientMessageEvent, message_type} event_ptr $ skypeMessageContinueAtom api

        mapM_ (send display event_ptr) bss

  where
    send display event_ptr chunk = do
      let data_ptr = #{ptr XClientMessageEvent, data} event_ptr

      BS.unsafeUseAsCStringLen chunk $ uncurry $ copyArray data_ptr

      X.sendEvent display (skypeInstanceWindow api) False 0 event_ptr
      X.flush display

    splitPerChunk bs
      | BS.length bs == messageChunkSize = bs : BS.singleton 0 : []
      | BS.length bs < messageChunkSize  = BS.snoc bs 0 : []
      | otherwise                        = let (xs, ys) = BS.splitAt messageChunkSize bs
                                           in  xs : splitPerChunk ys

-- | Generalized version of 'withForeignPtr'.
withForeignPtr' :: (MonadBaseControl IO m) => ForeignPtr a -> (Ptr a -> m b) -> m b
withForeignPtr' fp action =
  control $ \runInIO -> withForeignPtr fp $ runInIO . action

ptrIndex :: (Eq a, Storable a) => Ptr a -> a -> Int -> IO (Maybe Int)
ptrIndex ptr value n = go 0 value $ take n $ iterate (flip plusPtr 1) ptr
  where
    go _ _ [] = return Nothing
    go acc x (y:ys) = do
      y' <- peek y
      if y' == x
        then return $ Just acc
        else go (acc + 1) x ys

messageChunkSize :: Int
messageChunkSize = #{const sizeof(((XClientMessageEvent *) 0)->data.b) / sizeof(((XClientMessageEvent *) 0)->data.b[0])}
{-# INLINE messageChunkSize #-}

runEventLoop :: SkypeAPI -> (BL.ByteString -> IO ()) -> IO ()
runEventLoop api action =
  X.allocaXEvent $ \event_ptr ->
  withForeignPtr (skypeDisplay api) $ \display_ptr ->
  loop mempty (X.Display display_ptr) event_ptr
  where
    loop builder display event_ptr = do
      X.lockDisplay display
      X.nextEvent display event_ptr  -- will blocking
      X.unlockDisplay display

      eventType <- #{peek XClientMessageEvent, type} event_ptr

      if eventType == X.clientMessage
      then do
        messageType <- #{peek XClientMessageEvent, message_type} event_ptr

        if messageType == skypeMessageBeginAtom api ||
           messageType == skypeMessageContinueAtom api
        then do
          let data_ptr = #{ptr XClientMessageEvent, data} event_ptr

          maybeIndex <- ptrIndex data_ptr (0 :: CChar) messageChunkSize

          case maybeIndex of
            Just i -> do
              bs <- BS.packCStringLen (data_ptr, i)
              action $ BS.toLazyByteString $ builder `mappend` BS.byteString bs
              loop mempty display event_ptr

            Nothing -> do
              bs <- BS.packCStringLen (data_ptr, messageChunkSize)
              loop (builder `mappend` BS.byteString bs) display event_ptr
        else
          loop builder display event_ptr
      else
        loop builder display event_ptr
