module Web.Skype.API.Carbon (
  SkypeConnection,
  connect
) where

import Control.Applicative
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM.TChan (TChan, newBroadcastTChanIO, writeTChan)
import Control.Concurrent.STM.TMVar
import Control.Monad (when)
import Control.Monad.Error (Error, strMsg)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.STM (atomically)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Maybe
import Foreign hiding (addForeignPtrFinalizer)
import Foreign.Concurrent (addForeignPtrFinalizer)
import Web.Skype.API.Carbon.CFBase
import Web.Skype.API.Carbon.CFDictionary
import Web.Skype.API.Carbon.CFNotificationCenter
import Web.Skype.API.Carbon.CFNumber
import Web.Skype.API.Carbon.CFString
import Web.Skype.API.Carbon.CarbonEventsCore
import Web.Skype.Core

import qualified Data.ByteString as BS

type ClientID = Int
type ClientName = CFString

data SkypeConnection = SkypeConnection
  { skypeClientID :: TMVar ClientID
  , skypeNotificationChan :: TChan Notification
  , skypeNotificationCenter :: NotificationCenter
  , skypeThread :: ThreadId
  }

instance MonadIO m => MonadSkype (ReaderT SkypeConnection m) where
  sendCommand command = do
    center <- asks skypeNotificationCenter
    clientID <- asks skypeClientID >>= liftIO . atomically . readTMVar
    liftIO $ sendTo center clientID command

  getNotificationChan = asks skypeNotificationChan

connect :: (Error e, MonadIO m, MonadError e m)
        => BS.ByteString
        -> m SkypeConnection
connect appName = do
  connection <- liftIO $ newConnection appName
  clientID <- liftIO $ atomically $ readTMVar $ skypeClientID connection

  when (clientID <= 0) $ throwError $ strMsg "Couldn't connect to Skype client."

  liftIO $ addForeignPtrFinalizer
    (getNotificationCenter $ skypeNotificationCenter connection)
    (disconnectFrom (skypeNotificationCenter connection) clientID)

  return connection

newConnection :: BS.ByteString -> IO SkypeConnection
newConnection appName = do
  clientName <- newCFString appName >>= newForeignPtr p_CFRelease
  clientIDVar <- newEmptyTMVarIO
  notificatonChan <- newBroadcastTChanIO

  center <- getDistributedCenter clientName

  addObserver center "SKSkypeAPINotification" $ notificationCallback clientIDVar notificatonChan
  addObserver center "SKSkypeAttachResponse" $ attachResponseCallback clientIDVar clientName

  threadID <- forkIO $ c_RunCurrentEventLoop eventDurationForever

  attachTo center clientName

  return SkypeConnection
    { skypeClientID = clientIDVar
    , skypeNotificationChan = notificatonChan
    , skypeNotificationCenter = center
    , skypeThread = threadID
    }

disconnectFrom :: NotificationCenter -> ClientID -> IO ()
disconnectFrom center clientID =
  withNotificationCenter center $ \center_ptr ->
  withCFNumber clientID $ \clientID_ptr -> do
    userInfo <- newCFDictionary
      [ ("SKYPE_API_CLIENT_ID" :: CFStringRef, castPtr clientID_ptr) ]

    c_CFNotificationCenterPostNotification
      center_ptr
      "SKSkypeAPIDetachRequest"
      nullPtr
      userInfo
      True

    c_CFRelease userInfo

attachTo :: NotificationCenter -> ForeignPtr ClientName -> IO ()
attachTo center clientName =
  withNotificationCenter center $ \center_ptr ->
  withForeignPtr clientName $ \clientName_ptr ->
    c_CFNotificationCenterPostNotification
      center_ptr
      "SKSkypeAPIAttachRequest"
      clientName_ptr
      nullPtr
      True

sendTo :: NotificationCenter -> ClientID -> Command -> IO ()
sendTo center clientID command =
  withNotificationCenter center $ \center_ptr ->
  withCFString command $ \command_ptr ->
  withCFNumber clientID $ \clientID_ptr -> do
    userInfo <- newCFDictionary
      [ ("SKYPE_API_COMMAND" :: CFStringRef, castPtr command_ptr)
      , ("SKYPE_API_CLIENT_ID", castPtr clientID_ptr)
      ]

    c_CFNotificationCenterPostNotification
      center_ptr
      "SKSkypeAPICommand"
      nullPtr
      userInfo
      True

    c_CFRelease userInfo

notificationCallback :: TMVar ClientID
                     -> TChan Notification
                     -> CFNotificationCallback observer object CFString value
notificationCallback clientIDVar notificatonChan _ _ _ _ userInfo = do
  maybeClientID <- atomically $ tryReadTMVar clientIDVar

  case maybeClientID of
    Just clientID -> do
      otherClientID <- getClientID

      when (clientID == otherClientID) $
        getNotification >>= atomically . writeTChan notificatonChan

    Nothing -> return ()
  where
    getClientID = c_CFDictionaryGetValue userInfo "SKYPE_API_CLIENT_ID" >>=
                  fromCFNumber . castPtr

    getNotification = c_CFDictionaryGetValue userInfo "SKYPE_API_NOTIFICATION_STRING" >>=
                      fromCFString . castPtr

attachResponseCallback :: TMVar ClientID
                       -> ForeignPtr ClientName
                       -> CFNotificationCallback observer object CFString value
attachResponseCallback clientIDVar clientName _ _ _ _ userInfo = do
  comparisonResult <- withForeignPtr clientName $ \clientName_ptr -> do
    otherClientName <- getClientName
    c_CFStringCompare clientName_ptr otherClientName compareDefault

  when (comparisonResult == compareEqualTo) $ do
    maybeClientID <- getClientID
    atomically $ putTMVar clientIDVar $ fromMaybe 0 maybeClientID
  where
    getClientName = castPtr <$> c_CFDictionaryGetValue userInfo "SKYPE_API_CLIENT_NAME"

    getClientID = do
      clientID_ptr <- c_CFDictionaryGetValue userInfo "SKYPE_API_ATTACH_RESPONSE"
      if clientID_ptr == nullPtr
        then pure Nothing
        else Just <$> fromCFNumber (castPtr clientID_ptr)
