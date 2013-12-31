module Web.Skype.Command (
  attachX11,
  protocol,
  sendChatMessage
) where

import Control.Concurrent.STM.TChan (readTChan)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.STM (atomically)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Unique (newUnique, hashUnique)
import System.Timeout (timeout)
import Web.Skype.Core
import Web.Skype.Parser
import Web.Skype.Protocol

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

attachX11 :: (MonadIO m, MonadReader (SkypeEnvironment c) m, MonadSkype m)
          => BS.ByteString
          -> m SkypeAttachStatus
attachX11 client = handleCommand command handler >>=
                   return . fromMaybe SkypeNotAvailable
  where
    command = "NAME " <> client

    handler "OK"                 = Just SkypeAttached
    handler "CONNSTATUS OFFLINE" = Just SkypeNotAvailable
    handler "ERROR 68"           = Just SkypeRefused
    handler _                    = Nothing

sendChatMessage :: (MonadIO m, MonadReader (SkypeEnvironment c) m, MonadSkype m)
                => ChatID
                -> ChatMessageBody
                -> m (Maybe ChatMessageID)
sendChatMessage chatID body = handleCommandWithID command handler
  where
    command = BL.toStrict $ BB.toLazyByteString $
      "CHATMESSAGE " <> BB.byteString chatID
                     <> " "
                     <> BB.byteString (T.encodeUtf8 body)

    handler (ChatMessageResponse chatMessageID _) = Just chatMessageID
    handler _ = Nothing

getRecentChatMessages :: (MonadIO m, MonadReader (SkypeEnvironment c) m, MonadSkype m)
                      => ChatID
                      -> m [ChatMessageID]
getRecentChatMessages chatID = handleCommandWithID command handler >>=
                               return . fromMaybe []
  where
    command = BL.toStrict $ BB.toLazyByteString $
              "GET CHAT " <> BB.byteString chatID <> " RECENTCHATMESSAGES"

    handler (ChatResponse _ (ChatRecentMessages chatMessageIDs)) = Just chatMessageIDs
    handler _ = Nothing

protocol :: (MonadIO m, MonadReader (SkypeEnvironment c) m, MonadSkype m)
         => Int
         -> m Bool
protocol version = handleCommand command handler >>=
                   return . fromMaybe False
  where
    command = "PROTOCOL " <> BS.pack (show version)

    handler responce
      | BL.isPrefixOf "PROTOCOL " responce = Just True
      | otherwise                          = Nothing

handleCommand :: (MonadIO m, MonadReader (SkypeEnvironment c) m, MonadSkype m)
              => Command
              -> (BL.ByteString -> Maybe a)
              -> m (Maybe a)
handleCommand command handler = do
  chan <- duplicateChannel

  sendCommand command

  time <- asks $ (* 1000) . skypeTimeout

  liftIO $ timeout time $ loop chan
  where
    loop chan = do
      responce <- atomically $ readTChan chan

      case handler responce of
        Just x  -> return x
        Nothing -> loop chan

handleCommandWithID :: (MonadIO m, MonadReader (SkypeEnvironment c) m, MonadSkype m)
                    => Command
                    -> (SkypeResponse -> Maybe a)
                    -> m (Maybe a)
handleCommandWithID command handler = do
  commandID <- liftIO $ (BB.intDec . hashUnique) `fmap` newUnique

  let command' = BL.toStrict $ BB.toLazyByteString $
                 "#" <> commandID <> " " <> BB.byteString command

  handleCommand command' $
                createHandler $ BL.toStrict $ BB.toLazyByteString commandID

  where
    createHandler exceptCommandID = \responce ->
      case parseResponseWithCommandID responce of
        Right (commandID, response')
          | commandID == exceptCommandID -> handler response'
          | otherwise                    -> Nothing
        Left _                           -> Nothing
