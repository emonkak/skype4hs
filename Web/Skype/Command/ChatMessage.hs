module Web.Skype.Command.ChatMessage where

import Control.Monad.Trans (MonadIO)
import Data.Monoid ((<>))
import Web.Skype.Command.Utils
import Web.Skype.Core
import Web.Skype.Parser
import Web.Skype.Protocol

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Encoding as T

-- | GET CHATMESSAGE <id> TIMESTAMP
getTimestamp :: (MonadIO m, MonadSkype m) => ChatMessageID -> m Timestamp
getTimestamp (ChatMessageID chatMessageID) = executeCommandWithID command $ \response ->
  case response of
    ChatMessageResponse _ (ChatMessageTimestamp timestamp) -> Just $ Right timestamp
    ErrorResponse code description                         -> Just $ Left $ SkypeError code command description
    _                                                      -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " TIMESTAMP"

-- | GET CHATMESSAGE <id> FROM_HANDLE
getFromHandle :: (MonadIO m, MonadSkype m) => ChatMessageID -> m UserHandle
getFromHandle (ChatMessageID chatMessageID) = executeCommandWithID command $ \response ->
  case response of
    ChatMessageResponse _ (ChatMessageFromHandle userHandle) -> Just $ Right userHandle
    ErrorResponse code description                           -> Just $ Left $ SkypeError code command description
    _                                                        -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " FROM_HANDLE"

-- | GET CHATMESSAGE <id> FROM_DISPNAME
getFromDisplayName :: (MonadIO m, MonadSkype m) => ChatMessageID -> m UserHandle
getFromDisplayName (ChatMessageID chatMessageID) = executeCommandWithID command $ \response ->
  case response of
    ChatMessageResponse _ (ChatMessageFromDisplayName userHandle) -> Just $ Right userHandle
    ErrorResponse code description                                -> Just $ Left $ SkypeError code command description
    _                                                             -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " FROM_DISPNAME"

-- | GET CHATMESSAGE <id> TYPE
getType :: (MonadIO m, MonadSkype m) => ChatMessageID -> m ChatMessageType
getType (ChatMessageID chatMessageID) = executeCommandWithID command $ \response ->
  case response of
    ChatMessageResponse _ (ChatMessageType messageType) -> Just $ Right messageType
    ErrorResponse code description                      -> Just $ Left $ SkypeError code command description
    _                                                   -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID) <> " TYPE"

-- | GET CHATMESSAGE <id> STATUS
getStatus :: (MonadIO m, MonadSkype m) => ChatMessageID -> m ChatMessageStatus
getStatus (ChatMessageID chatMessageID) = executeCommandWithID command $ \response ->
  case response of
    ChatMessageResponse _ (ChatMessageStatus messageStatus) -> Just $ Right messageStatus
    ErrorResponse code description                          -> Just $ Left $ SkypeError code command description
    _                                                       -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " STATUS"

-- | GET CHATMESSAGE <id> LEAVEREASON
getLeaveReason :: (MonadIO m, MonadSkype m) => ChatMessageID -> m ChatMessageLeaveReason
getLeaveReason (ChatMessageID chatMessageID) = executeCommandWithID command $ \response ->
  case response of
    ChatMessageResponse _ (ChatMessageLeaveReason leaveReason) -> Just $ Right leaveReason
    ErrorResponse code description                             -> Just $ Left $ SkypeError code command description
    _                                                          -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " LEAVEREASON"

-- | GET CHATMESSAGE <id> CHATNAME
getChatName :: (MonadIO m, MonadSkype m) => ChatMessageID -> m ChatID
getChatName (ChatMessageID chatMessageID) = executeCommandWithID command $ \response ->
  case response of
    ChatMessageResponse _ (ChatMessageChatName chatID) -> Just $ Right chatID
    ErrorResponse code description                     -> Just $ Left $ SkypeError code command description
    _                                                  -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " CHATNAME"

-- | GET CHATMESSAGE <id> BODY
getBody :: (MonadIO m, MonadSkype m) => ChatMessageID -> m ChatMessageBody
getBody (ChatMessageID chatMessageID) = executeCommandWithID command $ \response ->
  case response of
    ChatMessageResponse _ (ChatMessageBody messageBody) -> Just $ Right messageBody
    ErrorResponse code description                      -> Just $ Left $ SkypeError code command description
    _                                                   -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID) <> " BODY"
