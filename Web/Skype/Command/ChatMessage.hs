module Web.Skype.Command.ChatMessage (
  getTimestamp,
  getSender,
  getSenderDisplayName,
  getType,
  getStatus,
  getLeaveReason,
  getChat,
  getAllUsers,
  isEditable,
  getBody,
  setBody
) where

import Control.Monad.Trans (MonadIO)
import Data.Monoid ((<>))
import Web.Skype.Command.Utils
import Web.Skype.Core
import Web.Skype.Parser
import Web.Skype.Protocol

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Encoding as T

-- | Returns the time when message was sent.
getTimestamp :: (MonadIO m, MonadSkype m) => ChatMessageID -> Skype m Timestamp
getTimestamp chatMessageID = executeCommandWithID command $ \response ->
  case response of
    ChatMessage _ (ChatMessageTimestamp timestamp) -> Just $ Right timestamp
    Error code description                         -> Just $ Left $ SkypeError code command description
    _                                              -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " TIMESTAMP"

-- | Returns the skype name of the sender of this message.
getSender :: (MonadIO m, MonadSkype m) => ChatMessageID -> Skype m UserID
getSender chatMessageID = executeCommandWithID command $ \response ->
  case response of
    ChatMessage _ (ChatMessageFromHandle userHandle) -> Just $ Right userHandle
    Error code description                           -> Just $ Left $ SkypeError code command description
    _                                                -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " FROM_HANDLE"

-- | Returns the displayed name of the sender of this message.
getSenderDisplayName :: (MonadIO m, MonadSkype m) => ChatMessageID -> Skype m UserDisplayName
getSenderDisplayName chatMessageID = executeCommandWithID command $ \response ->
  case response of
    ChatMessage _ (ChatMessageFromDisplayName userHandle) -> Just $ Right userHandle
    Error code description                                -> Just $ Left $ SkypeError code command description
    _                                                     -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " FROM_DISPNAME"

-- | Returns the message type.
getType :: (MonadIO m, MonadSkype m) => ChatMessageID -> Skype m ChatMessageType
getType chatMessageID = executeCommandWithID command $ \response ->
  case response of
    ChatMessage _ (ChatMessageType messageType) -> Just $ Right messageType
    Error code description                      -> Just $ Left $ SkypeError code command description
    _                                           -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID) <> " TYPE"

-- | Returns the message status.
getStatus :: (MonadIO m, MonadSkype m) => ChatMessageID -> Skype m ChatMessageStatus
getStatus chatMessageID = executeCommandWithID command $ \response ->
  case response of
    ChatMessage _ (ChatMessageStatus messageStatus) -> Just $ Right messageStatus
    Error code description                          -> Just $ Left $ SkypeError code command description
    _                                               -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " STATUS"

-- | Returns the leave reason.
getLeaveReason :: (MonadIO m, MonadSkype m) => ChatMessageID -> Skype m ChatMessageLeaveReason
getLeaveReason chatMessageID = executeCommandWithID command $ \response ->
  case response of
    ChatMessage _ (ChatMessageLeaveReason leaveReason) -> Just $ Right leaveReason
    Error code description                             -> Just $ Left $ SkypeError code command description
    _                                                  -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " LEAVEREASON"

-- | Returns the chat which this message belongs.
getChat :: (MonadIO m, MonadSkype m) => ChatMessageID -> Skype m ChatID
getChat chatMessageID = executeCommandWithID command $ \response ->
  case response of
    ChatMessage _ (ChatMessageChatName chatID) -> Just $ Right chatID
    Error code description                     -> Just $ Left $ SkypeError code command description
    _                                          -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " CHATNAME"

-- | Returns all users that have been added to the chat.
getAllUsers :: (MonadIO m, MonadSkype m) => ChatMessageID -> Skype m [UserID]
getAllUsers chatMessageID = executeCommandWithID command $ \response ->
  case response of
    ChatMessage _ (ChatMessageUsers userIDs) -> Just $ Right userIDs
    Error code description                   -> Just $ Left $ SkypeError code command description
    _                                        -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " USER"

-- | Indicates if the chat message is editable.
isEditable :: (MonadIO m, MonadSkype m) => ChatMessageID -> Skype m Bool
isEditable chatMessageID = executeCommandWithID command $ \response ->
  case response of
    ChatMessage _ (ChatMessageIsEditable pred) -> Just $ Right pred
    Error code description                     -> Just $ Left $ SkypeError code command description
    _                                          -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " IS_EDITABLE"

-- | Returns the content of this chat message.
getBody :: (MonadIO m, MonadSkype m) => ChatMessageID -> Skype m ChatMessageBody
getBody chatMessageID = executeCommandWithID command $ \response ->
  case response of
    ChatMessage _ (ChatMessageBody messageBody) -> Just $ Right messageBody
    Error code description                      -> Just $ Left $ SkypeError code command description
    _                                           -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID) <> " BODY"

-- | Sets the content of this chat message.
setBody :: (MonadIO m, MonadSkype m) => ChatMessageID -> ChatMessageBody -> Skype m ChatMessageBody
setBody chatMessageID content = executeCommandWithID command $ \response ->
  case response of
    ChatMessage _ (ChatMessageBody messageBody) -> Just $ Right messageBody
    Error code description                      -> Just $ Left $ SkypeError code command description
    _                                           -> Nothing
  where
    command = "SET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " BODY "
                                 <> T.encodeUtf8 content
