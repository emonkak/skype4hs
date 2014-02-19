module Web.Skype.Command.ChatMessage where

import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Monoid ((<>))
import Web.Skype.Command.Utils
import Web.Skype.Core
import Web.Skype.Protocol

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Encoding as T

-- | Returns the time when message was sent.
getTimestamp :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatMessageID -> SkypeT m Timestamp
getTimestamp chatMessageID = executeCommandWithID command $ \response ->
  case response of
    ChatMessage _ (ChatMessageTimestamp timestamp) -> return $ Just timestamp
    _                                              -> return Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " TIMESTAMP"

-- | Returns the skype name of the sender of this message.
getSender :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatMessageID -> SkypeT m UserID
getSender chatMessageID = executeCommandWithID command $ \response ->
  case response of
    ChatMessage _ (ChatMessageFromHandle userHandle) -> return $ Just userHandle
    _                                                -> return Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " FROM_HANDLE"

-- | Returns the displayed name of the sender of this message.
getSenderDisplayName :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatMessageID -> SkypeT m UserDisplayName
getSenderDisplayName chatMessageID = executeCommandWithID command $ \response ->
  case response of
    ChatMessage _ (ChatMessageFromDisplayName userHandle) -> return $ Just userHandle
    _                                                     -> return Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " FROM_DISPNAME"

-- | Returns the message type.
getType :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatMessageID -> SkypeT m ChatMessageType
getType chatMessageID = executeCommandWithID command $ \response ->
  case response of
    ChatMessage _ (ChatMessageType messageType) -> return $ Just messageType
    _                                           -> return Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID) <> " TYPE"

-- | Returns the message status.
getStatus :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatMessageID -> SkypeT m ChatMessageStatus
getStatus chatMessageID = executeCommandWithID command $ \response ->
  case response of
    ChatMessage _ (ChatMessageStatus messageStatus) -> return $ Just messageStatus
    _                                               -> return Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " STATUS"

-- | Returns the leave reason.
getLeaveReason :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatMessageID -> SkypeT m ChatMessageLeaveReason
getLeaveReason chatMessageID = executeCommandWithID command $ \response ->
  case response of
    ChatMessage _ (ChatMessageLeaveReason leaveReason) -> return $ Just leaveReason
    _                                                  -> return Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " LEAVEREASON"

-- | Returns the chat which this message belongs.
getChat :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatMessageID -> SkypeT m ChatID
getChat chatMessageID = executeCommandWithID command $ \response ->
  case response of
    ChatMessage _ (ChatMessageChatName chatID) -> return $ Just chatID
    _                                          -> return Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " CHATNAME"

-- | Returns all users that have been added to the chat.
getAllUsers :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatMessageID -> SkypeT m [UserID]
getAllUsers chatMessageID = executeCommandWithID command $ \response ->
  case response of
    ChatMessage _ (ChatMessageUsers userIDs) -> return $ Just userIDs
    _                                        -> return Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " USER"

-- | Indicates if the chat message is editable.
isEditable :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatMessageID -> SkypeT m Bool
isEditable chatMessageID = executeCommandWithID command $ \response ->
  case response of
    ChatMessage _ (ChatMessageIsEditable editable) -> return $ Just editable
    _                                              -> return Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " IS_EDITABLE"

-- | Returns the content of this chat message.
getBody :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatMessageID -> SkypeT m ChatMessageBody
getBody chatMessageID = executeCommandWithID command $ \response ->
  case response of
    ChatMessage _ (ChatMessageBody messageBody) -> return $ Just messageBody
    _                                           -> return Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID) <> " BODY"

-- | Sets the content of this chat message.
setBody :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatMessageID -> ChatMessageBody -> SkypeT m ChatMessageBody
setBody chatMessageID content = executeCommandWithID command $ \response ->
  case response of
    ChatMessage _ (ChatMessageBody messageBody) -> return $ Just messageBody
    _                                           -> return Nothing
  where
    command = "SET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " BODY "
                                 <> T.encodeUtf8 content
