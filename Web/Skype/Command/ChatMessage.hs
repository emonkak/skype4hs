module Web.Skype.Command.ChatMessage where

import Control.Monad.Trans (MonadIO)
import Data.Monoid ((<>))
import Web.Skype.Command.Utils
import Web.Skype.Core
import Web.Skype.Parser
import Web.Skype.Protocol.ChatMessage
import Web.Skype.Protocol.Types

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Encoding as T

-- | GET CHATMESSAGE <id> TIMESTAMP
getTimestamp :: (MonadIO m, MonadSkype m) => ChatMessageID -> m Timestamp
getTimestamp messageID = handleCommandWithID command $ \response ->
  case response of
    ChatMessageResponse _ (ChatMessageTimestamp timestamp) -> Just $ Right timestamp
    ErrorResponse error                                    -> Just $ Left error
    _                                                      -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show messageID)
                                 <> " TIMESTAMP"

-- | GET CHATMESSAGE <id> FROM_HANDLE
getFromHandle :: (MonadIO m, MonadSkype m) => ChatMessageID -> m UserHandle
getFromHandle chatMessageID =
  handleCommandWithID command $ \response ->
    case response of
      ChatMessageResponse _ (ChatMessageFromHandle userHandle) -> Just $ Right userHandle
      ErrorResponse error                                      -> Just $ Left error
      _                                                        -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " FROM_HANDLE"

-- | GET CHATMESSAGE <id> FROM_DISPNAME
getFromDisplayName :: (MonadIO m, MonadSkype m) => ChatMessageID -> m UserHandle
getFromDisplayName chatMessageID = handleCommandWithID command $ \response ->
  case response of
    ChatMessageResponse _ (ChatMessageFromDisplayName userHandle) -> Just $ Right userHandle
    ErrorResponse error                                           -> Just $ Left error
    _                                                             -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " FROM_DISPNAME"

-- | GET CHATMESSAGE <id> TYPE
getType :: (MonadIO m, MonadSkype m) => ChatMessageID -> m ChatMessageType
getType chatMessageID = handleCommandWithID command $ \response ->
  case response of
    ChatMessageResponse _ (ChatMessageType messageType) -> Just $ Right messageType
    ErrorResponse error                                 -> Just $ Left error
    _                                                   -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID) <> " TYPE"

-- | GET CHATMESSAGE <id> STATUS
getStatus :: (MonadIO m, MonadSkype m) => ChatMessageID -> m ChatMessageStatus
getStatus chatMessageID = handleCommandWithID command $ \response ->
  case response of
    ChatMessageResponse _ (ChatMessageStatus messageStatus) -> Just $ Right messageStatus
    ErrorResponse error                                     -> Just $ Left error
    _                                                       -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " STATUS"

-- | GET CHATMESSAGE <id> LEAVEREASON
getLeaveReason :: (MonadIO m, MonadSkype m) => ChatMessageID -> m ChatMessageLeaveReason
getLeaveReason chatMessageID = handleCommandWithID command $ \response ->
  case response of
    ChatMessageResponse _ (ChatMessageLeaveReason leaveReason) -> Just $ Right leaveReason
    ErrorResponse error                                        -> Just $ Left error
    _                                                          -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " LEAVEREASON"

-- | GET CHATMESSAGE <id> CHATNAME
getChatName :: (MonadIO m, MonadSkype m) => ChatMessageID -> m ChatID
getChatName chatMessageID = handleCommandWithID command $ \response ->
  case response of
    ChatMessageResponse _ (ChatMessageChatName chatID) -> Just $ Right chatID
    ErrorResponse error                                -> Just $ Left error
    _                                                  -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " CHATNAME"

-- | GET CHATMESSAGE <id> BODY
getBody :: (MonadIO m, MonadSkype m) => ChatMessageID -> m ChatMessageBody
getBody chatMessageID = handleCommandWithID command $ \response ->
  case response of
    ChatMessageResponse _ (ChatMessageBody messageBody) -> Just $ Right messageBody
    ErrorResponse error                                 -> Just $ Left error
    _                                                   -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID) <> " BODY"
