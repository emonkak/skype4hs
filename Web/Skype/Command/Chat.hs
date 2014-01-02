module Web.Skype.Command.Chat where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadIO)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import System.Posix.Types (EpochTime)
import Web.Skype.Command.Utils
import Web.Skype.Core
import Web.Skype.Parser
import Web.Skype.Protocol.Chat
import Web.Skype.Protocol.User

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Encoding as T

-- | GET CHAT <chat_id> RECENTCHATMESSAGES
getRecentChatMessages :: (MonadIO m, MonadSkype m)
                      => ChatID
                      -> m [ChatMessageID]
getRecentChatMessages chatID =
  handleCommandWithID command $ \response ->
    case response of
      Chat _ (ChatRecentMessages chatMessageIDs) -> Just chatMessageIDs
      _                                          -> Nothing
  where
    command = "GET CHAT " <> chatID <> " RECENTCHATMESSAGES"

-- | GET CHATMESSAGE <id> TIMESTAMP
getChatMessageTimestamp :: (MonadIO m, MonadSkype m)
                        => ChatMessageID
                        -> m EpochTime
getChatMessageTimestamp messageID =
  handleCommandWithID command $ \response ->
    case response of
      ChatMessage _ (ChatMessageTimestamp timestamp) -> Just timestamp
      _                                              -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show messageID)
                                 <> " TIMESTAMP"

-- | GET CHATMESSAGE <id> FROM_HANDLE
getChatMessageFromHandle :: (MonadIO m, MonadSkype m)
                         => ChatMessageID
                         -> m UserHandle
getChatMessageFromHandle chatMessageID =
  handleCommandWithID command $ \response ->
    case response of
      ChatMessage _ (ChatMessageFromHandle userHandle) -> Just userHandle
      _                                                -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " FROM_HANDLE"

-- | GET CHATMESSAGE <id> FROM_DISPNAME
getChatMessageFromDisplayName :: (MonadIO m, MonadSkype m)
                              => ChatMessageID
                              -> m UserHandle
getChatMessageFromDisplayName chatMessageID =
  handleCommandWithID command $ \response ->
    case response of
      ChatMessage _ (ChatMessageFromDisplayName userHandle) -> Just userHandle
      _                                                     -> Nothing

  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " FROM_DISPNAME"

-- | GET CHATMESSAGE <id> TYPE
getChatMessageType :: (MonadIO m, MonadSkype m)
                   => ChatMessageID
                   -> m ChatMessageType
getChatMessageType chatMessageID =
  handleCommandWithID command $ \response ->
    case response of
      ChatMessage _ (ChatMessageType messageType) -> Just messageType
      _                                           -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID) <> " TYPE"

-- | GET CHATMESSAGE <id> STATUS
getChatMessageStatus :: (MonadIO m, MonadSkype m)
                     => ChatMessageID
                     -> m ChatMessageStatus
getChatMessageStatus chatMessageID =
  handleCommandWithID command $ \response ->
    case response of
      ChatMessage _ (ChatMessageStatus messageStatus) -> Just messageStatus
      _                                               -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " STATUS"

-- | GET CHATMESSAGE <id> LEAVEREASON
getChatMessageLeaveReason :: (MonadIO m, MonadSkype m)
                          => ChatMessageID
                          -> m ChatMessageLeaveReason
getChatMessageLeaveReason chatMessageID =
  handleCommandWithID command $ \response ->
    case response of
      ChatMessage _ (ChatMessageLeaveReason leaveReason) -> Just leaveReason
      _                                                  -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " LEAVEREASON"

-- | GET CHATMESSAGE <id> CHATNAME
getChatMessageChatName :: (MonadIO m, MonadSkype m)
                       => ChatMessageID
                       -> m ChatID
getChatMessageChatName chatMessageID =
  handleCommandWithID command $ \response ->
    case response of
      ChatMessage _ (ChatMessageChatName chatID) -> Just chatID
      _                                          -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID)
                                 <> " CHATNAME"

-- | GET CHATMESSAGE <id> BODY
getChatMessageBody :: (MonadIO m, MonadSkype m)
                   => ChatMessageID
                   -> m ChatMessageBody
getChatMessageBody chatMessageID =
  handleCommandWithID command $ \response ->
    case response of
      ChatMessage _ (ChatMessageBody messageBody) -> Just messageBody
      _                                           -> Nothing
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID) <> " BODY"

-- | CHATMESSAGE <chat_id> <message>
sendChatMessage :: (MonadIO m, MonadSkype m)
                => ChatID
                -> ChatMessageBody
                -> m ChatMessageID
sendChatMessage chatID messageBody =
  handleCommandWithID command $ \response ->
    case response of
      ChatMessage chatMessageID _ -> Just chatMessageID
      _                           -> Nothing
  where
    command = "CHATMESSAGE " <> chatID <> " " <> T.encodeUtf8 messageBody

-- | ALTER CHAT <chat_id> LEAVE
leaveChat :: (MonadIO m, MonadSkype m)
          => ChatID
          -> m ()
leaveChat chatID = handleCommand command $ \response ->
  case response of
    "ALTER CHAT LEAVE" -> Just ()
    _                  -> Nothing
  where
    command = "ALTER CHAT " <> chatID <> " LEAVE"
