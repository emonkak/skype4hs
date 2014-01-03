module Web.Skype.Command.Chat where

import Control.Monad.Trans (MonadIO)
import Data.Monoid ((<>))
import Web.Skype.Command.Utils
import Web.Skype.Core
import Web.Skype.Parser
import Web.Skype.Protocol.Chat
import Web.Skype.Protocol.Types

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Encoding as T

-- | Changes a chat topic.
setChatTopic :: (MonadIO m, MonadSkype m) => ChatID -> ChatTopic -> m ()
setChatTopic chatID chatTopic = handleCommandWithID command $ \response ->
  case response of
    AlterChatSetTopic   -> Just $ Right ()
    ErrorResponse error -> Just $ Left error
    _                   -> Nothing
  where
    command = "ALTER CHAT " <> chatID
                            <> " SETTOPIC "
                            <> T.encodeUtf8 chatTopic

-- | Adds new members to a chat.
addChatMembers :: (MonadIO m, MonadSkype m) => ChatID -> [UserID] -> m ()
addChatMembers chatID []      = return ()
addChatMembers chatID userIDs = handleCommandWithID command $ \response ->
  case response of
    AlterChatAddMembers -> Just $ Right ()
    ErrorResponse error -> Just $ Left error
    _                   -> Nothing
  where
    command = "ALTER CHAT " <> chatID
                            <> " ADDMEMBERS "
                            <> BC.intercalate ", " userIDs

-- | Joins to a chat.
joinChat :: (MonadIO m, MonadSkype m) => ChatID -> m ()
joinChat chatID = handleCommandWithID command $ \response ->
  case response of
    AlterChatJoin       -> Just $ Right ()
    ErrorResponse error -> Just $ Left error
    _                   -> Nothing
  where
    command = "GET CHAT " <> chatID <> " RECENTCHATMESSAGES"

-- | Leaves to a chat.
leaveChat :: (MonadIO m, MonadSkype m) => ChatID -> m ()
leaveChat chatID = handleCommandWithID command $ \response ->
  case response of
    AlterChatLeave      -> Just $ Right ()
    ErrorResponse erorr -> Just $ Left erorr
    _                   -> Nothing
  where
    command = "ALTER CHAT " <> chatID <> " LEAVE"

-- | Returns all messages in this chat.
getAllChatMessages :: (MonadIO m, MonadSkype m) => ChatID -> m [ChatMessageID]
getAllChatMessages chatID = handleCommandWithID command $ \response ->
  case response of
    ChatResponse _ (ChatMessages chatMessageIDs) -> Just $ Right chatMessageIDs
    ErrorResponse error                          -> Just $ Left error
    _                                            -> Nothing
  where
    command = "GET CHAT " <> chatID <> " CHATMESSAGES"

-- | Returns recent messages in this chat.
getRecentChatMessages :: (MonadIO m, MonadSkype m) => ChatID -> m [ChatMessageID]
getRecentChatMessages chatID = handleCommandWithID command $ \response ->
  case response of
    ChatResponse _ (ChatRecentMessages chatMessageIDs) -> Just $ Right chatMessageIDs
    ErrorResponse error                                -> Just $ Left error
    _                                                  -> Nothing
  where
    command = "GET CHAT " <> chatID <> " RECENTCHATMESSAGES"

-- | Sends a message to this chat.
sendChatMessage :: (MonadIO m, MonadSkype m)
                => ChatID
                -> ChatMessageBody
                -> m ChatMessageID
sendChatMessage chatID messageBody =
  handleCommandWithID command $ \response ->
    case response of
      ChatMessageResponse chatMessageID _ -> Just $ Right chatMessageID
      ErrorResponse error                 -> Just $ Left error
      _                                   -> Nothing
  where
    command = "CHATMESSAGE " <> chatID <> " " <> T.encodeUtf8 messageBody
