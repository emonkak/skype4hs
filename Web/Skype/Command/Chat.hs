module Web.Skype.Command.Chat where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadIO)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import System.Posix.Types (EpochTime)
import Web.Skype.Command.Internal
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
getRecentChatMessages chatID = handleCommandWithID command handler >>=
                               return . fromMaybe []
  where
    command = "GET CHAT " <> chatID <> " RECENTCHATMESSAGES"

    handler (Chat _ (ChatRecentMessages chatMessageIDs)) = Just chatMessageIDs
    handler _ = Nothing

-- | GET CHATMESSAGE <id> TIMESTAMP
getChatMessageTimestamp :: (MonadIO m, MonadSkype m)
                        => ChatMessageID
                        -> m (Maybe EpochTime)
getChatMessageTimestamp messageID = handleCommandWithID command handler
  where
    command = "GET CHATMESSAGE " <> BC.pack (show messageID) <> " TIMESTAMP"

    handler (ChatMessage _ (ChatMessageTimestamp timestamp)) = Just timestamp
    handler _ = Nothing

-- | GET CHATMESSAGE <id> FROM_HANDLE
getChatMessageFromHandle :: (MonadIO m, MonadSkype m)
                         => ChatMessageID
                         -> m (Maybe UserHandle)
getChatMessageFromHandle chatMessageID = handleCommandWithID command handler
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID) <> " FROM_HANDLE"

    handler (ChatMessage _ (ChatMessageFromHandle userHandle)) = Just userHandle
    handler _ = Nothing

-- | GET CHATMESSAGE <id> FROM_DISPNAME
getChatMessageFromDisplayName :: (MonadIO m, MonadSkype m)
                              => ChatMessageID
                              -> m (Maybe UserHandle)
getChatMessageFromDisplayName chatMessageID = handleCommandWithID command handler
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID) <> " FROM_DISPNAME"

    handler (ChatMessage _ (ChatMessageFromDisplayName userHandle)) = Just userHandle
    handler _ = Nothing

-- | GET CHATMESSAGE <id> TYPE
getChatMessageType :: (MonadIO m, MonadSkype m)
                   => ChatMessageID
                   -> m (Maybe ChatMessageType)
getChatMessageType chatMessageID = handleCommandWithID command handler
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID) <> " TYPE"

    handler (ChatMessage _ (ChatMessageType messageType)) = Just messageType
    handler _ = Nothing

-- | GET CHATMESSAGE <id> STATUS
getChatMessageStatus :: (MonadIO m, MonadSkype m)
                     => ChatMessageID
                     -> m (Maybe ChatMessageStatus)
getChatMessageStatus chatMessageID = handleCommandWithID command handler
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID) <> " STATUS"

    handler (ChatMessage _ (ChatMessageStatus messageStatus)) = Just messageStatus
    handler _ = Nothing

-- | GET CHATMESSAGE <id> LEAVEREASON
getChatMessageLeaveReason :: (MonadIO m, MonadSkype m)
                          => ChatMessageID
                          -> m (Maybe ChatMessageLeaveReason)
getChatMessageLeaveReason chatMessageID = handleCommandWithID command handler
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID) <> " LEAVEREASON"

    handler (ChatMessage _ (ChatMessageLeaveReason leaveReason)) = Just leaveReason
    handler _ = Nothing

-- | GET CHATMESSAGE <id> CHATNAME
getChatMessageChatName :: (MonadIO m, MonadSkype m)
                       => ChatMessageID
                       -> m (Maybe ChatID)
getChatMessageChatName chatMessageID = handleCommandWithID command handler
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID) <> " CHATNAME"

    handler (ChatMessage _ (ChatMessageChatName chatID)) = Just chatID
    handler _ = Nothing

-- | GET CHATMESSAGE <id> BODY
getChatMessageBody :: (MonadIO m, MonadSkype m)
                   => ChatMessageID
                   -> m (Maybe ChatMessageBody)
getChatMessageBody chatMessageID = handleCommandWithID command handler
  where
    command = "GET CHATMESSAGE " <> BC.pack (show chatMessageID) <> " BODY"

    handler (ChatMessage _ (ChatMessageBody body)) = Just body
    handler _ = Nothing

-- | CHATMESSAGE <chat_id> <message>
sendMessage :: (MonadIO m, MonadSkype m)
            => ChatID
            -> ChatMessageBody
            -> m (Maybe ChatMessageID)
sendMessage chatID messageBody = handleCommandWithID command handler
  where
    command = "CHATMESSAGE " <> chatID <> " " <> T.encodeUtf8 messageBody

    handler (ChatMessage chatMessageID _) = Just chatMessageID
    handler _ = Nothing
