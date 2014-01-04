module Web.Skype.Command.Chat where

import Control.Monad.Trans (MonadIO)
import Data.Monoid ((<>))
import Web.Skype.Command.Utils
import Web.Skype.Core
import Web.Skype.Parser
import Web.Skype.Protocol

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Encoding as T

-- | Changes a chat topic.
setChatTopic :: (MonadIO m, MonadSkype m) => ChatID -> ChatTopic -> m ()
setChatTopic (ChatID chatID) chatTopic = executeCommandWithID command $ \response ->
  case response of
    AlterChatSetTopic              -> Just $ Right ()
    ErrorResponse code description -> Just $ Left $ SkypeError code command description
    _                              -> Nothing
  where
    command = "ALTER CHAT " <> chatID
                            <> " SETTOPIC "
                            <> T.encodeUtf8 chatTopic

-- | Adds new members to a chat.
addChatMembers :: (MonadIO m, MonadSkype m) => ChatID -> [UserID] -> m ()
addChatMembers (ChatID chatID) []      = return ()
addChatMembers (ChatID chatID) userIDs = executeCommandWithID command $ \response ->
  case response of
    AlterChatAddMembers            -> Just $ Right ()
    ErrorResponse code description -> Just $ Left $ SkypeError code command description
    _                              -> Nothing
  where
    command = "ALTER CHAT " <> chatID
                            <> " ADDMEMBERS "
                            <> BC.intercalate ", " (map getUserID userIDs)

-- | Joins to a chat.
joinChat :: (MonadIO m, MonadSkype m) => ChatID -> m ()
joinChat (ChatID chatID) = executeCommandWithID command $ \response ->
  case response of
    AlterChatJoin                  -> Just $ Right ()
    ErrorResponse code description -> Just $ Left $ SkypeError code command description
    _                              -> Nothing
  where
    command = "ALTER CHAT " <> chatID <> " JOIN"

-- | Leaves to a chat.
leaveChat :: (MonadIO m, MonadSkype m) => ChatID -> m ()
leaveChat (ChatID chatID) = executeCommandWithID command $ \response ->
  case response of
    AlterChatLeave                 -> Just $ Right ()
    ErrorResponse code description -> Just $ Left $ SkypeError code command description
    _                              -> Nothing
  where
    command = "ALTER CHAT " <> chatID <> " LEAVE"

-- | Sends a message to this chat.
sendChatMessage :: (MonadIO m, MonadSkype m)
                => ChatID
                -> ChatMessageBody
                -> m ChatMessageID
sendChatMessage (ChatID chatID) messageBody = executeCommandWithID command $ \response ->
  case response of
    ChatMessageResponse chatMessageID _ -> Just $ Right chatMessageID
    ErrorResponse code description      -> Just $ Left $ SkypeError code command description
    _                                   -> Nothing
  where
    command = "CHATMESSAGE " <> chatID <> " " <> T.encodeUtf8 messageBody

-- | Returns the timestamp of this chat.
getTimestamp :: (MonadIO m, MonadSkype m) => ChatID -> m Timestamp
getTimestamp (ChatID chatID) = executeCommandWithID command $ \response ->
  case response of
    ChatResponse _ (ChatTimestamp timestamp) -> Just $ Right timestamp
    ErrorResponse code description           -> Just $ Left $ SkypeError code command description
    _                                        -> Nothing
  where
    command = "GET CHAT " <> chatID <> " TIMESTAMP"

-- | Returns the user who added the current user to chat.
getAdder :: (MonadIO m, MonadSkype m) => ChatID -> m UserID
getAdder (ChatID chatID) = executeCommandWithID command $ \response ->
  case response of
    ChatResponse _ (ChatAdder adder) -> Just $ Right adder
    ErrorResponse code description   -> Just $ Left $ SkypeError code command description
    _                                -> Nothing
  where
    command = "GET CHAT " <> chatID <> " ADDER"

-- | Returns the chat status.
getStatus :: (MonadIO m, MonadSkype m) => ChatID -> m ChatStatus
getStatus (ChatID chatID) = executeCommandWithID command $ \response ->
  case response of
    ChatResponse _ (ChatStatus status) -> Just $ Right status
    ErrorResponse code description     -> Just $ Left $ SkypeError code command description
    _                                  -> Nothing
  where
    command = "GET CHAT " <> chatID <> " STATUS"

-- | Returns the name shown in chat window title.
getAllPosters :: (MonadIO m, MonadSkype m) => ChatID -> m [UserID]
getAllPosters (ChatID chatID) = executeCommandWithID command $ \response ->
  case response of
    ChatResponse _ (ChatPosters posters) -> Just $ Right posters
    ErrorResponse code description       -> Just $ Left $ SkypeError code command description
    _                                    -> Nothing
  where
    command = "GET CHAT " <> chatID <> " POSTERS"

-- | Returns all users who have been there.
getAllMembers :: (MonadIO m, MonadSkype m) => ChatID -> m [UserID]
getAllMembers (ChatID chatID) = executeCommandWithID command $ \response ->
  case response of
    ChatResponse _ (ChatMembers members) -> Just $ Right members
    ErrorResponse code description       -> Just $ Left $ SkypeError code command description
    _                                    -> Nothing
  where
    command = "GET CHAT " <> chatID <> " MEMBERS"

-- | Returns the chat topic.
getTopic :: (MonadIO m, MonadSkype m) => ChatID -> m ChatTopic
getTopic (ChatID chatID) = executeCommandWithID command $ \response ->
  case response of
    ChatResponse _ (ChatTopic topic) -> Just $ Right topic
    ErrorResponse code description   -> Just $ Left $ SkypeError code command description
    _                                -> Nothing
  where
    command = "GET CHAT " <> chatID <> " TOPIC"

-- | Returns the members who have stayed in chat.
getActiveMembers :: (MonadIO m, MonadSkype m) => ChatID -> m [UserID]
getActiveMembers (ChatID chatID) = executeCommandWithID command $ \response ->
  case response of
    ChatResponse _ (ChatActiveMembers members) -> Just $ Right members
    ErrorResponse code description             -> Just $ Left $ SkypeError code command description
    _                                          -> Nothing
  where
    command = "GET CHAT " <> chatID <> " ACTIVEMEMBERS"

-- | Returns the chat window title.
getWindowTitle :: (MonadIO m, MonadSkype m) => ChatID -> m ChatWindowTitle
getWindowTitle (ChatID chatID) = executeCommandWithID command $ \response ->
  case response of
    ChatResponse _ (ChatFriendyName name) -> Just $ Right name
    ErrorResponse code description        -> Just $ Left $ SkypeError code command description
    _                                     -> Nothing
  where
    command = "GET CHAT " <> chatID <> " FRIENDLYNAME"

-- | Returns all messages in this chat.
getAllMessages :: (MonadIO m, MonadSkype m) => ChatID -> m [ChatMessageID]
getAllMessages (ChatID chatID) = executeCommandWithID command $ \response ->
  case response of
    ChatResponse _ (ChatMessages chatMessageIDs) -> Just $ Right chatMessageIDs
    ErrorResponse code description               -> Just $ Left $ SkypeError code command description
    _                                            -> Nothing
  where
    command = "GET CHAT " <> chatID <> " CHATMESSAGES"

-- | Returns recent messages in this chat.
getRecentMessages :: (MonadIO m, MonadSkype m) => ChatID -> m [ChatMessageID]
getRecentMessages (ChatID chatID) = executeCommandWithID command $ \response ->
  case response of
    ChatResponse _ (ChatRecentMessages chatMessageIDs) -> Just $ Right chatMessageIDs
    ErrorResponse code description                     -> Just $ Left $ SkypeError code command description
    _                                                  -> Nothing
  where
    command = "GET CHAT " <> chatID <> " RECENTCHATMESSAGES"

-- | Indicates if this chat has been bookmarked.
isBookmarked :: (MonadIO m, MonadSkype m) => ChatID -> m Bool
isBookmarked (ChatID chatID) = executeCommandWithID command $ \response ->
  case response of
    ChatResponse _ (ChatBookmarked pred) -> Just $ Right pred
    ErrorResponse code description       -> Just $ Left $ SkypeError code command description
    _                                    -> Nothing
  where
    command = "GET CHAT " <> chatID <> " BOOKMARKED"
