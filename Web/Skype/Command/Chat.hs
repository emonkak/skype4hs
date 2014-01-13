module Web.Skype.Command.Chat (
  setTopic,
  addMembers,
  joinChat,
  leaveChat,
  sendMessage,
  getTimestamp,
  getAdder,
  getStatus,
  getAllPosters,
  getAllMembers,
  getTopic,
  getActiveMembers,
  getWindowTitle,
  getAllMessages,
  getRecentMessages,
  isBookmarked,
  createChat,
  openChat,
  searchAllChats,
  searchActiveChats,
  searchMissedChats,
  searchRecentChats
)
where

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
setTopic :: (MonadIO m, MonadSkype m) => ChatID -> ChatTopic -> SkypeT m ()
setTopic chatID chatTopic = executeCommandWithID command $ \response ->
  case response of
    AlterChatSetTopic      -> Just $ Right ()
    Error code description -> Just $ Left $ SkypeError code command description
    _                      -> Nothing
  where
    command = "ALTER CHAT " <> chatID
                            <> " SETTOPIC "
                            <> T.encodeUtf8 chatTopic

-- | Adds new members to a chat.
addMembers :: (MonadIO m, MonadSkype m) => ChatID -> [UserID] -> SkypeT m ()
addMembers _ [] = return ()
addMembers chatID userIDs = executeCommandWithID command $ \response ->
  case response of
    AlterChatAddMembers    -> Just $ Right ()
    Error code description -> Just $ Left $ SkypeError code command description
    _                      -> Nothing
  where
    command = "ALTER CHAT " <> chatID
                            <> " ADDMEMBERS "
                            <> BC.intercalate ", " userIDs

-- | Joins to a chat.
joinChat :: (MonadIO m, MonadSkype m) => ChatID -> SkypeT m ()
joinChat chatID = executeCommandWithID command $ \response ->
  case response of
    AlterChatJoin          -> Just $ Right ()
    Error code description -> Just $ Left $ SkypeError code command description
    _                      -> Nothing
  where
    command = "ALTER CHAT " <> chatID <> " JOIN"

-- | Leaves to a chat.
leaveChat :: (MonadIO m, MonadSkype m) => ChatID -> SkypeT m ()
leaveChat chatID = executeCommandWithID command $ \response ->
  case response of
    AlterChatLeave         -> Just $ Right ()
    Error code description -> Just $ Left $ SkypeError code command description
    _                      -> Nothing
  where
    command = "ALTER CHAT " <> chatID <> " LEAVE"

-- | Sends a message to this chat.
sendMessage :: (MonadIO m, MonadSkype m)
            => ChatID
            -> ChatMessageBody
            -> SkypeT m ChatMessageID
sendMessage chatID messageBody = executeCommandWithID command $ \response ->
  case response of
    ChatMessage chatMessageID _ -> Just $ Right chatMessageID
    Error code description      -> Just $ Left $ SkypeError code command description
    _                           -> Nothing
  where
    command = "CHATMESSAGE " <> chatID <> " " <> T.encodeUtf8 messageBody

-- | Returns the timestamp of this chat.
getTimestamp :: (MonadIO m, MonadSkype m) => ChatID -> SkypeT m Timestamp
getTimestamp chatID = executeCommandWithID command $ \response ->
  case response of
    Chat _ (ChatTimestamp timestamp) -> Just $ Right timestamp
    Error code description           -> Just $ Left $ SkypeError code command description
    _                                -> Nothing
  where
    command = "GET CHAT " <> chatID <> " TIMESTAMP"

-- | Returns the user who added the current user to chat.
getAdder :: (MonadIO m, MonadSkype m) => ChatID -> SkypeT m UserID
getAdder chatID = executeCommandWithID command $ \response ->
  case response of
    Chat _ (ChatAdder adder) -> Just $ Right adder
    Error code description   -> Just $ Left $ SkypeError code command description
    _                        -> Nothing
  where
    command = "GET CHAT " <> chatID <> " ADDER"

-- | Returns the chat status.
getStatus :: (MonadIO m, MonadSkype m) => ChatID -> SkypeT m ChatStatus
getStatus chatID = executeCommandWithID command $ \response ->
  case response of
    Chat _ (ChatStatus status) -> Just $ Right status
    Error code description     -> Just $ Left $ SkypeError code command description
    _                          -> Nothing
  where
    command = "GET CHAT " <> chatID <> " STATUS"

-- | Returns the name shown in chat window title.
getAllPosters :: (MonadIO m, MonadSkype m) => ChatID -> SkypeT m [UserID]
getAllPosters chatID = executeCommandWithID command $ \response ->
  case response of
    Chat _ (ChatPosters posters) -> Just $ Right posters
    Error code description       -> Just $ Left $ SkypeError code command description
    _                            -> Nothing
  where
    command = "GET CHAT " <> chatID <> " POSTERS"

-- | Returns all users who have been there.
getAllMembers :: (MonadIO m, MonadSkype m) => ChatID -> SkypeT m [UserID]
getAllMembers chatID = executeCommandWithID command $ \response ->
  case response of
    Chat _ (ChatMembers members) -> Just $ Right members
    Error code description       -> Just $ Left $ SkypeError code command description
    _                            -> Nothing
  where
    command = "GET CHAT " <> chatID <> " MEMBERS"

-- | Returns the chat topic.
getTopic :: (MonadIO m, MonadSkype m) => ChatID -> SkypeT m ChatTopic
getTopic chatID = executeCommandWithID command $ \response ->
  case response of
    Chat _ (ChatTopic topic) -> Just $ Right topic
    Error code description   -> Just $ Left $ SkypeError code command description
    _                        -> Nothing
  where
    command = "GET CHAT " <> chatID <> " TOPIC"

-- | Returns the members who have stayed in chat.
getActiveMembers :: (MonadIO m, MonadSkype m) => ChatID -> SkypeT m [UserID]
getActiveMembers chatID = executeCommandWithID command $ \response ->
  case response of
    Chat _ (ChatActiveMembers members) -> Just $ Right members
    Error code description             -> Just $ Left $ SkypeError code command description
    _                                  -> Nothing
  where
    command = "GET CHAT " <> chatID <> " ACTIVEMEMBERS"

-- | Returns the chat window title.
getWindowTitle :: (MonadIO m, MonadSkype m) => ChatID -> SkypeT m ChatWindowTitle
getWindowTitle chatID = executeCommandWithID command $ \response ->
  case response of
    Chat _ (ChatFriendyName name) -> Just $ Right name
    Error code description        -> Just $ Left $ SkypeError code command description
    _                             -> Nothing
  where
    command = "GET CHAT " <> chatID <> " FRIENDLYNAME"

-- | Returns all messages in this chat.
getAllMessages :: (MonadIO m, MonadSkype m) => ChatID -> SkypeT m [ChatMessageID]
getAllMessages chatID = executeCommandWithID command $ \response ->
  case response of
    Chat _ (ChatMessages chatMessageIDs) -> Just $ Right chatMessageIDs
    Error code description               -> Just $ Left $ SkypeError code command description
    _                                    -> Nothing
  where
    command = "GET CHAT " <> chatID <> " CHATMESSAGES"

-- | Returns recent messages in this chat.
getRecentMessages :: (MonadIO m, MonadSkype m) => ChatID -> SkypeT m [ChatMessageID]
getRecentMessages chatID = executeCommandWithID command $ \response ->
  case response of
    Chat _ (ChatRecentMessages chatMessageIDs) -> Just $ Right chatMessageIDs
    Error code description                     -> Just $ Left $ SkypeError code command description
    _                                          -> Nothing
  where
    command = "GET CHAT " <> chatID <> " RECENTCHATMESSAGES"

-- | Indicates if this chat has been bookmarked.
isBookmarked :: (MonadIO m, MonadSkype m) => ChatID -> SkypeT m Bool
isBookmarked chatID = executeCommandWithID command $ \response ->
  case response of
    Chat _ (ChatBookmarked pred) -> Just $ Right pred
    Error code description       -> Just $ Left $ SkypeError code command description
    _                            -> Nothing
  where
    command = "GET CHAT " <> chatID <> " BOOKMARKED"

-- | Create a chat.
createChat :: (MonadIO m, MonadSkype m) => [UserID] -> SkypeT m (ChatID, ChatStatus)
createChat userIDs = executeCommandWithID command $ \response ->
  case response of
    Chat chatID (ChatStatus status) -> Just $ Right (chatID, status)
    Error code description          -> Just $ Left $ SkypeError code command description
    _                               -> Nothing
  where
    command = "CAHT CREATE " <> BC.intercalate ", " userIDs

-- | Open a chat window.
openChat :: (MonadIO m, MonadSkype m) => ChatID -> SkypeT m ()
openChat chatID = executeCommandWithID command $ \response ->
  case response of
    OpenChat chatID        -> Just $ Right ()
    Error code description -> Just $ Left $ SkypeError code command description
    _                      -> Nothing
  where
    command = "OPEN CHAT " <> chatID

-- | Returns a list of chat IDs.
searchAllChats :: (MonadIO m, MonadSkype m) => SkypeT m [ChatID]
searchAllChats = searchChats "SEARCH CHATS"

-- | Returns a list of chat IDs that are open in the window.
searchActiveChats :: (MonadIO m, MonadSkype m) => SkypeT m [ChatID]
searchActiveChats = searchChats "SEARCH ACTIVECHATS"

-- | Returns a list of chat IDs that include unread messages.
searchMissedChats :: (MonadIO m, MonadSkype m) => SkypeT m [ChatID]
searchMissedChats = searchChats "SEARCH MISSEDCHATS"

-- | Returns a list of recent chat IDs.
searchRecentChats :: (MonadIO m, MonadSkype m) => SkypeT m [ChatID]
searchRecentChats = searchChats "SEARCH RECENTCHATS"

searchChats :: (MonadIO m, MonadSkype m) => Command -> SkypeT m [ChatID]
searchChats command = executeCommandWithID command $ \response ->
  case response of
    Chats chatIDs          -> Just $ Right chatIDs
    Error code description -> Just $ Left $ SkypeError code command description
    _                      -> Nothing
