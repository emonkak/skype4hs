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

import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Monoid ((<>))
import Web.Skype.Command.Utils
import Web.Skype.Core
import Web.Skype.Protocol

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Encoding as T

-- | Changes a chat topic.
setTopic :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatID -> ChatTopic -> SkypeT m ()
setTopic chatID chatTopic = executeCommandWithID command $ \response ->
  case response of
    AlterChat AlterChatSetTopic -> return $ Just ()
    _                           -> return Nothing
  where
    command = "ALTER CHAT " <> chatID
                            <> " SETTOPIC "
                            <> T.encodeUtf8 chatTopic

-- | Adds new members to a chat.
addMembers :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatID -> [UserID] -> SkypeT m ()
addMembers _ [] = return ()
addMembers chatID userIDs = executeCommandWithID command $ \response ->
  case response of
    AlterChat AlterChatAddMembers -> return $ Just ()
    _                             -> return Nothing
  where
    command = "ALTER CHAT " <> chatID
                            <> " ADDMEMBERS "
                            <> BC.intercalate ", " userIDs

-- | Joins to a chat.
joinChat :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatID -> SkypeT m ()
joinChat chatID = executeCommandWithID command $ \response ->
  case response of
    AlterChat AlterChatJoin -> return $ Just ()
    _                       -> return Nothing
  where
    command = "ALTER CHAT " <> chatID <> " JOIN"

-- | Leaves to a chat.
leaveChat :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatID -> SkypeT m ()
leaveChat chatID = executeCommandWithID command $ \response ->
  case response of
    AlterChat AlterChatLeave -> return $ Just ()
    _                        -> return Nothing
  where
    command = "ALTER CHAT " <> chatID <> " LEAVE"

-- | Sends a message to this chat.
sendMessage :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
            => ChatID
            -> ChatMessageBody
            -> SkypeT m ChatMessageID
sendMessage chatID messageBody = executeCommandWithID command $ \response ->
  case response of
    ChatMessage chatMessageID _ -> return $ Just chatMessageID
    _                           -> return Nothing
  where
    command = "CHATMESSAGE " <> chatID <> " " <> T.encodeUtf8 messageBody

-- | Returns the timestamp of this chat.
getTimestamp :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatID -> SkypeT m Timestamp
getTimestamp chatID = executeCommandWithID command $ \response ->
  case response of
    Chat _ (ChatTimestamp timestamp) -> return $ Just timestamp
    _                                -> return Nothing
  where
    command = "GET CHAT " <> chatID <> " TIMESTAMP"

-- | Returns the user who added the current user to chat.
getAdder :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatID -> SkypeT m UserID
getAdder chatID = executeCommandWithID command $ \response ->
  case response of
    Chat _ (ChatAdder adder) -> return $ Just adder
    _                        -> return Nothing
  where
    command = "GET CHAT " <> chatID <> " ADDER"

-- | Returns the chat status.
getStatus :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatID -> SkypeT m ChatStatus
getStatus chatID = executeCommandWithID command $ \response ->
  case response of
    Chat _ (ChatStatus status) -> return $ Just status
    _                          -> return Nothing
  where
    command = "GET CHAT " <> chatID <> " STATUS"

-- | Returns the name shown in chat window title.
getAllPosters :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatID -> SkypeT m [UserID]
getAllPosters chatID = executeCommandWithID command $ \response ->
  case response of
    Chat _ (ChatPosters posters) -> return $ Just posters
    _                            -> return Nothing
  where
    command = "GET CHAT " <> chatID <> " POSTERS"

-- | Returns all users who have been there.
getAllMembers :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatID -> SkypeT m [UserID]
getAllMembers chatID = executeCommandWithID command $ \response ->
  case response of
    Chat _ (ChatMembers members) -> return $ Just members
    _                            -> return Nothing
  where
    command = "GET CHAT " <> chatID <> " MEMBERS"

-- | Returns the chat topic.
getTopic :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatID -> SkypeT m ChatTopic
getTopic chatID = executeCommandWithID command $ \response ->
  case response of
    Chat _ (ChatTopic topic) -> return $ Just topic
    _                        -> return Nothing
  where
    command = "GET CHAT " <> chatID <> " TOPIC"

-- | Returns the members who have stayed in chat.
getActiveMembers :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatID -> SkypeT m [UserID]
getActiveMembers chatID = executeCommandWithID command $ \response ->
  case response of
    Chat _ (ChatActiveMembers members) -> return $ Just members
    _                                  -> return Nothing
  where
    command = "GET CHAT " <> chatID <> " ACTIVEMEMBERS"

-- | Returns the chat window title.
getWindowTitle :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatID -> SkypeT m ChatWindowTitle
getWindowTitle chatID = executeCommandWithID command $ \response ->
  case response of
    Chat _ (ChatFriendyName name) -> return $ Just name
    _                             -> return Nothing
  where
    command = "GET CHAT " <> chatID <> " FRIENDLYNAME"

-- | Returns all messages in this chat.
getAllMessages :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatID -> SkypeT m [ChatMessageID]
getAllMessages chatID = executeCommandWithID command $ \response ->
  case response of
    Chat _ (ChatMessages chatMessageIDs) -> return $ Just chatMessageIDs
    _                                    -> return Nothing
  where
    command = "GET CHAT " <> chatID <> " CHATMESSAGES"

-- | Returns recent messages in this chat.
getRecentMessages :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatID -> SkypeT m [ChatMessageID]
getRecentMessages chatID = executeCommandWithID command $ \response ->
  case response of
    Chat _ (ChatRecentMessages chatMessageIDs) -> return $ Just chatMessageIDs
    _                                          -> return Nothing
  where
    command = "GET CHAT " <> chatID <> " RECENTCHATMESSAGES"

-- | Indicates if this chat has been bookmarked.
isBookmarked :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatID -> SkypeT m Bool
isBookmarked chatID = executeCommandWithID command $ \response ->
  case response of
    Chat _ (ChatBookmarked isbookmarked) -> return $ Just isbookmarked
    _                                    -> return Nothing
  where
    command = "GET CHAT " <> chatID <> " BOOKMARKED"

-- | Create a chat.
createChat :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => [UserID] -> SkypeT m (ChatID, ChatStatus)
createChat userIDs = executeCommandWithID command $ \response ->
  case response of
    Chat chatID (ChatStatus status) -> return $ Just (chatID, status)
    _                               -> return Nothing
  where
    command = "CAHT CREATE " <> BC.intercalate ", " userIDs

-- | Open a chat window.
openChat :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => ChatID -> SkypeT m ()
openChat chatID = executeCommandWithID command $ \response ->
  case response of
    OpenChat _ -> return $ Just ()
    _          -> return Nothing
  where
    command = "OPEN CHAT " <> chatID

-- | Returns a list of chat IDs.
searchAllChats :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => SkypeT m [ChatID]
searchAllChats = searchChats "SEARCH CHATS"

-- | Returns a list of chat IDs that are open in the window.
searchActiveChats :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => SkypeT m [ChatID]
searchActiveChats = searchChats "SEARCH ACTIVECHATS"

-- | Returns a list of chat IDs that include unread messages.
searchMissedChats :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => SkypeT m [ChatID]
searchMissedChats = searchChats "SEARCH MISSEDCHATS"

-- | Returns a list of recent chat IDs.
searchRecentChats :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => SkypeT m [ChatID]
searchRecentChats = searchChats "SEARCH RECENTCHATS"

searchChats :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => Command -> SkypeT m [ChatID]
searchChats command = executeCommandWithID command $ \response ->
  case response of
    Chats chatIDs          -> return $ Just chatIDs
    _                      -> return Nothing
