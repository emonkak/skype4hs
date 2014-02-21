module Web.Skype.Command.ChatMember where

import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Monoid ((<>))
import Web.Skype.Command.Utils
import Web.Skype.Core
import Web.Skype.Protocol

import qualified Data.ByteString.Char8 as BC

getAllMembers :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
              => ChatID
              -> SkypeT m [ChatMemberID]
getAllMembers chatID = executeCommandWithID command $ \response ->
  case response of
    Chat _ (ChatMemberObjects chatMemberIDs) -> return $ Just chatMemberIDs
    _                                        -> return Nothing
  where
    command = "GET CHAT " <> chatID <> " MEMBEROBJECTS"

getUserID :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
          => ChatMemberID
          -> SkypeT m UserID
getUserID chatMemberID = executeCommandWithID command $ \response ->
  case response of
    ChatMember _ (ChatMemberIdentity userID) -> return $ Just userID
    _                                        -> return Nothing
  where
    command = "GET CHATMEMBER " <> (BC.pack $ show chatMemberID) <> " IDENTITY"

getChatID :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
            => ChatMemberID
            -> SkypeT m ChatID
getChatID chatMemberID = executeCommandWithID command $ \response ->
  case response of
    ChatMember _ (ChatMemberChatName chatID) -> return $ Just chatID
    _                                        -> return Nothing
  where
    command = "GET CHATMEMBER " <> (BC.pack $ show chatMemberID) <> " CHATNAME"

getRole :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
        => ChatMemberID
        -> SkypeT m ChatRole
getRole chatMemberID = executeCommandWithID command $ \response ->
  case response of
    ChatMember _ (ChatMemberRole role) -> return $ Just role
    _                                  -> return Nothing
  where
    command = "GET CHATMEMBER " <> (BC.pack $ show chatMemberID) <> " ROLE"

isActive :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
         => ChatMemberID
         -> SkypeT m Bool
isActive chatMemberID = executeCommandWithID command $ \response ->
  case response of
    ChatMember _ (ChatMemberIsActive active) -> return $ Just active
    _                                        -> return Nothing
  where
    command = "GET CHATMEMBER " <> (BC.pack $ show chatMemberID) <> " IS_ACTIVE"
