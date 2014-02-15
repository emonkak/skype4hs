module Web.Skype.Protocol (
  parseNotification,
  parseCommandResponse,

  SkypeNotification(..),

  AlterChatProperty(..),
  ChatID,
  ChatTopic,
  ChatWindowTitle,
  ChatPasswordHint,
  ChatGuidelines,
  ChatDescription,
  ChatBlob,
  ChatProperty(..),
  ChatStatus(..),
  ChatOption,
  ChatType(..),
  ChatMyStatus(..),
  ChatRole(..),
  chatOptionJoiningEnabled,
  chatOptionJoinersBecomeApplicants,
  chatOptionJoinersBecomeListeners,
  chatOptionHistoryDisclosed,
  chatOptionUsersAreListeners,
  chatOptionTopicAndPicLockedForUsers,

  ChatMemberID,
  ChatMemberProperty(..),

  ChatMessageID,
  ChatMessageBody,
  ChatMessageProperty(..),
  ChatMessageType(..),
  ChatMessageStatus(..),
  ChatMessageLeaveReason(..),

  ConnectionStatus(..),

  UserID,
  UserHandle,
  UserFullName,
  UserDisplayName,
  UserBirthday,
  UserLanguage,
  UserLanguagePrefix,
  UserCountry,
  UserCountryPrefix,
  UserProvince,
  UserCity,
  UserPhone,
  UserAbout,
  UserHomepage,
  UserSpeedDial,
  UserAuthRequestMessage,
  UserMoodText,
  UserRichMoodText,
  UserTimezoneOffset,
  UserProperty(..),
  UserSex(..),
  UserStatus(..),
  UserBuddyStatus(..),
  UserOnlineStatus(..),

  Timestamp,
  ErrorCode,
  ErrorDescription,
  ProtocolVersion
) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.Attoparsec.ByteString.Lazy
import Data.Word8
import Web.Skype.Core
import Web.Skype.Protocol.Chat
import Web.Skype.Protocol.ChatMember
import Web.Skype.Protocol.ChatMessage
import Web.Skype.Protocol.Misc
import Web.Skype.Protocol.Types
import Web.Skype.Protocol.User

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL

data SkypeNotification = AlterChat AlterChatProperty
                       | Chat ChatID ChatProperty
                       | Chats [ChatID]
                       | ChatMessage ChatMessageID ChatMessageProperty
                       | ConnectionStatus ConnectionStatus
                       | Error ErrorCode ErrorDescription
                       | OpenChat ChatID
                       | Protocol ProtocolVersion
                       | User UserID UserProperty
                       | UserStatus UserStatus
                       | CurrentUserHandle UserID
  deriving (Eq, Show)

parseNotification :: BL.ByteString -> Maybe SkypeNotification
parseNotification = maybeResult . parse p_notification

parseCommandResponse :: BL.ByteString -> Maybe (CommandID, SkypeNotification)
parseCommandResponse = maybeResult . parse p_notificationWithCommandID

p_notification :: Parser SkypeNotification
p_notification = choice
  [ p_alterNotification
  , p_chatsNotification
  , p_chatNotification
  , p_chatMessageNotification
  , p_connectionStatusNotification
  , p_errorNotification
  , p_openNotification
  , p_protocolNotification
  , p_userNotification
  , p_userStatusNotification
  , p_currentUserHandleNotification
  ]

p_notificationWithCommandID :: Parser (CommandID, SkypeNotification)
p_notificationWithCommandID = (,) <$> (p_commandID <* spaces) <*> p_notification
  where
    p_commandID = word8 _numbersign *> takeWhile1 (not . isSpace)

-- | ALTER
p_alterNotification :: Parser SkypeNotification
p_alterNotification = string "ALTER" *> spaces *> choice
  [ AlterChat <$> (string "CHAT" *> spaces *> p_alterChatProperties)
  ]

-- | CHAT
p_chatNotification :: Parser SkypeNotification
p_chatNotification =
  string "CHAT" *> spaces
                *> (Chat <$> (p_chatID <* spaces) <*> p_chatProperty)

-- | CHATS
p_chatsNotification :: Parser SkypeNotification
p_chatsNotification = Chats <$> (string "CHATS" *> spaces *> p_chatIDs)
  where
    p_chatIDs = p_chatID `sepBy` (word8 _comma *> spaces)

-- | CHATMESSAGE
p_chatMessageNotification :: Parser SkypeNotification
p_chatMessageNotification =
    string "CHATMESSAGE" *> spaces
                         *> (ChatMessage <$> (p_chatMessageID <* spaces) <*> p_chatMessageProperty)

-- | CONNSTATUS
p_connectionStatusNotification :: Parser SkypeNotification
p_connectionStatusNotification = ConnectionStatus <$> (string "CONNSTATUS" *> spaces *> p_status)
  where
    p_status = choice
      [ ConnectionStatusOffline    <$ string "OFFLINE"
      , ConnectionStatusConnecting <$ string "CONNECTING"
      , ConnectionStatusPausing    <$ string "PAUSING"
      , ConnectionStatusOnline     <$ string "ONLINE" ]

-- | ERROR
p_errorNotification :: Parser SkypeNotification
p_errorNotification = Error <$> p_code <*> (p_description <|> pure "")
  where
    p_code = string "ERROR" *> spaces *> decimal

    p_description = spaces *> (T.decodeUtf8 <$> takeByteString)

-- | OPEN
p_openNotification :: Parser SkypeNotification
p_openNotification = string "OPEN" *> spaces *> choice
  [ p_chat ]
  where
    p_chat = OpenChat <$> (string "CHAT" *> spaces *> p_chatID)

-- | PROTOCOL
p_protocolNotification :: Parser SkypeNotification
p_protocolNotification = Protocol <$> (string "PROTOCOL" *> spaces *> decimal)

-- | USER
p_userNotification :: Parser SkypeNotification
p_userNotification = User <$> (string "USER" *> spaces *> p_userID <* spaces)
                          <*> p_userProperty

-- | USER
p_userStatusNotification :: Parser SkypeNotification
p_userStatusNotification = UserStatus <$> (string "USERSTATUS" *> spaces *> p_userStatus)

-- | CURRENTUSERHANDLE
p_currentUserHandleNotification :: Parser SkypeNotification
p_currentUserHandleNotification = CurrentUserHandle <$>
                                  (string "CURRENTUSERHANDLE" *> spaces *> p_userID)
