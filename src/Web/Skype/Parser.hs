module Web.Skype.Parser where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.Attoparsec.ByteString.Lazy
import Data.Word8
import Web.Skype.Core
import Web.Skype.Parser.Chat
import Web.Skype.Parser.ChatMember
import Web.Skype.Parser.ChatMessage
import Web.Skype.Parser.Types
import Web.Skype.Parser.User
import Web.Skype.Protocol

import qualified Data.Text.Encoding as T

parseNotification :: Notification -> Result NotificationObject
parseNotification = parse notification

parseCommandID :: Notification -> Result CommandID
parseCommandID = parse commandID

notification :: Parser NotificationObject
notification = skipWhile isSpace *> choice
  [ alterNotification
  , chatsNotification
  , chatNotification
  , chatMemberNotification
  , chatMessageNotification
  , connectionStatusNotification
  , errorNotification
  , openNotification
  , protocolNotification
  , usersNotification
  , userNotification
  , userStatusNotification
  , currentUserHandleNotification
  ]

commandID :: Parser CommandID
commandID = word8 _numbersign *> takeWhile1 (not . isSpace)

-- | ALTER
alterNotification :: Parser NotificationObject
alterNotification = string "ALTER" *> spaces *> choice
  [ AlterChat <$> (string "CHAT" *> spaces *> alterChatProperties)
  ]

-- | CHAT
chatNotification :: Parser NotificationObject
chatNotification = string "CHAT"
                *> spaces
                *> (Chat <$> (chatID <* spaces) <*> chatProperty)

-- | CHATS
chatsNotification :: Parser NotificationObject
chatsNotification = Chats <$> (string "CHATS" *> spaces *> chatIDs)
  where
    chatIDs = chatID `sepBy` (word8 _comma *> spaces)

-- | CHATMEMEMBER
chatMemberNotification :: Parser NotificationObject
chatMemberNotification = string "CHATMEMEMBER"
                      *> spaces
                      *> (ChatMember <$> (chatMemberID <* spaces) <*> chatMemberProperty)

-- | CHATMESSAGE
chatMessageNotification :: Parser NotificationObject
chatMessageNotification = string "CHATMESSAGE"
                       *> spaces
                       *> (ChatMessage <$> (chatMessageID <* spaces) <*> chatMessageProperty)

-- | CONNSTATUS
connectionStatusNotification :: Parser NotificationObject
connectionStatusNotification = ConnectionStatus <$> (string "CONNSTATUS" *> spaces *> status)
  where
    status = choice
      [ ConnectionStatusOffline    <$ string "OFFLINE"
      , ConnectionStatusConnecting <$ string "CONNECTING"
      , ConnectionStatusPausing    <$ string "PAUSING"
      , ConnectionStatusOnline     <$ string "ONLINE"
      ]

-- | ERROR
errorNotification :: Parser NotificationObject
errorNotification = Error <$> code <*> (description <|> pure "")
  where
    code = string "ERROR" *> spaces *> decimal

    description = spaces *> (T.decodeUtf8 <$> takeByteString)

-- | OPEN
openNotification :: Parser NotificationObject
openNotification = string "OPEN" *> spaces *> choice
  [ chat ]
  where
    chat = OpenChat <$> (string "CHAT" *> spaces *> chatID)

-- | PROTOCOL
protocolNotification :: Parser NotificationObject
protocolNotification = Protocol <$> (string "PROTOCOL" *> spaces *> decimal)

-- | USERS
usersNotification :: Parser NotificationObject
usersNotification = Users <$> (string "USERS" *> spaces *> userIDs)
  where
    userIDs = userID `sepBy` (word8 _comma *> spaces)

-- | USER
userNotification :: Parser NotificationObject
userNotification = User <$> (string "USER" *> spaces *> userID <* spaces)
                        <*> userProperty

-- | USERSTATUS
userStatusNotification :: Parser NotificationObject
userStatusNotification = UserStatus <$> (string "USERSTATUS" *> spaces *> userStatus)

-- | CURRENTUSERHANDLE
currentUserHandleNotification :: Parser NotificationObject
currentUserHandleNotification = CurrentUserHandle <$>
                                (string "CURRENTUSERHANDLE" *> spaces *> userID)
