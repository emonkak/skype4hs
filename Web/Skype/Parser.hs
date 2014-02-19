module Web.Skype.Parser where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.Attoparsec.ByteString.Lazy
import Data.Word8
import Web.Skype.Core
import Web.Skype.Parser.Chat
import Web.Skype.Parser.ChatMessage
import Web.Skype.Parser.Types
import Web.Skype.Parser.User
import Web.Skype.Protocol

import qualified Data.Text.Encoding as T

parseNotification :: Notification -> Maybe SkypeNotification
parseNotification = maybeResult . parse notification

parseNotificationWithCommandID :: Notification -> Maybe (CommandID, SkypeNotification)
parseNotificationWithCommandID = maybeResult . parse notificationWithCommandID

notification :: Parser SkypeNotification
notification = choice
  [ alterNotification
  , chatsNotification
  , chatNotification
  , chatMessageNotification
  , connectionStatusNotification
  , errorNotification
  , openNotification
  , protocolNotification
  , userNotification
  , userStatusNotification
  , currentUserHandleNotification
  ]

notificationWithCommandID :: Parser (CommandID, SkypeNotification)
notificationWithCommandID = (,) <$> (commandID <* spaces) <*> notification
  where
    commandID = word8 _numbersign *> takeWhile1 (not . isSpace)

-- | ALTER
alterNotification :: Parser SkypeNotification
alterNotification = string "ALTER" *> spaces *> choice
  [ AlterChat <$> (string "CHAT" *> spaces *> alterChatProperties)
  ]

-- | CHAT
chatNotification :: Parser SkypeNotification
chatNotification =
  string "CHAT" *> spaces
                *> (Chat <$> (chatID <* spaces) <*> chatProperty)

-- | CHATS
chatsNotification :: Parser SkypeNotification
chatsNotification = Chats <$> (string "CHATS" *> spaces *> chatIDs)
  where
    chatIDs = chatID `sepBy` (word8 _comma *> spaces)

-- | CHATMESSAGE
chatMessageNotification :: Parser SkypeNotification
chatMessageNotification =
    string "CHATMESSAGE" *> spaces
                         *> (ChatMessage <$> (chatMessageID <* spaces) <*> chatMessageProperty)

-- | CONNSTATUS
connectionStatusNotification :: Parser SkypeNotification
connectionStatusNotification = ConnectionStatus <$> (string "CONNSTATUS" *> spaces *> status)
  where
    status = choice
      [ ConnectionStatusOffline    <$ string "OFFLINE"
      , ConnectionStatusConnecting <$ string "CONNECTING"
      , ConnectionStatusPausing    <$ string "PAUSING"
      , ConnectionStatusOnline     <$ string "ONLINE" ]

-- | ERROR
errorNotification :: Parser SkypeNotification
errorNotification = Error <$> code <*> (description <|> pure "")
  where
    code = string "ERROR" *> spaces *> decimal

    description = spaces *> (T.decodeUtf8 <$> takeByteString)

-- | OPEN
openNotification :: Parser SkypeNotification
openNotification = string "OPEN" *> spaces *> choice
  [ chat ]
  where
    chat = OpenChat <$> (string "CHAT" *> spaces *> chatID)

-- | PROTOCOL
protocolNotification :: Parser SkypeNotification
protocolNotification = Protocol <$> (string "PROTOCOL" *> spaces *> decimal)

-- | USER
userNotification :: Parser SkypeNotification
userNotification = User <$> (string "USER" *> spaces *> userID <* spaces)
                          <*> userProperty

-- | USERSTATUS
userStatusNotification :: Parser SkypeNotification
userStatusNotification = UserStatus <$> (string "USERSTATUS" *> spaces *> userStatus)

-- | CURRENTUSERHANDLE
currentUserHandleNotification :: Parser SkypeNotification
currentUserHandleNotification = CurrentUserHandle <$>
                                  (string "CURRENTUSERHANDLE" *> spaces *> userID)
