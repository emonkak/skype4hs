module Network.Skype.Parser.ChatMember where

import Control.Applicative
import Data.Attoparsec.ByteString.Lazy
import Network.Skype.Parser.Chat
import Network.Skype.Parser.Types
import Network.Skype.Protocol.ChatMember

chatMemberProperty :: Parser ChatMemberProperty
chatMemberProperty = choice
  [ ChatMemberChatName <$> (property "CHATNAME" *> chatID)
  , ChatMemberIdentity <$> (property "IDENTITY" *> userID)
  , ChatMemberRole     <$> (property "ROLE" *> chatRole)
  , ChatMemberIsActive <$> (property "IS_ACTIVE" *> boolean)
  ]
  where
    property prop = string prop *> spaces
