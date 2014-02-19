module Web.Skype.Parser.ChatMember where

import Control.Applicative
import Data.Attoparsec.ByteString.Lazy
import Web.Skype.Parser.Chat
import Web.Skype.Parser.Types
import Web.Skype.Protocol.ChatMember

chatMemberProperty :: Parser ChatMemberProperty
chatMemberProperty = choice
  [ ChatMemberChatName <$> (property "CHATNAME" *> chatID)
  , ChatMemberIdentity <$> (property "IDENTITY" *> userID)
  , ChatMemberRole     <$> (property "ROLE" *> chatRole)
  , ChatMemberIsActive <$> (property "IS_ACTIVE" *> boolean)
  ]
  where
    property prop = string prop *> spaces
