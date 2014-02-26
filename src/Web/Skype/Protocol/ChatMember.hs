module Web.Skype.Protocol.ChatMember where

import Data.Typeable (Typeable)
import Web.Skype.Protocol.Chat
import Web.Skype.Protocol.Types

data ChatMemberProperty = ChatMemberChatName ChatID
                        | ChatMemberIdentity UserID
                        | ChatMemberRole ChatRole
                        | ChatMemberIsActive Bool
  deriving (Eq, Show, Typeable)
