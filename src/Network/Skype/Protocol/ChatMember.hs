module Network.Skype.Protocol.ChatMember where

import Data.Typeable (Typeable)
import Network.Skype.Protocol.Chat
import Network.Skype.Protocol.Types

data ChatMemberProperty = ChatMemberChatName ChatID
                        | ChatMemberIdentity UserID
                        | ChatMemberRole ChatRole
                        | ChatMemberIsActive Bool
  deriving (Eq, Show, Typeable)
