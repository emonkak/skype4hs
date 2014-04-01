module Network.Skype.Protocol (
  module Network.Skype.Protocol.Chat,
  module Network.Skype.Protocol.ChatMember,
  module Network.Skype.Protocol.ChatMessage,
  module Network.Skype.Protocol.Misc,
  module Network.Skype.Protocol.Types,
  module Network.Skype.Protocol.User,

  NotificationObject(..)
) where

import Network.Skype.Protocol.Chat
import Network.Skype.Protocol.ChatMember
import Network.Skype.Protocol.ChatMessage
import Network.Skype.Protocol.Misc
import Network.Skype.Protocol.Types
import Network.Skype.Protocol.User

data NotificationObject = AlterChat AlterChatProperty
                        | Chat ChatID ChatProperty
                        | Chats [ChatID]
                        | ChatMember ChatMemberID ChatMemberProperty
                        | ChatMessage ChatMessageID ChatMessageProperty
                        | ConnectionStatus ConnectionStatus
                        | Error ErrorCode ErrorDescription
                        | OpenChat ChatID
                        | Protocol ProtocolVersion
                        | User UserID UserProperty
                        | Users [UserID]
                        | UserStatus UserStatus
                        | CurrentUserHandle UserID
  deriving (Eq, Show)
