module Web.Skype.Protocol (
  module Web.Skype.Protocol.Chat,
  module Web.Skype.Protocol.ChatMember,
  module Web.Skype.Protocol.ChatMessage,
  module Web.Skype.Protocol.Misc,
  module Web.Skype.Protocol.Types,
  module Web.Skype.Protocol.User,

  NotificationObject(..)
) where

import Web.Skype.Protocol.Chat
import Web.Skype.Protocol.ChatMember
import Web.Skype.Protocol.ChatMessage
import Web.Skype.Protocol.Misc
import Web.Skype.Protocol.Types
import Web.Skype.Protocol.User

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
                        | UserStatus UserStatus
                        | CurrentUserHandle UserID
  deriving (Eq, Show)
