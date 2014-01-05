module Web.Skype.Protocol (
  SkypeResponse(..),
  ConnectionStatus(..),
  ProtocolVersion,

  module Web.Skype.Protocol.Chat,
  module Web.Skype.Protocol.ChatMessage,
  module Web.Skype.Protocol.Types,
  module Web.Skype.Protocol.User
) where

import Web.Skype.Core
import Web.Skype.Protocol.Chat
import Web.Skype.Protocol.ChatMessage
import Web.Skype.Protocol.Types
import Web.Skype.Protocol.User

data SkypeResponse
  = AlterChatAcceptAdd
  | AlterChatAddMembers
  | AlterChatBookmarked Bool
  | AlterChatClearRecentMessages
  | AlterChatDisband
  | AlterChatEnterPassword
  | AlterChatJoin
  | AlterChatLeave
  | AlterChatSetAlertString
  | AlterChatSetOptions
  | AlterChatSetPassword
  | AlterChatSetTopic
  | Chat ChatID ChatProperty
  | Chats [ChatID]
  | ChatMessage ChatMessageID ChatMessageProperty
  | ConnectionStatus ConnectionStatus
  | Error ErrorCode ErrorDescription
  | OK
  | OpenChat ChatID
  | Protocol ProtocolVersion
  | User UserID UserProperty
  deriving (Eq, Show)

data ConnectionStatus = ConnectionStatusOffline
                      | ConnectionStatusConnecting
                      | ConnectionStatusPausing
                      | ConnectionStatusOnline
  deriving (Eq, Show)

type ProtocolVersion = Int
