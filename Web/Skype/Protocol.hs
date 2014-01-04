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
  = AlterChatSetTopic
  | AlterChatAddMembers
  | AlterChatJoin
  | AlterChatLeave
  | AlterChatBookmarked Bool
  | AlterChatClearRecentMessages
  | AlterChatSetAlertString
  | AlterChatAcceptAdd
  | AlterChatDisband
  | AlterChatSetPassword
  | AlterChatEnterPassword
  | AlterChatSetOptions
  | ChatResponse ChatID ChatProperty
  | ChatMessageResponse ChatMessageID ChatMessageProperty
  | ConnectionStatus ConnectionStatus
  | ErrorResponse ErrorCode ErrorDescription
  | OK
  | Protocol ProtocolVersion
  deriving (Eq, Show)

data ConnectionStatus = ConnectionStatusOffline
                      | ConnectionStatusConnecting
                      | ConnectionStatusPausing
                      | ConnectionStatusOnline
  deriving (Eq, Show)

type ProtocolVersion = Int
