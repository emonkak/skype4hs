module Network.Skype.Protocol.ChatMessage where

import Data.Typeable (Typeable)
import Network.Skype.Protocol.Chat
import Network.Skype.Protocol.Types

data ChatMessageProperty
  -- | Time when the message was sent (UNIX timestamp).
  = ChatMessageTimestamp Timestamp

  -- | Skypename of the originator of the chatmessage.
  | ChatMessageFromHandle UserID

  -- | Displayed name of the originator of the chatmessage.
  | ChatMessageFromDisplayName UserDisplayName

  -- | Message type
  | ChatMessageType ChatMessageType

  -- | Message status
  | ChatMessageStatus ChatMessageStatus

  -- | Used with LEFT type message
  | ChatMessageLeaveReason (Maybe ChatMessageLeaveReason)

  -- | Chat that includes the message
  | ChatMessageChatName ChatID

  -- | People added to chat
  | ChatMessageUsers [UserID]

  -- | TRUE|FALSE
  | ChatMessageIsEditable Bool

  -- | Identity of the last user who edited the message.
  | ChatMessageEditedBy UserID

  -- | UNIX timestamp of the last edit.
  | ChatMessageEditedTimestamp Timestamp

  -- | Numeric field that contains chat options bitmap in system messages that
  -- get sent out when a change is made to chat options (messages where TYPE is
  -- SETOPTIONS). In normal messages the value of this field is 0.
  | ChatMessageOptions ChatOption

  -- | Used in system messages that get sent when a public chat administrator
  -- has promoted or demoted a chat member. The TYPE property of such messages
  -- is set to SETROLE. In these messages the value of this field is set to the
  -- new assigned role of the promoted or demoted chat member. In normal
  -- messages the value of this property is set to UNKNOWN.
  | ChatMessageRole ChatRole

  -- | Message body
  | ChatMessageBody ChatMessageBody

  -- | The message is seen and will be removed from missed messages list. The
  -- UI sets this automatically if auto-popup is enabled for the user.
  | ChatMessageSeen
  deriving (Eq, Show, Typeable)

data ChatMessageType
  -- | Change of chattopic
  = ChatMessageTypeSetTopic

  -- | IM
  | ChatMessageTypeSaid

  -- | Invited someone to chat.
  | ChatMessageTypeAddedMembers

  -- | Chat participant has seen other members.
  | ChatMessageTypeSawMembers

  -- | Chat to multiple people is created.
  | ChatMessageTypeCreatedChatWith

  -- | Someone left chat.
  -- Can also be a notification if somebody cannot be added to chat.
  | ChatMessageTypeLeft

  -- | System message that is sent or received when one user sends contacts to
  -- another. Added in protocol 7.
  | ChatMessageTypePostedContacts

  -- | messages of this type are generated locally, during synchronization, when
  -- a user enters a chat and it becomes apparent that it is impossible to
  -- update user's chat history completely. Chat history is kept only up to
  -- maximum of 400 messages or 2 weeks. When a user has been offline past that
  -- limit, GAP_IN_CHAT notification is generated. Added in protocol 7.
  | ChatMessageTypeGapInChat

  -- | System messages that are sent when a chat member gets promoted or
  -- demoted.
  | ChatMessageTypeSetRole

  -- | System messages that are sent when a chat member gets kicked
  | ChatMessageTypeKicked

  -- | System messages that are sent when a chat member gets banned.
  | ChatMessageTypeKickBanned

  -- | System messages that are sent when chat options are changed.
  | ChatMessageTypeSetOptions

  -- | System messages that are sent when a chat member has changed the public
  -- chat topic picture. Added in protocol 7.
  | ChatMessageTypeSetPicture

  -- | System messages that are sent when chat guidelines are changed.
  | ChatMessageTypeSetGuideLines

  -- | notification message that gets sent in a public chat with
  -- JOINERS_BECOME_APPLICANTS options, when a new user joins the chat.
  | ChatMessageTypeJoinedAsApplicant

  | ChatMessageTypeEmoted

  -- | Unknown message type, possibly due to connecting to Skype with older
  -- protocol.
  | ChatMessageTypeUnkown
  deriving (Eq, Show, Typeable)

data ChatMessageStatus
  -- | Message is being sent
  = ChatMessageStatusSending

  -- | Message was sent
  | ChatMessageStatusSent

  -- | Message has been received
  | ChatMessageStatusReceived

  -- | Message has been read
  | ChatMessageStatusRead
  deriving (Eq, Show, Typeable)

data ChatMessageLeaveReason
  -- | User was not found
  = ChatMessageLeaveReasonUserNotFound

  -- | User has an older Skype version and cannot join multichat
  | ChatMessageLeaveReasonUserIncapable

  -- | Recipient accepts messages from contacts only and sender is not in
  -- his/her contact list
  | ChatMessageLeaveReasonAdderMustBeFriend

  -- | Recipient accepts messages from authorized users only and sender is not
  -- authorized
  | ChatMessageLeaveReasonAdderMustBeAuthorized

  -- | Participant left chat
  | ChatMessageLeaveReasonUnsubscribe
  deriving (Eq, Show, Typeable)
