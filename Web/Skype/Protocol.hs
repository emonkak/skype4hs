module Web.Skype.Protocol where

import Control.Applicative
import Data.Bits (Bits)
import System.Posix.Types (EpochTime)

import qualified Data.Text as T
import qualified Data.ByteString as BS

data Notification
  = ChatNotification Chat
  | ChatMessageNotification ChatMessage
  deriving (Eq, Show)

-- User  --{{{1

type UserId = BS.ByteString
type UserHandle = T.Text
type UserDisplayName = T.Text




-- Chat  --{{{1

type ChatId = BS.ByteString
type ChatTimestamp = EpochTime
type ChatTopic = T.Text
type ChatWindowTitle = T.Text
type ChatPasswordHint = T.Text
type ChatGuidelines = T.Text
type ChatDescription = T.Text
type ChatBlob = BS.ByteString

data Chat = Chat ChatId ChatProperty
  deriving (Eq, Show)

data ChatProperty
  -- | Chat ID.
  = ChatName ChatId

  -- | Time when chat was created.
  | ChatTimestamp ChatTimestamp

  -- | User who added the current user to chat.
  | ChatAdder UserId

  -- | Chat status.
  | ChatStatus ChatStatus

  -- | Members who have posted messages.
  | ChatPosters [UserId]

  -- | All users who have been there.
  | ChatMembers [UserId]

  -- | Chat topic.
  | ChatTopic ChatTopic

  -- | Chat topic contains XML formatting elements.
  | ChatTopicXml ChatTopic

  -- | Members who have stayed in chat.
  | ChatActiveMembers [UserId]

  -- | Name shown in chat window title.
  | ChatFriendyName ChatWindowTitle

  -- | list of chatmessage identifiers
  | ChatMessages [ChatMessageId]

  -- | List of missed/recent chatmessage identifiers.
  | ChatRecentMessages [ChatMessageId]

  -- | TRUE|FALSE
  | ChatBookmarked Bool

  -- | Contains the list of CHATMEMBER object IDs
  | ChatMemberObjects [ChatMemberId]

  -- | Contains password hint text for the chat object.
  | ChatPasswordHint ChatPasswordHint

  -- | Contains chat guidelines text.
  | ChatGuidelines ChatGuidelines

  -- | Bitmap of chat options.
  | ChatOptions ChatOption

  -- | currently used only for hidden synchronization channels for managing
  -- shared groups.
  | ChatDescription ChatDescription

  -- | The handle of the dialog partner for dialog type chats
  -- (chats with two participants).
  | ChatDialogPartner UserId

  -- | The UNIX timestamp of last activity.
  | ChatActivityTimestamp EpochTime

  -- | Chat type.
  | ChatType ChatType

  -- | For public chats, this property contains encoded list of chat
  -- join-points. Contents of this field is used in public chat URLs.
  | ChatBlob ChatBlob

  -- | User's current status in chat
  | ChatMyStatus ChatMyStatus

  -- | User's privilege level in chat
  | ChatMyRole ChatRole

  -- | this property contains list of skypenames of people who have applied to
  -- join the chat but have not yet been accepted by a public chat
  -- administrator. Users only become applicants when the chat has
  -- JOINERS_BECOME_APPLICANTS option.
  | ChatApplicants [UserId]

  | ChatClosed

  | ChatOpened
  deriving (Eq, Show)

data ChatStatus
  -- | Old style IM.
  = ChatStatusLegacyDialog

  -- | 1:1 chat.
  | ChatStatusDialog

  -- | Participant in chat.
  | ChatStatusMultiSubscribed

  -- | Left chat.
  | ChatStatusUnsubscribed
  deriving (Eq, Show)

newtype ChatOption = ChatOption Int
  deriving (Bits, Eq, Show)

-- | When this bit is off, new users cannot join the chat.
chatOptionJoiningEnabled = ChatOption 1

-- | When this bit is on, new users will be able to join the chat but they
-- will be unable to post or receive messages until authorized by one of the
-- chat administrators (CREATOR or MASTER).
chatOptionJoinersBecomeApplicants = ChatOption 2

-- | When this bit is on, new users will be able to receive message in chat
-- but unable to post until promoted to USER role. Basically a read-only flag
-- for new users.
chatOptionJoinersBecomeListeners = ChatOption 4

-- | When this bit is off, newly joined members can see chat history prior to
-- their joining. Maximum amount of history backlog available is either 400
-- messages or 2 weeks of time, depending on which limit is reached first.
chatOptionHistoryDisclosed = ChatOption 8

-- | Read-only flag for chat members with USER role.
chatOptionUsersAreListeners = ChatOption 16

-- | when this bit of options is off, USER level chat members can change chat
-- topic and the topic picture.
chatOptionTopicAndPicLockedForUsers = ChatOption 32

data ChatType
  -- | No longer supported.
  = ChatTypeLegacyDialog

  -- | A chat with only two participants.
  | ChatTypeDialog

  -- | A chat with more than two participants.
  | ChatTypeMultiChat

  -- | A chat used for synchronization of shared contact groups.
  | ChatTypeSharedGroup

  -- | No longer supported.
  | ChatTypeLegacyUnsubscribed
  deriving (Eq, Show)

data ChatMyStatus
  -- | Status set when the system is trying to connect to the chat.
  = ChatMyStatusConnecting

  -- | Set when a new user joins a public chat. When the chat has
  -- "participants need authorization to read messages" option, the MYSTATUS
  -- property of a new applicant will remain in this status until he gets
  -- accepted or rejected by a chat administrator. Otherwise user's MYSTATUS
  -- will automatically change to either LISTENER or USER, depending on public
  -- chat options.
  | ChatMyStatusWaitingRemoteAccept

  -- | This status is used for shared contact groups functionality.
  | ChatMyStatusAcceptRequired

  -- | Status set when the system is waiting for user to supply the chat
  -- password.
  | ChatMyStatusPasswordRequired

  -- | Set when user joins the chat.
  | ChatMyStatusSubscribed

  -- | Set when user leaves the chat or chat ends.
  | ChatMyStatusUnsubscribed

  -- | Status set when the chat is disbanded.
  | ChatMyStatusDisbanded

  -- | Currently the maximum number of people in the same chat is 100.
  | ChatMyStatusQueuedBecauseChatIsFull

  -- | Set when public chat administrator has rejected user from joining.
  | ChatMyStatusApplicationDenied

  -- | Status set when the user has been kicked from the chat. Note that it is
  -- possible for the user to re-join the chat after being kicked.
  | ChatMyStatusKicked

  -- | Status set when the user has been banned from the chat.
  | ChatMyStatusBanned

  -- | Status set when connect to chat failed and system retries to establish
  -- connection.
  | ChatMyStatusRetryConnecting
  deriving (Eq, Show)

data ChatRole
  -- | Member who created the chat. There can be only one creator per chat. Only
  -- creator can promote other members to masters.
  = ChatRoleCreator

  -- | Also known as chat hosts. Masters cannot promote other people to masters.
  | ChatRoleMaster

  -- | A semi-privileged member. Helpers will not be affected by the
  -- USERS_ARE_LISTENERS option. Helpers cannot promote or demote other members.
  | ChatRoleHelper

  -- | Regular members who can post messages into the chat.
  | ChatRoleUser

  -- | A demoted member who can only receive messages but not post anything into
  -- the chat.
  | ChatRoleListener

  -- | A member waiting for acceptance into the chat. Member cannot be demoted
  -- to applicants once they have been accepted.
  | ChatRoleApplicant
  deriving (Eq, Show)

-- ChatMessage  --{{{1

type ChatMessageId = Integer
type ChatMessageBody = T.Text

data ChatMessage = Chatmessage ChatMessageId ChatMessageProperty
  deriving (Eq, Show)

data ChatMessageProperty
  -- | Time when the message was sent (UNIX timestamp)
  = ChatMesssageTimestamp EpochTime

  -- | Skypename of the originator of the chatmessage
  | ChatMessageFromHandle UserHandle

  -- | Displayed name of the originator of the chatmessage
  | ChatMessageFromDisplayName UserDisplayName

  -- | message type
  | ChatMessageType ChatMessageType

  -- | message body
  | ChatMessageBody ChatMessageBody
  deriving (Eq, Show)

data ChatMessageType
  = ChatMessageSetTopic
  | ChatMessageSaid
  | ChatMessageAddMembers
  | ChatMessageSawMembers
  | ChatMessageCreatedChatWith
  | ChatMessageLeft
  | ChatMessagePostedContacts
  | ChatMessageGetInChat
  | ChatMessageSetRole
  | ChatMessageKicked
  | ChatMessageKickBanned
  | ChatMessageSetOptions
  | ChatMessageSetPicture
  | ChatMessageSetGuideLines
  | ChatMessageJoinedAsApplicant
  | ChatMessageUnkown
  deriving (Eq, Show)




-- ChatMember  --{{{1

type ChatMemberId = Integer

data ChatMemberProperty
  = ChatMemberProperty
  deriving (Eq, Show)
