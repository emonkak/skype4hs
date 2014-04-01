module Network.Skype.Protocol.Chat where

import Data.Bits (Bits)
import Data.Typeable (Typeable)
import Network.Skype.Protocol.Types

data AlterChatProperty = AlterChatAcceptAdd
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
  deriving (Eq, Show, Typeable)

data ChatProperty
  -- | Chat ID
  = ChatName ChatID

  -- | Time when chat was created.
  | ChatTimestamp Timestamp

  -- | User who added the current user to chat.
  | ChatAdder (Maybe UserID)

  -- | Chat status
  | ChatStatus ChatStatus

  -- | Members who have posted messages.
  | ChatPosters [UserID]

  -- | All users who have been there.
  | ChatMembers [UserID]

  -- | Chat topic
  | ChatTopic ChatTopic

  -- | set when a chat topic contains XML formatting elements (topic was changed
  -- with ALTER CHATSETTOPICXML command) This property works in parallel with
  -- TOPIC property - when TOPICXML is set, the value is stripped of XML tags
  -- and updated in TOPIC.
  | ChatTopicXml ChatTopic

  -- | Members who have stayed in chat.
  | ChatActiveMembers [UserID]

  -- | Name shown in chat window title.
  | ChatFriendyName ChatWindowTitle

  -- | All messages IDs in this chat.
  | ChatMessages [ChatMessageID]

  -- | List of missed/recent chatmessage identifiers
  | ChatRecentMessages [ChatMessageID]

  -- | TRUE|FALSE
  | ChatBookmarked Bool

  -- | Contains the list of CHATMEMBER object IDs.
  | ChatMemberObjects [ChatMemberID]

  -- | Contains password hint text for the chat object.
  | ChatPasswordHint ChatPasswordHint

  -- | Contains chat guidelines text.
  | ChatGuidelines ChatGuidelines

  -- | Bitmap of chat options.
  | ChatOptions ChatOption

  -- | Currently used only for hidden synchronization channels for managing
  -- shared groups.
  | ChatDescription ChatDescription

  -- | The handle of the dialog partner for dialog type chats
  -- (chats with two participants).
  | ChatDialogPartner UserID

  -- | The UNIX timestamp of last activity.
  | ChatActivityTimestamp Timestamp

  -- | Chat type
  | ChatType ChatType

  -- | For public chats, this property contains encoded list of chat
  -- join-points. Contents of this field is used in public chat URLs.
  | ChatBlob ChatBlob

  -- | User's current status in chat.
  | ChatMyStatus ChatMyStatus

  -- | User's privilege level in chat.
  | ChatMyRole ChatRole

  -- | This property contains list of skypenames of people who have applied to
  -- join the chat but have not yet been accepted by a public chat
  -- administrator. Users only become applicants when the chat has
  -- JOINERS_BECOME_APPLICANTS option.
  | ChatApplicants [UserID]

  -- | Chat was opened.
  | ChatOpened

  -- | Chat was closed.
  | ChatClosed
  deriving (Eq, Show, Typeable)

data ChatStatus
  -- | Old style IM
  = ChatStatusLegacyDialog

  -- | 1:1 chat
  | ChatStatusDialog

  -- | Participant in chat
  | ChatStatusMultiSubscribed

  -- | Left chat
  | ChatStatusUnsubscribed
  deriving (Eq, Show, Typeable)

newtype ChatOption = ChatOption Int
  deriving (Bits, Eq, Show, Typeable)

-- | When this bit is off, new users cannot join the chat.
chatOptionJoiningEnabled :: ChatOption
chatOptionJoiningEnabled = ChatOption 1

-- | When this bit is on, new users will be able to join the chat but they
-- will be unable to post or receive messages until authorized by one of the
-- chat administrators (CREATOR or MASTER).
chatOptionJoinersBecomeApplicants :: ChatOption
chatOptionJoinersBecomeApplicants = ChatOption 2

-- | When this bit is on, new users will be able to receive message in chat
-- but unable to post until promoted to USER role. Basically a read-only flag
-- for new users.
chatOptionJoinersBecomeListeners :: ChatOption
chatOptionJoinersBecomeListeners = ChatOption 4

-- | When this bit is off, newly joined members can see chat history prior to
-- their joining. Maximum amount of history backlog available is either 400
-- messages or 2 weeks of time, depending on which limit is reached first.
chatOptionHistoryDisclosed :: ChatOption
chatOptionHistoryDisclosed = ChatOption 8

-- | Read-only flag for chat members with USER role.
chatOptionUsersAreListeners :: ChatOption
chatOptionUsersAreListeners = ChatOption 16

-- | when this bit of options is off, USER level chat members can change chat
-- topic and the topic picture.
chatOptionTopicAndPicLockedForUsers :: ChatOption
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
  deriving (Eq, Show, Typeable)

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
  deriving (Eq, Show, Typeable)

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
  deriving (Eq, Show, Typeable)
