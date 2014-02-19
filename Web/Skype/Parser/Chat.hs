module Web.Skype.Parser.Chat where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.Attoparsec.ByteString.Lazy
import Data.Word8
import Web.Skype.Parser.Types
import Web.Skype.Protocol.Chat

chatProperty :: Parser ChatProperty
chatProperty = choice
  [ ChatName              <$> (property "NAME" *> chatID)
  , ChatTimestamp         <$> (property "TIMESTAMP" *> timestamp)
  , ChatAdder             <$> (property "ADDER" *> userID)
  , ChatStatus            <$> (property "STATUS" *> chatStatus)
  , ChatPosters           <$> (property "POSTERS" *> userIDs)
  , ChatMembers           <$> (property "MEMBERS" *> userIDs)
  , ChatTopic             <$> (property "TOPIC" *> chatTopic)
  , ChatTopicXml          <$> (property "TOPICXML" *> chatTopic)
  , ChatActiveMembers     <$> (property "ACTIVEMEMBERS" *> userIDs)
  , ChatFriendyName       <$> (property "FRIENDLYNAME" *> chatWindowTitle)
  , ChatMessages          <$> (property "CHATMESSAGES" *> chatMessages)
  , ChatRecentMessages    <$> (property "RECENTCHATMESSAGES" *> chatMessages)
  , ChatBookmarked        <$> (property "BOOKMARKED" *> boolean)
  , ChatMemberObjects     <$> (property "MEMBEROBJECTS" *> chatMemberObjects)
  , ChatPasswordHint      <$> (property "PASSWORDHINT" *> chatPasswordHint)
  , ChatGuidelines        <$> (property "GUIDELINES" *> chatGuidelines)
  , ChatOptions           <$> (property "OPTIONS" *> chatOptions)
  , ChatDescription       <$> (property "DESCRIPTION" *> chatDescription)
  , ChatDialogPartner     <$> (property "DIALOG_PARTNER" *> userID)
  , ChatActivityTimestamp <$> (property "ACTIVITY_TIMESTAMP" *> timestamp)
  , ChatType              <$> (property "TYPE" *> chatType)
  , ChatMyStatus          <$> (property "MYSTATUS" *> chatMyStatus)
  , ChatMyRole            <$> (property "MYROLE" *> chatRole)
  , ChatBlob              <$> (property "BLOB" *> chatBlob)
  , ChatApplicants        <$> (property "APPLICANTS" *> userIDs)
  , ChatClosed            <$  (string "CLOSED" *> endOfInput)
  , ChatOpened            <$  (string "OPENED" *> endOfInput)
  ]
  where
    property prop = string prop *> spaces

    userIDs = userID `sepBy` spaces

    chatMessages = chatMessageID `sepBy` (word8 _comma *> spaces)

    chatMemberObjects = chatMemberID `sepBy` (word8 _comma *> spaces)

chatStatus :: Parser ChatStatus
chatStatus = choice
  [ ChatStatusLegacyDialog    <$ string "LEGACY_DIALOG"
  , ChatStatusDialog          <$ string "DIALOG"
  , ChatStatusMultiSubscribed <$ string "MULTI_SUBSCRIBED"
  , ChatStatusUnsubscribed    <$ string "UNSUBSCRIBED"
  ]

chatOptions :: Parser ChatOption
chatOptions = ChatOption <$> decimal

chatType :: Parser ChatType
chatType = choice
  [ ChatTypeLegacyDialog       <$ string "LEGACY_DIALOG"
  , ChatTypeDialog             <$ string "DIALOG"
  , ChatTypeMultiChat          <$ string "MULTICHAT"
  , ChatTypeSharedGroup        <$ string "SHAREDGROUP"
  , ChatTypeLegacyUnsubscribed <$ string "LEGACY_UNSUBSCRIBED"
  ]

chatMyStatus :: Parser ChatMyStatus
chatMyStatus = choice
  [ ChatMyStatusConnecting              <$ string "CONNECTING"
  , ChatMyStatusWaitingRemoteAccept     <$ string "WAITING_REMOTE_ACCEPT"
  , ChatMyStatusAcceptRequired          <$ string "ACCEPT_REQUIRED"
  , ChatMyStatusPasswordRequired        <$ string "PASSWORD_REQUIRED"
  , ChatMyStatusSubscribed              <$ string "SUBSCRIBED"
  , ChatMyStatusUnsubscribed            <$ string "UNSUBSCRIBED"
  , ChatMyStatusDisbanded               <$ string "CHAT_DISBANDED"
  , ChatMyStatusQueuedBecauseChatIsFull <$ string "QUEUED_BECAUSE_CHAT_IS_FULL"
  , ChatMyStatusApplicationDenied       <$ string "APPLICATION_DENIED"
  , ChatMyStatusKicked                  <$ string "KICKED"
  , ChatMyStatusBanned                  <$ string "BANNED"
  , ChatMyStatusRetryConnecting         <$ string "RETRY_CONNECTING"
  ]

chatRole :: Parser ChatRole
chatRole = choice
  [ ChatRoleCreator   <$ string "CREATOR"
  , ChatRoleMaster    <$ string "MASTER"
  , ChatRoleHelper    <$ string "HELPER"
  , ChatRoleUser      <$ string "USER"
  , ChatRoleListener  <$ string "LISTENER"
  , ChatRoleApplicant <$ string "APPLICANT"
  ]

alterChatProperties :: Parser AlterChatProperty
alterChatProperties = choice
  [ AlterChatAcceptAdd           <$  (string "ACCEPTADD")
  , AlterChatAddMembers          <$  (string "ADDMEMBERS")
  , AlterChatBookmarked          <$> (string "BOOKMARKED" *> boolean)
  , AlterChatClearRecentMessages <$  (string "CLEARRECENTMESSAGES")
  , AlterChatDisband             <$  (string "DISBAND")
  , AlterChatEnterPassword       <$  (string "ENTERPASSWORD")
  , AlterChatJoin                <$  (string "JOIN")
  , AlterChatLeave               <$  (string "LEAVE")
  , AlterChatSetAlertString      <$  (string "SETALERTSTRING")
  , AlterChatSetOptions          <$  (string "SETOPTIONS")
  , AlterChatSetPassword         <$  (string "SETPASSWORD")
  , AlterChatSetTopic            <$  (string "SETTOPIC")
  ]
