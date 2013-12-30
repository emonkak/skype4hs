module Web.Skype.Parser (
  parseNotification
) where

import Control.Applicative
import Data.Attoparsec.ByteString.Lazy
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.Attoparsec.Combinator
import Data.Word8
import Foreign.C.Types
import Web.Skype.Protocol

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

parseNotification :: BL.ByteString -> Either String Notification
parseNotification = eitherResult . parse p_notification

spaces = takeWhile1 isSpace

p_notification :: Parser Notification
p_notification = choice
  [ ChatNotification <$> p_chat ]

p_userId :: Parser UserId
p_userId = takeWhile1 $ \c ->
           any ($ c) [isAlpha, isDigit, (==) _underscore, (==) _hyphen]

p_chat :: Parser Chat
p_chat = string "CHAT"
      *> spaces
      *> (Chat <$> (p_chatId <* spaces) <*> p_chatProperty)

p_chatId :: Parser ChatId
p_chatId = takeWhile1 $ not . isSpace

p_chatProperty :: Parser ChatProperty
p_chatProperty = choice
  [ ChatName              <$> (property "NAME" *> p_chatId)
  , ChatTimestamp         <$> (property "TIMESTAMP" *> p_chatTimestamp)
  , ChatAdder             <$> (property "ADDER" *> p_userId)
  , ChatStatus            <$> (property "STATUS" *> p_chatStatus)
  , ChatPosters           <$> (property "POSTERS" *> p_posters)
  , ChatMembers           <$> (property "MEMBERS" *> p_members)
  , ChatTopic             <$> (property "TOPIC" *> p_chatTopic)
  , ChatTopicXml          <$> (property "TOPICXML" *> p_chatTopic)
  , ChatActiveMembers     <$> (property "ACTIVEMEMBERS" *> p_members)
  , ChatFriendyName       <$> (property "FRIENDLYNAME" *> p_chatWindowTitle)
  , ChatMessages          <$> (property "CHATMESSAGES" *> p_chatMessages)
  , ChatRecentMessages    <$> (property "RECENTCHATMESSAGES" *> p_chatMessages)
  , ChatBookmarked        <$> (property "BOOKMARKED" *> p_boolean)
  , ChatMemberObjects     <$> (property "MEMBEROBJECTS" *> p_chatMemberObjects)
  , ChatPasswordHint      <$> (property "PASSWORDHINT" *> p_chatPasswordHint)
  , ChatGuidelines        <$> (property "GUIDELINES" *> p_chatGuidelines)
  , ChatOptions           <$> (property "OPTIONS" *> p_chatOptions)
  , ChatDescription       <$> (property "DESCRIPTION" *> p_chatDescription)
  , ChatDialogPartner     <$> (property "DIALOG_PARTNER" *> p_userId)
  , ChatActivityTimestamp <$> (property "ACTIVITY_TIMESTAMP" *> p_chatTimestamp)
  , ChatType              <$> (property "TYPE" *> p_chatType)
  , ChatMyStatus          <$> (property "MYSTATUS" *> p_chatMyStatus)
  , ChatMyRole            <$> (property "MYROLE" *> p_chatRole)
  , ChatBlob              <$> (property "BLOB" *> p_chatBlob)
  , ChatApplicants        <$> (property "APPLICANTS" *> p_chatApplicants)
  , ChatClosed            <$  (string "CLOSED" *> endOfInput)
  , ChatOpened            <$  (string "OPENED" *> endOfInput)
  ]
  where
    property prop = string prop *> spaces

    p_posters = p_userId `sepBy` spaces

    p_members = p_userId `sepBy` spaces

    p_boolean = (True <$ string "TRUE") <|> (False <$ string "FALSE")

    p_chatMessages = p_chatMessageId `sepBy` (word8 _comma *> spaces)

    p_chatMemberObjects = p_chatMemberId `sepBy` (word8 _comma *> spaces)

    p_chatApplicants = p_userId `sepBy` spaces

p_chatTimestamp :: Parser ChatTimestamp
p_chatTimestamp = CTime <$> decimal

p_chatStatus :: Parser ChatStatus
p_chatStatus = choice
  [ ChatStatusLegacyDialog    <$ string "LEGACY_DIALOG"
  , ChatStatusDialog          <$ string "DIALOG"
  , ChatStatusMultiSubscribed <$ string "MULTI_SUBSCRIBED"
  , ChatStatusUnsubscribed    <$ string "UNSUBSCRIBED"
  ]

p_chatTopic :: Parser ChatTopic
p_chatTopic = T.decodeUtf8 <$> takeByteString

p_chatMessageId :: Parser ChatMessageId
p_chatMessageId = decimal

p_chatWindowTitle :: Parser ChatWindowTitle
p_chatWindowTitle = T.decodeUtf8 <$> takeByteString

p_chatMemberId :: Parser ChatMemberId
p_chatMemberId = decimal

p_chatPasswordHint :: Parser ChatPasswordHint
p_chatPasswordHint = T.decodeUtf8 <$> takeByteString

p_chatGuidelines :: Parser ChatGuidelines
p_chatGuidelines = T.decodeUtf8 <$> takeByteString

p_chatOptions :: Parser ChatOption
p_chatOptions = ChatOption <$> decimal

p_chatDescription :: Parser ChatDescription
p_chatDescription = T.decodeUtf8 <$> takeByteString

p_chatType :: Parser ChatType
p_chatType = choice
  [ ChatTypeLegacyDialog       <$ string "LEGACY_DIALOG"
  , ChatTypeDialog             <$ string "DIALOG"
  , ChatTypeMultiChat          <$ string "MULTICHAT"
  , ChatTypeSharedGroup        <$ string "SHAREDGROUP"
  , ChatTypeLegacyUnsubscribed <$ string "LEGACY_UNSUBSCRIBED"
  ]

p_chatMyStatus :: Parser ChatMyStatus
p_chatMyStatus = choice
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

p_chatRole :: Parser ChatRole
p_chatRole = choice
  [ ChatRoleCreator   <$ string "CREATOR"
  , ChatRoleMaster    <$ string "MASTER"
  , ChatRoleHelper    <$ string "HELPER"
  , ChatRoleUser      <$ string "USER"
  , ChatRoleListener  <$ string "LISTENER"
  , ChatRoleApplicant <$ string "APPLICANT"
  ]

p_chatBlob :: Parser ChatBlob
p_chatBlob = takeByteString
