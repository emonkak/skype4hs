module Web.Skype.Parser (
  parseResponse,
  parseResponseWithCommandID
) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.Attoparsec.ByteString.Lazy
import Data.Attoparsec.Combinator
import Data.Maybe (fromMaybe)
import Data.Word8
import Foreign.C.Types (CTime(..))
import Web.Skype.Core
import Web.Skype.Protocol

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

-- Public API  --{{{1

parseResponse :: BL.ByteString -> Maybe SkypeResponse
parseResponse = maybeResult . parse p_response

parseResponseWithCommandID :: BL.ByteString -> Maybe (CommandID, SkypeResponse)
parseResponseWithCommandID = maybeResult . parse p_responseWithCommandID




-- Main  --{{{1

p_response :: Parser SkypeResponse
p_response = choice
  [ p_alter
  , p_chat
  , p_chatMessage
  , p_connectionStatus
  , p_error
  , p_ok
  , p_protocol
  ]

p_responseWithCommandID :: Parser (CommandID, SkypeResponse)
p_responseWithCommandID = (,) <$> (p_commandID <* spaces) <*> p_response
  where
    p_commandID = word8 _numbersign *> takeWhile1 (not . isSpace)




-- ALTER  --{{{1

p_alter :: Parser SkypeResponse
p_alter = string "ALTER" *> spaces *> choice
  [ p_chat ]
  where
    p_chat = string "CHAT" *> spaces *> choice
      [ AlterChatSetTopic            <$  (string "SETTOPIC")
      , AlterChatAddMembers          <$  (string "ADDMEMBERS")
      , AlterChatJoin                <$  (string "JOIN")
      , AlterChatLeave               <$  (string "LEAVE")
      , AlterChatBookmarked          <$> (string "BOOKMARKED" *> p_boolean)
      , AlterChatClearRecentMessages <$  (string "CLEARRECENTMESSAGES")
      , AlterChatSetAlertString      <$  (string "SETALERTSTRING")
      , AlterChatAcceptAdd           <$  (string "ACCEPTADD")
      , AlterChatDisband             <$  (string "DISBAND")
      , AlterChatSetPassword         <$  (string "SETPASSWORD")
      , AlterChatEnterPassword       <$  (string "ENTERPASSWORD")
      , AlterChatSetOptions          <$  (string "SETOPTIONS")
      ]




-- CHAT  --{{{1

p_chat :: Parser SkypeResponse
p_chat = string "CHAT"
      *> spaces
      *> (ChatResponse <$> (p_chatID <* spaces) <*> p_chatProperty)

p_chatID :: Parser ChatID
p_chatID = ChatID <$> (takeWhile1 $ not . isSpace)

p_chatProperty :: Parser ChatProperty
p_chatProperty = choice
  [ ChatName              <$> (property "NAME" *> p_chatID)
  , ChatTimestamp         <$> (property "TIMESTAMP" *> p_timestamp)
  , ChatAdder             <$> (property "ADDER" *> p_userID)
  , ChatStatus            <$> (property "STATUS" *> p_chatStatus)
  , ChatPosters           <$> (property "POSTERS" *> p_userIDs)
  , ChatMembers           <$> (property "MEMBERS" *> p_userIDs)
  , ChatTopic             <$> (property "TOPIC" *> p_chatTopic)
  , ChatTopicXml          <$> (property "TOPICXML" *> p_chatTopic)
  , ChatActiveMembers     <$> (property "ACTIVEMEMBERS" *> p_userIDs)
  , ChatFriendyName       <$> (property "FRIENDLYNAME" *> p_chatWindowTitle)
  , ChatMessages          <$> (property "CHATMESSAGES" *> p_chatMessages)
  , ChatRecentMessages    <$> (property "RECENTCHATMESSAGES" *> p_chatMessages)
  , ChatBookmarked        <$> (property "BOOKMARKED" *> p_boolean)
  , ChatMemberObjects     <$> (property "MEMBEROBJECTS" *> p_chatMemberObjects)
  , ChatPasswordHint      <$> (property "PASSWORDHINT" *> p_chatPasswordHint)
  , ChatGuidelines        <$> (property "GUIDELINES" *> p_chatGuidelines)
  , ChatOptions           <$> (property "OPTIONS" *> p_chatOptions)
  , ChatDescription       <$> (property "DESCRIPTION" *> p_chatDescription)
  , ChatDialogPartner     <$> (property "DIALOG_PARTNER" *> p_userID)
  , ChatActivityTimestamp <$> (property "ACTIVITY_TIMESTAMP" *> p_timestamp)
  , ChatType              <$> (property "TYPE" *> p_chatType)
  , ChatMyStatus          <$> (property "MYSTATUS" *> p_chatMyStatus)
  , ChatMyRole            <$> (property "MYROLE" *> p_chatRole)
  , ChatBlob              <$> (property "BLOB" *> p_chatBlob)
  , ChatApplicants        <$> (property "APPLICANTS" *> p_userIDs)
  , ChatClosed            <$  (string "CLOSED" *> endOfInput)
  , ChatOpened            <$  (string "OPENED" *> endOfInput)
  ]
  where
    property prop = string prop *> spaces

    p_userIDs = p_userID `sepBy` spaces

    p_chatMessages = p_chatMessageID `sepBy` (word8 _comma *> spaces)

    p_chatMemberObjects = p_chatMemberID `sepBy` (word8 _comma *> spaces)

p_chatStatus :: Parser ChatStatus
p_chatStatus = choice
  [ ChatStatusLegacyDialog    <$ string "LEGACY_DIALOG"
  , ChatStatusDialog          <$ string "DIALOG"
  , ChatStatusMultiSubscribed <$ string "MULTI_SUBSCRIBED"
  , ChatStatusUnsubscribed    <$ string "UNSUBSCRIBED"
  ]

p_chatTopic :: Parser ChatTopic
p_chatTopic = takeText

p_chatMessageID :: Parser ChatMessageID
p_chatMessageID = ChatMessageID <$> decimal

p_chatWindowTitle :: Parser ChatWindowTitle
p_chatWindowTitle = takeText

p_chatMemberID :: Parser ChatMemberID
p_chatMemberID = ChatMemberID <$> decimal

p_chatPasswordHint :: Parser ChatPasswordHint
p_chatPasswordHint = takeText

p_chatGuidelines :: Parser ChatGuidelines
p_chatGuidelines = takeText

p_chatOptions :: Parser ChatOption
p_chatOptions = ChatOption <$> decimal

p_chatDescription :: Parser ChatDescription
p_chatDescription = takeText

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




-- CHATMESSAGE  --{{{1

p_chatMessage :: Parser SkypeResponse
p_chatMessage = string "CHATMESSAGE"
      *> spaces
      *> (ChatMessageResponse <$> (p_chatMessageID <* spaces) <*> p_chatMessageProperty)

p_chatMessageProperty :: Parser ChatMessageProperty
p_chatMessageProperty = choice
  [ ChatMessageTimestamp       <$> (property "TIMESTAMP" *> p_timestamp)
  , ChatMessageFromHandle      <$> (property "FROM_HANDLE" *> p_userHandle)
  , ChatMessageFromDisplayName <$> (property "FROM_DISPNAME" *> p_userHandle)
  , ChatMessageType            <$> (property "TYPE" *> p_chatMessageType)
  , ChatMessageStatus          <$> (property "STATUS" *> p_chatMessageStatus)
  , ChatMessageLeaveReason     <$> (property "LEAVEREASON" *> p_chatMessageLeaveReason)
  , ChatMessageChatName        <$> (property "CHATNAME" *> p_chatID)
  , ChatMessageUsers           <$> (property "USERS" *> p_userIDs)
  , ChatMessageIsEditable      <$> (property "IS_EDITABLE" *> p_boolean)
  , ChatMessageEditedBy        <$> (property "EDITED_BY" *> p_userID)
  , ChatMessageEditedTimestamp <$> (property "EDITED_TIMESTAMP" *> p_timestamp)
  , ChatMessageOptions         <$> (property "OPTIONS" *> p_chatOptions)
  , ChatMessageRole            <$> (property "ROLE" *> p_chatRole)
  , ChatMessageSeen            <$  (string "SEEN" *> endOfInput)
  , ChatMessageBody            <$> (property "BODY" *> p_chatMessageBody)
  ]
  where
    property prop = string prop *> spaces

    p_userIDs = p_userID `sepBy` spaces

p_chatMessageBody :: Parser ChatMessageBody
p_chatMessageBody = takeText

p_chatMessageType :: Parser ChatMessageType
p_chatMessageType = choice
  [ ChatMessageTypeSetTopic          <$ string "SETTOPIC"
  , ChatMessageTypeSaid              <$ string "SAID"
  , ChatMessageTypeAddMembers        <$ string "ADDMEMBERS"
  , ChatMessageTypeSawMembers        <$ string "SAWMEMBERS"
  , ChatMessageTypeCreatedChatWith   <$ string "CREATEDCHATWITH"
  , ChatMessageTypeLeft              <$ string "LEFT"
  , ChatMessageTypePostedContacts    <$ string "POSTEDCONTACTS"
  , ChatMessageTypeGapInChat         <$ string "GAP_IN_CHAT"
  , ChatMessageTypeSetRole           <$ string "SETROLE"
  , ChatMessageTypeKicked            <$ string "KICKED"
  , ChatMessageTypeKickBanned        <$ string "KICKBANNED"
  , ChatMessageTypeSetOptions        <$ string "SETOPTIONS"
  , ChatMessageTypeSetPicture        <$ string "SETPICTURE"
  , ChatMessageTypeSetGuideLines     <$ string "SETGUIDELINES"
  , ChatMessageTypeJoinedAsApplicant <$ string "JOINEDASAPPLICANT"
  , ChatMessageTypeUnkown            <$ string "UNKNOWN"
  ]

p_chatMessageStatus :: Parser ChatMessageStatus
p_chatMessageStatus = choice
  [ ChatMessageStatusSending <$ string "SENDING"
  , ChatMessageStatusSent    <$ string "SENT"
  , ChatMessageStatusReceive <$ string "RECEIVE"
  , ChatMessageStatusRead    <$ string "READ"
  ]

p_chatMessageLeaveReason :: Parser ChatMessageLeaveReason
p_chatMessageLeaveReason = choice
  [ ChatMessageLeaveReasonUserNotFound          <$ string "USER_NOT_FOUND"
  , ChatMessageLeaveReasonUserIncapable         <$ string "USER_INCAPABLE"
  , ChatMessageLeaveReasonAdderMustBeFriend     <$ string "ADDER_MUST_BE_FRIEND"
  , ChatMessageLeaveReasonAdderMustBeAuthorized <$ string "ADDED_MUST_BE_AUTHORIZED"
  , ChatMessageLeaveReasonUnsubscribe           <$ string "UNSUBSCRIBE"
  ]




-- CONNSTATUS  --{{{1

p_connectionStatus :: Parser SkypeResponse
p_connectionStatus = ConnectionStatus <$> (string "CONNSTATUS" *> spaces *> p_status)
  where
    p_status = choice
      [ ConnectionStatusOffline    <$ string "OFFLINE"
      , ConnectionStatusConnecting <$ string "CONNECTING"
      , ConnectionStatusPausing    <$ string "PAUSING"
      , ConnectionStatusOnline     <$ string "ONLINE" ]




-- ERROR  --{{{1

p_error :: Parser SkypeResponse
p_error = ErrorResponse <$> p_code <*> (p_description <|> pure "")
  where
    p_code = string "ERROR" *> spaces *> decimal

    p_description = spaces *> (T.decodeUtf8 <$> takeByteString)




-- OK  --{{{1

p_ok :: Parser SkypeResponse
p_ok = OK <$ string "OK"




-- PROTOCOL  --{{{1

p_protocol :: Parser SkypeResponse
p_protocol = Protocol <$> (string "PROTOCOL" *> spaces *> decimal)




-- USER  --{{{1

p_userID :: Parser UserID
p_userID = UserID <$> (takeWhile1 isSymbol)
  where
    isSymbol c = any ($ c) [isAlpha, isDigit, (==) _underscore, (==) _hyphen]

p_userHandle :: Parser UserHandle
p_userHandle = takeText




-- Internal Utilities  --{{{1

p_boolean :: Parser Bool
p_boolean = (True <$ string "TRUE") <|> (False <$ string "FALSE")

p_timestamp :: Parser Timestamp
p_timestamp = CTime <$> decimal

spaces :: Parser BS.ByteString
spaces = takeWhile1 isSpace

takeText :: Parser T.Text
takeText = T.decodeUtf8 <$> takeByteString
