module Web.Skype.Parser (
  SkypeResponse(..),
  AlterObject(..),
  AlterChatProperty(..),
  parseResponse,
  parseResponseWithCommandID
) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.Attoparsec.ByteString.Lazy
import Data.Attoparsec.Combinator
import Data.Char (chr)
import Data.Maybe (fromMaybe)
import Data.Time.Calendar (fromGregorian)
import Data.Word8
import Foreign.C.Types (CTime(..))
import Web.Skype.Core
import Web.Skype.Protocol

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

data SkypeResponse = Alter AlterObject
                   | Chat ChatID ChatProperty
                   | Chats [ChatID]
                   | ChatMessage ChatMessageID ChatMessageProperty
                   | ConnectionStatus ConnectionStatus
                   | Error ErrorCode ErrorDescription
                   | OpenChat ChatID
                   | Protocol ProtocolVersion
                   | User UserID UserProperty
                   | UserStatus UserStatus
                   | CurrentUserHandle UserID
  deriving (Eq, Show)

data AlterObject = AlterChat AlterChatProperty
  deriving (Eq, Show)

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
  deriving (Eq, Show)

parseResponse :: BL.ByteString -> Either String SkypeResponse
parseResponse = eitherResult . parse p_response

parseResponseWithCommandID :: BL.ByteString -> Maybe (CommandID, SkypeResponse)
parseResponseWithCommandID = maybeResult . parse p_responseWithCommandID

p_response :: Parser SkypeResponse
p_response = choice
  [ p_alter
  , p_chats
  , p_chat
  , p_chatMessage
  , p_connectionStatus
  , p_error
  , p_open
  , p_protocol
  , p_user
  , p_userStatus
  , p_currentUserHandle
  ]

p_responseWithCommandID :: Parser (CommandID, SkypeResponse)
p_responseWithCommandID = (,) <$> (p_commandID <* spaces) <*> p_response
  where
    p_commandID = word8 _numbersign *> takeWhile1 (not . isSpace)

-- * ALTER
----------

p_alter :: Parser SkypeResponse
p_alter = Alter <$> (string "ALTER" *> spaces *> p_objects)
  where
    p_objects = choice
      [ AlterChat <$> (string "CHAT" *> spaces *> p_chatProperties)
      ]

    p_chatProperties = choice
      [ AlterChatAcceptAdd           <$  (string "ACCEPTADD")
      , AlterChatAddMembers          <$  (string "ADDMEMBERS")
      , AlterChatBookmarked          <$> (string "BOOKMARKED" *> p_boolean)
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

-- * CHAT
---------

p_chat :: Parser SkypeResponse
p_chat = string "CHAT"
      *> spaces
      *> (Chat <$> (p_chatID <* spaces) <*> p_chatProperty)

p_chatID :: Parser ChatID
p_chatID = takeWhile1 $ isSymbol
  where
    isSymbol c = any ($ c)
      [ isAlpha
      , isDigit
      , (==) _numbersign
      , (==) _dollar
      , (==) _hyphen
      , (==) _period
      , (==) _slash
      , (==) _underscore
      , (==) _semicolon
      ]

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
p_chatMessageID = decimal

p_chatWindowTitle :: Parser ChatWindowTitle
p_chatWindowTitle = takeText

p_chatMemberID :: Parser ChatMemberID
p_chatMemberID = decimal

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

-- * CHATS
----------

p_chats :: Parser SkypeResponse
p_chats = Chats <$> (string "CHATS" *> spaces *> p_chatIDs)
  where
    p_chatIDs = p_chatID `sepBy` (word8 _comma *> spaces)

-- * CHATMESSAGE
----------------

p_chatMessage :: Parser SkypeResponse
p_chatMessage = string "CHATMESSAGE"
      *> spaces
      *> (ChatMessage <$> (p_chatMessageID <* spaces) <*> p_chatMessageProperty)

p_chatMessageProperty :: Parser ChatMessageProperty
p_chatMessageProperty = choice
  [ ChatMessageTimestamp       <$> (property "TIMESTAMP" *> p_timestamp)
  , ChatMessageFromHandle      <$> (property "FROM_HANDLE" *> p_userID)
  , ChatMessageFromDisplayName <$> (property "FROM_DISPNAME" *> p_userDisplayName)
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

-- * CONNSTATUS
---------------

p_connectionStatus :: Parser SkypeResponse
p_connectionStatus = ConnectionStatus <$> (string "CONNSTATUS" *> spaces *> p_status)
  where
    p_status = choice
      [ ConnectionStatusOffline    <$ string "OFFLINE"
      , ConnectionStatusConnecting <$ string "CONNECTING"
      , ConnectionStatusPausing    <$ string "PAUSING"
      , ConnectionStatusOnline     <$ string "ONLINE" ]

-- * ERROR
----------

p_error :: Parser SkypeResponse
p_error = Error <$> p_code <*> (p_description <|> pure "")
  where
    p_code = string "ERROR" *> spaces *> decimal

    p_description = spaces *> (T.decodeUtf8 <$> takeByteString)

-- * OPEN
---------

p_open :: Parser SkypeResponse
p_open = string "OPEN" *> spaces *> choice
  [ p_chat ]
  where
    p_chat = OpenChat <$> (string "CHAT" *> spaces *> p_chatID)

-- * PROTOCOL
-------------

p_protocol :: Parser SkypeResponse
p_protocol = Protocol <$> (string "PROTOCOL" *> spaces *> decimal)

-- * USER
---------

p_user :: Parser SkypeResponse
p_user = User <$> (string "USER" *> spaces *> p_userID <* spaces)
              <*> p_userProperty

p_userID :: Parser UserID
p_userID = takeWhile1 isSymbol
  where
    isSymbol c = any ($ c)
      [ isAlpha
      , isDigit
      , (==) _numbersign
      , (==) _hyphen
      , (==) _period
      ]

p_userProperty :: Parser UserProperty
p_userProperty = choice
  [ UserHandle                 <$> (property "HANDLE" *> p_userID)
  , UserFullName               <$> (property "FULLNAME" *> p_userFullName)
  , UserBirthday               <$> (property "BIRTHDAY" *> p_userBirthday)
  , UserSex                    <$> (property "SEX" *> p_userSex)
  , UserLanguage               <$> (property "LANGUAGE" *> p_userLanguage)
  , UserCountry                <$> (property "COUNTRY" *> p_userCountry)
  , UserProvince               <$> (property "PROVINCE" *> p_userProvince)
  , UserCity                   <$> (property "CITY" *> p_userCity)
  , UserPhoneHome              <$> (property "PHONE_HOME" *> p_userPhone)
  , UserPhoneOffice            <$> (property "PHONE_OFFICE" *> p_userPhone)
  , UserPhoneMobile            <$> (property "PHONE_MOBILE" *> p_userPhone)
  , UserHomepage               <$> (property "HOMEPAGE" *> p_userHomepage)
  , UserAbout                  <$> (property "ABOUT" *> p_userAbout)
  , UserHasCallEquipment       <$> (property "HASCALLEQUIPMENT" *> p_boolean)
  , UserIsVideoCapable         <$> (property "IS_VIDEO_CAPABLE" *> p_boolean)
  , UserIsVoicemailCapable     <$> (property "IS_VOICEMAIL_CAPABLE" *> p_boolean)
  , UserBuddyStatus            <$> (property "BUDDYSTATUS" *> p_userBuddyStatus)
  , UserIsAuthorized           <$> (property "ISAUTHORIZED" *> p_boolean)
  , UserIsBlocked              <$> (property "ISBLOCKED" *> p_boolean)
  , UserOnlineStatus           <$> (property "ONLINESTATUS" *> p_userOnlineStatus)
  , UserLastOnlineTimestamp    <$> (property "LASTONLINETIMESTAMP" *> p_timestamp)
  , UserCanLeaveVoiceMail      <$> (property "CAN_LEAVE_VM" *> p_boolean)
  , UserSpeedDial              <$> (property "SPEEDDIAL" *> p_userSpeedDial)
  , UserReceiveAuthRequest     <$> (property "RECEIVEDAUTHREQUEST" *> p_userAuthRequestMessage)
  , UserMoodText               <$> (property "MOOD_TEXT" *> p_userMoodText)
  , UserRichMoodText           <$> (property "RICH_MOOD_TEXT" *> p_userRichMoodText)
  , UserTimezone               <$> (property "TIMEZONE" *> p_userTimezoneOffset)
  , UserIsCallForwardingActive <$> (property "IS_CF_ACTIVE" *> p_boolean)
  , UserNumberOfAuthedBuddies  <$> (property "NROF_AUTHED_BUDDIES" *> decimal)
  , UserDisplayName            <$> (property "DISPLAYNAME" *> p_userDisplayName)
  ]
  where
    property prop = string prop *> spaces

p_userFullName :: Parser UserDisplayName
p_userFullName = takeText

p_userBirthday :: Parser (Maybe UserBirthday)
p_userBirthday = Just <$> (fromGregorian <$> digit 4 <*> digit 2 <*> digit 2)
             <|> Nothing <$ (word8 _0 *> endOfInput)
  where
    digit n = read . map (chr . fromIntegral) <$> count n (satisfy isDigit)

p_userSex :: Parser UserSex
p_userSex = choice
  [ UserSexUnknown <$ string "UNKNOWN"
  , UserSexMale    <$ string "MALE"
  , UserSexFemale  <$ string "FEMALE"
  ]

p_userStatus :: Parser SkypeResponse
p_userStatus = UserStatus <$> (string "USERSTATUS" *> spaces *> p_status)
  where
    p_status = choice
      [ UserStatusUnknown   <$ string "UNKNOWN"
      , UserStatusOnline    <$ string "ONLINE"
      , UserStatusOffline   <$ string "OFFLINE"
      , UserStatusSkypeMe   <$ string "SKYPEME"
      , UserStatusAway      <$ string "AWAY"
      , UserStatusNA        <$ string "NA"
      , UserStatusDND       <$ string "DND"
      , UserStatusInvisible <$ string "INVISIBLE"
      , UserStatusLoggedOut <$ string "LOGGEDOUT"
      ]

p_userLanguage :: Parser (UserLanguagePrefix, UserLanguage)
p_userLanguage = (,) <$> (tokens <* spaces) <*> tokens
  where
    tokens = T.decodeUtf8 <$> takeWhile1 isAlpha

p_userCountry :: Parser (UserCountryPrefix, UserCountry)
p_userCountry = (,) <$> (tokens <* spaces) <*> tokens
  where
    tokens = T.decodeUtf8 <$> takeWhile1 isAlpha

p_userProvince :: Parser UserProvince
p_userProvince = takeText

p_userCity :: Parser UserCity
p_userCity = takeText

p_userPhone :: Parser UserPhone
p_userPhone = takeText

p_userHomepage :: Parser UserHomepage
p_userHomepage = takeText

p_userAbout :: Parser UserAbout
p_userAbout = takeText

p_userBuddyStatus :: Parser UserBuddyStatus
p_userBuddyStatus = choice
  [ UserBuddyStatusNeverBeen <$ word8 _0
  , UserBuddyStatusDeleted   <$ word8 _1
  , UserBuddyStatusPending   <$ word8 _2
  , UserBuddyStatusAdded     <$ word8 _3
  ]

p_userOnlineStatus :: Parser UserOnlineStatus
p_userOnlineStatus = choice
  [ UserOnlineStatusUnknown      <$ string "UNKNOWN"
  , UserOnlineStatusOffline      <$ string "OFFLINE"
  , UserOnlineStatusOnline       <$ string "ONLINE"
  , UserOnlineStatusAway         <$ string "AWAY"
  , UserOnlineStatusNotAvailable <$ string "NA"
  , UserOnlineStatusDoNotDisturb <$ string "DND"
  ]

p_userSpeedDial :: Parser UserSpeedDial
p_userSpeedDial = takeText

p_userAuthRequestMessage :: Parser UserAuthRequestMessage
p_userAuthRequestMessage = takeText

p_userMoodText :: Parser UserMoodText
p_userMoodText = takeText

p_userRichMoodText :: Parser UserRichMoodText
p_userRichMoodText = takeText

p_userTimezoneOffset :: Parser UserTimezoneOffset
p_userTimezoneOffset = CTime <$> decimal

p_userDisplayName :: Parser UserDisplayName
p_userDisplayName = takeText

p_currentUserHandle :: Parser SkypeResponse
p_currentUserHandle = CurrentUserHandle <$>
                      (string "CURRENTUSERHANDLE" *> spaces *> p_userID)

-- * Internal Utilities
-----------------------

p_boolean :: Parser Bool
p_boolean = (True <$ string "TRUE") <|> (False <$ string "FALSE")

p_timestamp :: Parser Timestamp
p_timestamp = CTime <$> decimal

spaces :: Parser BS.ByteString
spaces = takeWhile1 isSpace

takeText :: Parser T.Text
takeText = T.decodeUtf8 <$> takeByteString
