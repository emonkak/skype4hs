module Web.Skype.Parser.ChatMessage where

import Control.Applicative
import Data.Attoparsec.ByteString.Lazy
import Web.Skype.Parser.Chat
import Web.Skype.Parser.Types
import Web.Skype.Protocol.ChatMessage

chatMessageProperty :: Parser ChatMessageProperty
chatMessageProperty = choice
  [ ChatMessageTimestamp       <$> (property "TIMESTAMP" *> timestamp)
  , ChatMessageFromHandle      <$> (property "FROM_HANDLE" *> userID)
  , ChatMessageFromDisplayName <$> (property "FROM_DISPNAME" *> userDisplayName)
  , ChatMessageType            <$> (property "TYPE" *> chatMessageType)
  , ChatMessageStatus          <$> (property "STATUS" *> chatMessageStatus)
  , ChatMessageLeaveReason     <$> (property "LEAVEREASON" *> (Just <$> chatMessageLeaveReason <|> pure Nothing))
  , ChatMessageChatName        <$> (property "CHATNAME" *> chatID)
  , ChatMessageUsers           <$> (property "USERS" *> userIDs)
  , ChatMessageIsEditable      <$> (property "IS_EDITABLE" *> boolean)
  , ChatMessageEditedBy        <$> (property "EDITED_BY" *> userID)
  , ChatMessageEditedTimestamp <$> (property "EDITED_TIMESTAMP" *> timestamp)
  , ChatMessageOptions         <$> (property "OPTIONS" *> chatOptions)
  , ChatMessageRole            <$> (property "ROLE" *> chatRole)
  , ChatMessageSeen            <$  (string "SEEN" *> endOfInput)
  , ChatMessageBody            <$> (property "BODY" *> chatMessageBody)
  ]
  where
    property prop = string prop *> spaces

    userIDs = userID `sepBy` spaces

chatMessageType :: Parser ChatMessageType
chatMessageType = choice
  [ ChatMessageTypeSetTopic          <$ string "SETTOPIC"
  , ChatMessageTypeSaid              <$ string "SAID"
  , ChatMessageTypeAddedMembers      <$ string "ADDEDMEMBERS"
  , ChatMessageTypeSawMembers        <$ string "SAWMEMBERS"
  , ChatMessageTypeCreatedChatWith   <$ string "CREATEDCHATWITH"
  , ChatMessageTypeLeft              <$ string "LEFT"
  , ChatMessageTypePostedContacts    <$ string "POSTEDCONTACTS"
  , ChatMessageTypeGapInChat         <$ string "GAiN_CHAT"
  , ChatMessageTypeSetRole           <$ string "SETROLE"
  , ChatMessageTypeKicked            <$ string "KICKED"
  , ChatMessageTypeKickBanned        <$ string "KICKBANNED"
  , ChatMessageTypeSetOptions        <$ string "SETOPTIONS"
  , ChatMessageTypeSetPicture        <$ string "SETPICTURE"
  , ChatMessageTypeSetGuideLines     <$ string "SETGUIDELINES"
  , ChatMessageTypeJoinedAsApplicant <$ string "JOINEDASAPPLICANT"
  , ChatMessageTypeEmoted            <$ string "EMOTED"
  , ChatMessageTypeUnkown            <$ string "UNKNOWN"
  ]

chatMessageStatus :: Parser ChatMessageStatus
chatMessageStatus = choice
  [ ChatMessageStatusSending <$ string "SENDING"
  , ChatMessageStatusSent    <$ string "SENT"
  , ChatMessageStatusReceive <$ string "RECEIVE"
  , ChatMessageStatusRead    <$ string "READ"
  ]

chatMessageLeaveReason :: Parser ChatMessageLeaveReason
chatMessageLeaveReason = choice
  [ ChatMessageLeaveReasonUserNotFound          <$ string "USER_NOT_FOUND"
  , ChatMessageLeaveReasonUserIncapable         <$ string "USER_INCAPABLE"
  , ChatMessageLeaveReasonAdderMustBeFriend     <$ string "ADDER_MUST_BE_FRIEND"
  , ChatMessageLeaveReasonAdderMustBeAuthorized <$ string "ADDED_MUST_BE_AUTHORIZED"
  , ChatMessageLeaveReasonUnsubscribe           <$ string "UNSUBSCRIBE"
  ]
