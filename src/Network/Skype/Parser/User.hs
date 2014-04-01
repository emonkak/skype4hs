module Network.Skype.Parser.User where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (anyChar, decimal)
import Data.Attoparsec.ByteString.Lazy
import Data.Word8
import Network.Skype.Parser.Types
import Network.Skype.Protocol.User

userProperty :: Parser UserProperty
userProperty = choice
  [ UserHandle                 <$> (property "HANDLE" *> userID)
  , UserFullName               <$> (property "FULLNAME" *> userFullName)
  , UserBirthday               <$> (property "BIRTHDAY" *> userBirthday)
  , UserSex                    <$> (property "SEX" *> userSex)
  , UserLanguage               <$> (property "LANGUAGE" *> userLanguage)
  , UserCountry                <$> (property "COUNTRY" *> userCountry)
  , UserProvince               <$> (property "PROVINCE" *> userProvince)
  , UserCity                   <$> (property "CITY" *> userCity)
  , UserHomePhone              <$> (property "PHONE_HOME" *> userPhone)
  , UserOfficePhone            <$> (property "PHONE_OFFICE" *> userPhone)
  , UserMobilePhone            <$> (property "PHONE_MOBILE" *> userPhone)
  , UserHomepage               <$> (property "HOMEPAGE" *> userHomepage)
  , UserAbout                  <$> (property "ABOUT" *> userAbout)
  , UserHasCallEquipment       <$> (property "HASCALLEQUIPMENT" *> boolean)
  , UserIsVideoCapable         <$> (property "IS_VIDEO_CAPABLE" *> boolean)
  , UserIsVoicemailCapable     <$> (property "IS_VOICEMAIL_CAPABLE" *> boolean)
  , UserBuddyStatus            <$> (property "BUDDYSTATUS" *> userBuddyStatus)
  , UserIsAuthorized           <$> (property "ISAUTHORIZED" *> boolean)
  , UserIsBlocked              <$> (property "ISBLOCKED" *> boolean)
  , UserOnlineStatus           <$> (property "ONLINESTATUS" *> userOnlineStatus)
  , UserLastOnlineTimestamp    <$> (property "LASTONLINETIMESTAMP" *> timestamp)
  , UserCanLeaveVoicemail      <$> (property "CAN_LEAVE_VM" *> boolean)
  , UserSpeedDial              <$> (property "SPEEDDIAL" *> userSpeedDial)
  , UserReceiveAuthRequest     <$> (property "RECEIVEDAUTHREQUEST" *> userAuthRequestMessage)
  , UserMoodText               <$> (property "MOOD_TEXT" *> userMoodText)
  , UserRichMoodText           <$> (property "RICH_MOOD_TEXT" *> userRichMoodText)
  , UserTimezone               <$> (property "TIMEZONE" *> userTimezoneOffset)
  , UserIsCallForwardingActive <$> (property "IS_CF_ACTIVE" *> boolean)
  , UserNumberOfAuthedBuddies  <$> (property "NROF_AUTHED_BUDDIES" *> decimal)
  , UserDisplayName            <$> (property "DISPLAYNAME" *> userDisplayName)
  ]
  where
    property prop = string prop *> spaces

userSex :: Parser UserSex
userSex = choice
  [ UserSexUnknown <$ string "UNKNOWN"
  , UserSexMale    <$ string "MALE"
  , UserSexFemale  <$ string "FEMALE"
  ]

userStatus :: Parser UserStatus
userStatus = choice
  [ UserStatusUnknown      <$ string "UNKNOWN"
  , UserStatusOnline       <$ string "ONLINE"
  , UserStatusOffline      <$ string "OFFLINE"
  , UserStatusSkypeMe      <$ string "SKYPEME"
  , UserStatusAway         <$ string "AWAY"
  , UserStatusNotAvailable <$ string "NA"
  , UserStatusDoNotDisturb <$ string "DND"
  , UserStatusInvisible    <$ string "INVISIBLE"
  , UserStatusLoggedOut    <$ string "LOGGEDOUT"
  ]

userBuddyStatus :: Parser UserBuddyStatus
userBuddyStatus = choice
  [ UserBuddyStatusNeverBeen <$ word8 _0
  , UserBuddyStatusDeleted   <$ word8 _1
  , UserBuddyStatusPending   <$ word8 _2
  , UserBuddyStatusAdded     <$ word8 _3
  ]

userOnlineStatus :: Parser UserOnlineStatus
userOnlineStatus = choice
  [ UserOnlineStatusUnknown      <$ string "UNKNOWN"
  , UserOnlineStatusOffline      <$ string "OFFLINE"
  , UserOnlineStatusOnline       <$ string "ONLINE"
  , UserOnlineStatusAway         <$ string "AWAY"
  , UserOnlineStatusNotAvailable <$ string "NA"
  , UserOnlineStatusDoNotDisturb <$ string "DND"
  ]

userAvater :: Parser (Int, FilePath)
userAvater = (,) <$> decimal <*> many anyChar
