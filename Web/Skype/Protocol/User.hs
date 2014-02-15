module Web.Skype.Protocol.User where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.Attoparsec.ByteString.Lazy
import Data.Word8
import Web.Skype.Protocol.Types

data UserProperty = UserHandle UserID
                  | UserFullName UserFullName
                  | UserBirthday (Maybe UserBirthday)
                  | UserSex UserSex
                  | UserLanguage (UserLanguagePrefix, UserLanguage)
                  | UserCountry (UserCountryPrefix, UserCountry)
                  | UserProvince UserProvince
                  | UserCity UserCity
                  | UserPhoneHome UserPhone
                  | UserPhoneOffice UserPhone
                  | UserPhoneMobile UserPhone
                  | UserHomepage UserHomepage
                  | UserAbout UserAbout
                  | UserHasCallEquipment Bool
                  | UserIsVideoCapable Bool
                  | UserIsVoicemailCapable Bool
                  | UserBuddyStatus UserBuddyStatus
                  | UserIsAuthorized Bool
                  | UserIsBlocked Bool
                  | UserOnlineStatus UserOnlineStatus
                  | UserLastOnlineTimestamp Timestamp
                  | UserCanLeaveVoiceMail Bool
                  | UserSpeedDial UserSpeedDial
                  | UserReceiveAuthRequest UserAuthRequestMessage
                  | UserMoodText UserMoodText
                  | UserRichMoodText UserRichMoodText
                  | UserTimezone UserTimezoneOffset
                  | UserIsCallForwardingActive Bool
                  | UserNumberOfAuthedBuddies Integer
                  | UserDisplayName UserDisplayName
  deriving (Eq, Show)

data UserSex = UserSexUnknown
             | UserSexMale
             | UserSexFemale
  deriving (Eq, Show)

data UserStatus = UserStatusUnknown    -- ^ no status information for current user.
                | UserStatusOnline     -- ^ current user is online.
                | UserStatusOffline    -- ^ current user is offline.
                | UserStatusSkypeMe    -- ^ current user is in "Skype Me" mode (Protocol 2).
                | UserStatusAway       -- ^ current user is away.
                | UserStatusNA         -- ^ current user is not available.
                | UserStatusDND        -- ^ current user is in "Do not disturb" mode.
                | UserStatusInvisible  -- ^ current user is invisible to others.
                | UserStatusLoggedOut  -- ^ current user is logged out. Clients are detached.
  deriving (Eq, Show)

data UserBuddyStatus = UserBuddyStatusNeverBeen
                     | UserBuddyStatusDeleted
                     | UserBuddyStatusPending
                     | UserBuddyStatusAdded
  deriving (Eq, Show)

data UserOnlineStatus = UserOnlineStatusUnknown
                      | UserOnlineStatusOffline
                      | UserOnlineStatusOnline
                      | UserOnlineStatusAway
                      | UserOnlineStatusNotAvailable
                      | UserOnlineStatusDoNotDisturb
  deriving (Eq, Show)

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

p_userSex :: Parser UserSex
p_userSex = choice
  [ UserSexUnknown <$ string "UNKNOWN"
  , UserSexMale    <$ string "MALE"
  , UserSexFemale  <$ string "FEMALE"
  ]

p_userStatus :: Parser UserStatus
p_userStatus = choice
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
