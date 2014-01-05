module Web.Skype.Protocol.User where

import Web.Skype.Protocol.Types

data UserProperty
  = UserHandle UserID
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
