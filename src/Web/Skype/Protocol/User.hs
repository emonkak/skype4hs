module Web.Skype.Protocol.User where

import Data.Typeable (Typeable)
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
  deriving (Eq, Show, Typeable)

data UserSex = UserSexUnknown
             | UserSexMale
             | UserSexFemale
  deriving (Eq, Show, Typeable)

data UserStatus = UserStatusUnknown    -- ^ no status information for current user.
                | UserStatusOnline     -- ^ current user is online.
                | UserStatusOffline    -- ^ current user is offline.
                | UserStatusSkypeMe    -- ^ current user is in "Skype Me" mode (Protocol 2).
                | UserStatusAway       -- ^ current user is away.
                | UserStatusNA         -- ^ current user is not available.
                | UserStatusDND        -- ^ current user is in "Do not disturb" mode.
                | UserStatusInvisible  -- ^ current user is invisible to others.
                | UserStatusLoggedOut  -- ^ current user is logged out. Clients are detached.
  deriving (Eq, Show, Typeable)

data UserBuddyStatus = UserBuddyStatusNeverBeen
                     | UserBuddyStatusDeleted
                     | UserBuddyStatusPending
                     | UserBuddyStatusAdded
  deriving (Eq, Show, Typeable)

data UserOnlineStatus = UserOnlineStatusUnknown
                      | UserOnlineStatusOffline
                      | UserOnlineStatusOnline
                      | UserOnlineStatusAway
                      | UserOnlineStatusNotAvailable
                      | UserOnlineStatusDoNotDisturb
  deriving (Eq, Show, Typeable)
