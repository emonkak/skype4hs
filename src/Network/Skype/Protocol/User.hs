module Network.Skype.Protocol.User where

import Data.Typeable (Typeable)
import Network.Skype.Protocol.Types

data UserProperty = UserHandle UserID
                  | UserFullName UserFullName
                  | UserBirthday (Maybe UserBirthday)
                  | UserSex UserSex
                  | UserLanguage (Maybe (UserLanguageISOCode, UserLanguage))
                  | UserCountry (Maybe (UserCountryISOCode, UserCountry))
                  | UserProvince UserProvince
                  | UserCity UserCity
                  | UserHomePhone UserPhone
                  | UserOfficePhone UserPhone
                  | UserMobilePhone UserPhone
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
                  | UserCanLeaveVoicemail Bool
                  | UserSpeedDial UserSpeedDial
                  | UserReceiveAuthRequest UserAuthRequestMessage
                  | UserMoodText UserMoodText
                  | UserRichMoodText UserRichMoodText
                  | UserTimezone UserTimezoneOffset
                  | UserIsCallForwardingActive Bool
                  | UserNumberOfAuthedBuddies Int
                  | UserDisplayName UserDisplayName
  deriving (Eq, Show, Typeable)

data UserSex = UserSexUnknown
             | UserSexMale
             | UserSexFemale
  deriving (Eq, Show, Typeable)

data UserStatus = UserStatusUnknown       -- ^ no status information for current user.
                | UserStatusOnline        -- ^ current user is online.
                | UserStatusOffline       -- ^ current user is offline.
                | UserStatusSkypeMe       -- ^ current user is in "Skype Me" mode (Protocol 2).
                | UserStatusAway          -- ^ current user is away.
                | UserStatusNotAvailable  -- ^ current user is not available.
                | UserStatusDoNotDisturb  -- ^ current user is in "Do not disturb" mode.
                | UserStatusInvisible     -- ^ current user is invisible to others.
                | UserStatusLoggedOut     -- ^ current user is logged out. Clients are detached.
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
