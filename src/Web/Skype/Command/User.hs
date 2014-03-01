module Web.Skype.Command.User where

import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Monoid ((<>))
import Web.Skype.Command.Utils
import Web.Skype.Core
import Web.Skype.Protocol

import qualified Data.Text.Encoding as T

-- | Gets the full name of the user.
getFullName :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
            => UserID
            -> SkypeT m UserFullName
getFullName userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserFullName fullName) -> return $ Just fullName
    _                              -> return Nothing
  where
    command = "GET USER " <> userID <> " FULLNAME"

-- | Gets the birthdate of the user.
getBirthday :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
            => UserID
            -> SkypeT m (Maybe UserBirthday)
getBirthday userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserBirthday birthday) -> return $ Just birthday
    _                              -> return Nothing
  where
    command = "GET USER " <> userID <> " BIRTHDAY"

-- | Gets the sex of the user.
getSex :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
       => UserID
       -> SkypeT m UserSex
getSex userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserSex sex) -> return $ Just sex
    _                    -> return Nothing
  where
    command = "GET USER " <> userID <> " SEX"

-- | Gets the native language of the user.
getLanguage :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
            => UserID
            -> SkypeT m (Maybe (UserLanguageISOCode, UserLanguage))
getLanguage userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserLanguage language) -> return $ Just language
    _                              -> return Nothing
  where
    command = "GET USER " <> userID <> " LANGUAGE"

-- | Gets the country the user is based.
getCountry :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
           => UserID
           -> SkypeT m (Maybe (UserCountryISOCode, UserCountry))
getCountry userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserCountry country) -> return $ Just country
    _                            -> return Nothing
  where
    command = "GET USER " <> userID <> " COUNTRY"

-- | Gets the province the user is based.
getProvince :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
            => UserID
            -> SkypeT m UserProvince
getProvince userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserProvince province) -> return $ Just province
    _                              -> return Nothing
  where
    command = "GET USER " <> userID <> " PROVINCE"

-- | Gets the city this user is based in.
getCity :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
        => UserID
        -> SkypeT m UserCity
getCity userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserCity city) -> return $ Just city
    _                      -> return Nothing
  where
    command = "GET USER " <> userID <> " CITY"

-- | Gets the home phone number that is in the user profile.
getHomePhone :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
             => UserID
             -> SkypeT m UserPhone
getHomePhone userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserHomePhone phone) -> return $ Just phone
    _                            -> return Nothing
  where
    command = "GET USER " <> userID <> " PHONE_HOME"

-- | Gets the office phone number that is in the user profile.
getOfficePhone :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
             => UserID
             -> SkypeT m UserPhone
getOfficePhone userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserOfficePhone phone) -> return $ Just phone
    _                              -> return Nothing
  where
    command = "GET USER " <> userID <> " PHONE_OFFICE"

-- | Gets the mobile phone number of the user.
getMobilePhone :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
                => UserID
                -> SkypeT m UserPhone
getMobilePhone userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserMobilePhone phone) -> return $ Just phone
    _                              -> return Nothing
  where
    command = "GET USER " <> userID <> " PHONE_MOBILE"

-- | Gets the homepage URL of the user.
getHomepage :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
            => UserID
            -> SkypeT m UserHomepage
getHomepage userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserHomepage homepage) -> return $ Just homepage
    _                              -> return Nothing
  where
    command = "GET USER " <> userID <> " HOMEPAGE"

-- | Gets extra information user has provided in his/her profile.
getAbout :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
         => UserID
         -> SkypeT m UserAbout
getAbout userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserAbout about) -> return $ Just about
    _                        -> return Nothing
  where
    command = "GET USER " <> userID <> " ABOUT"

-- | Checks if the user is video-capable.
isVideoCapable :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
               => UserID
               -> SkypeT m Bool
isVideoCapable userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserIsVideoCapable videoCapable) -> return $ Just videoCapable
    _                                        -> return Nothing
  where
    command = "GET USER " <> userID <> " IS_VIDEO_CAPABLE"

-- | Checks if the user is voicemail-capable.
isVoicemailCapable :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
                   => UserID
                   -> SkypeT m Bool
isVoicemailCapable userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserIsVoicemailCapable voicemailCapable) -> return $ Just voicemailCapable
    _                                                -> return Nothing
  where
    command = "GET USER " <> userID <> " IS_VOICEMAIL_CAPABLE"

-- | Gets the buddy status of the user.
getBuddyStatus :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
               => UserID
               -> SkypeT m UserBuddyStatus
getBuddyStatus userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserBuddyStatus displayName) -> return $ Just displayName
    _                                    -> return Nothing
  where
    command = "GET USER " <> userID <> " BUDDYSTATUS"

-- | Removes target from contactlist.
removeFromContactList :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
                      => UserID
                      -> SkypeT m ()
removeFromContactList userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserBuddyStatus _) -> return $ Just ()
    _                          -> return Nothing
  where
    command = "SET USER " <> userID <> " BUDDYSTATUS 1"

-- | Adds target into contactlist and ask authorization with message.
askAuthorization :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
                 => UserID
                 -> UserAuthRequestMessage
                 -> SkypeT m ()
askAuthorization userID message = executeCommandWithID command $ \response ->
  case response of
    User _ (UserBuddyStatus _) -> return $ Just ()
    _                          -> return Nothing
  where
    command = "SET USER " <> userID <> " BUDDYSTATUS 2"
                          <> T.encodeUtf8 message

-- | Checks if the user is authorized in your contactlist.
isAuthorized :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
             => UserID
             -> SkypeT m Bool
isAuthorized userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserIsAuthorized authorized) -> return $ Just authorized
    _                                    -> return Nothing
  where
    command = "GET USER " <> userID <> " ISAUTHORIZED"

-- | Set the user being authorized, or not in your contactlist.
setAuthorized :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
              => UserID
              -> Bool
              -> SkypeT m ()
setAuthorized userID authorized = executeCommandWithID command $ \response ->
  case response of
    User _ (UserIsAuthorized _) -> return $ Just ()
    _                           -> return Nothing
  where
    command = "SET USER " <> userID <> " ISAUTHORIZED "
                          <> if authorized then "TRUE" else "FALSE"

-- | Checks if the user is blocked in your contactlist.
isBlocked :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
          => UserID
          -> SkypeT m Bool
isBlocked userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserIsBlocked blocked) -> return $ Just blocked
    _                              -> return Nothing
  where
    command = "GET USER " <> userID <> " ISBLOCKED"

-- | Sets the user being blocked, or not in your contactlist.
setBlocked :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
           => UserID
           -> Bool
           -> SkypeT m ()
setBlocked userID blocked = executeCommandWithID command $ \response ->
  case response of
    User _ (UserIsBlocked _) -> return $ Just ()
    _                        -> return Nothing
  where
    command = "SET USER " <> userID <> " ISBLOCKED "
                          <> (if blocked then "TRUE" else "FALSE")

-- | Gets the online status of the user.
getOnlineStatus :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
                => UserID
                -> SkypeT m UserOnlineStatus
getOnlineStatus userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserOnlineStatus status) -> return $ Just status
    _                                -> return Nothing
  where
    command = "GET USER " <> userID <> " ONLINESTATUS"

-- | Gets last online timestamp of the user.
getLastOnlineTimestamp :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
                       => UserID
                       -> SkypeT m Timestamp
getLastOnlineTimestamp userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserLastOnlineTimestamp lastOnlineTime) -> return $ Just lastOnlineTime
    _                                               -> return Nothing
  where
    command = "GET USER " <> userID <> " LASTONLINETIMESTAMP"

-- | Indicates whether the current user can send voicemail to the user.
canLeaveVoicemail :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
                  => UserID
                  -> SkypeT m Bool
canLeaveVoicemail userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserCanLeaveVoicemail canLeave) -> return $ Just canLeave
    _                                       -> return Nothing
  where
    command = "GET USER " <> userID <> " CAN_LEAVE_VM"

-- | Gets the speed dial of the user.
getSpeedDial :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
             => UserID
             -> SkypeT m UserSpeedDial
getSpeedDial userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserSpeedDial speedDial) -> return $ Just speedDial
    _                                -> return Nothing
  where
    command = "GET USER " <> userID <> " SPEEDDIAL"

-- | Sets the speed dial of the user.
setSpeedDial :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
             => UserID
             -> UserSpeedDial
             -> SkypeT m ()
setSpeedDial userID speedDial = executeCommandWithID command $ \response ->
  case response of
    User _ (UserSpeedDial _) -> return $ Just ()
    _                        -> return Nothing
  where
    command = "SET USER " <> userID <> " SPEEDDIAL " <> (T.encodeUtf8 speedDial)

-- | Gets the mood message of the user.
getMoodText :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
            => UserID
            -> SkypeT m UserMoodText
getMoodText userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserMoodText moodText) -> return $ Just moodText
    _                              -> return Nothing
  where
    command = "GET USER " <> userID <> " MOOD_TEXT"

-- | Gets the time zone of the user.
getTimezone :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
            => UserID
            -> SkypeT m UserTimezoneOffset
getTimezone userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserTimezone timezone) -> return $ Just timezone
    _                              -> return Nothing
  where
    command = "GET USER " <> userID <> " TIMEZONE"

-- | Indicates whether the user has call forwarding activated or not.
isCallForwardingActive :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
                       => UserID
                       -> SkypeT m Bool
isCallForwardingActive userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserIsCallForwardingActive callForwardingActive) -> return $ Just callForwardingActive
    _                                                        -> return Nothing
  where
    command = "GET USER " <> userID <> " IS_CF_ACTIVE"


-- | Gets the number of authorized contacts in the contact list.
getNumberOfAuthedBuddies :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
                         => UserID
                         -> SkypeT m Int
getNumberOfAuthedBuddies userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserNumberOfAuthedBuddies numberOfAuthedBuddies) -> return $ Just numberOfAuthedBuddies
    _                                                        -> return Nothing
  where
    command = "GET USER " <> userID <> " NROF_AUTHED_BUDDIES"

-- | Gets the display name of the user.
getDisplayName :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
               => UserID
               -> SkypeT m UserDisplayName
getDisplayName userID = executeCommandWithID command $ \response ->
  case response of
    User _ (UserDisplayName displayName) -> return $ Just displayName
    _                                    -> return Nothing
  where
    command = "GET USER " <> userID <> " DISPLAYNAME"

-- | Sets the display name of the user.
setDisplayName :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
               => UserID
               -> UserDisplayName
               -> SkypeT m ()
setDisplayName userID displayName = executeCommandWithID command $ \response ->
  case response of
    User _ (UserDisplayName _) -> return $ Just ()
    _                          -> return Nothing
  where
    command = "SET USER " <> userID <> " DISPLAYNAME " <> (T.encodeUtf8 displayName)

-- | Gets the user name for the currently logged in user.
getCurrentUserHandle :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
                     => SkypeT m UserID
getCurrentUserHandle = executeCommandWithID command $ \response ->
  case response of
    CurrentUserHandle userID -> return $ Just userID
    _                        -> return Nothing
  where
    command = "GET CURRENTUSERHANDLE"

-- | Returns a list of found usernames.
searchAllFriends :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
                 => SkypeT m [UserID]
searchAllFriends = executeCommandWithID command $ \response ->
  case response of
    Users userIDs -> return $ Just userIDs
    _             -> return Nothing
  where
    command = "SEARCH FRIENDS"
