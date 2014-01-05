module Web.Skype.Protocol.Types where

import Data.Time.Calendar (Day)
import System.Posix.Types (EpochTime)

import qualified Data.Text as T
import qualified Data.ByteString as BS

type UserID = BS.ByteString
type UserHandle = T.Text
type UserFullName = T.Text
type UserDisplayName = T.Text
type UserBirthday = Day
type UserLanguage = T.Text
type UserLanguagePrefix = T.Text
type UserCountry = T.Text
type UserCountryPrefix = T.Text
type UserProvince = T.Text
type UserCity = T.Text
type UserPhone = T.Text
type UserAbout = T.Text
type UserHomepage = T.Text
type UserSpeedDial = T.Text
type UserAuthRequestMessage = T.Text
type UserMoodText = T.Text
type UserRichMoodText = T.Text
type UserTimezoneOffset = EpochTime

type ChatID = BS.ByteString
type ChatTopic = T.Text
type ChatWindowTitle = T.Text
type ChatPasswordHint = T.Text
type ChatGuidelines = T.Text
type ChatDescription = T.Text
type ChatBlob = BS.ByteString

type ChatMessageID = Integer
type ChatMessageBody = T.Text

type ChatMemberID = Integer

type Timestamp = EpochTime

type ErrorCode = Int
type ErrorDescription = T.Text
