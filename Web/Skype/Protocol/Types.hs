module Web.Skype.Protocol.Types where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.Attoparsec.ByteString.Lazy
import Data.Char (chr)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Word8
import Foreign.C.Types (CTime(..))
import System.Posix.Types (EpochTime)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- * User

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

p_userID :: Parser UserID
p_userID = takeWhile1 symbol
  where
    symbol c = any ($ c)
      [ isAlpha
      , isDigit
      , (==) _numbersign
      , (==) _hyphen
      , (==) _period
      , (==) _underscore
      ]

p_userFullName :: Parser UserDisplayName
p_userFullName = takeText

p_userDisplayName :: Parser UserDisplayName
p_userDisplayName = takeText

p_userBirthday :: Parser (Maybe UserBirthday)
p_userBirthday = Just <$> (fromGregorian <$> digit 4 <*> digit 2 <*> digit 2)
             <|> Nothing <$ (word8 _0 *> endOfInput)
  where
    digit n = read . map (chr . fromIntegral) <$> count n (satisfy isDigit)

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

p_userAbout :: Parser UserAbout
p_userAbout = takeText

p_userHomepage :: Parser UserHomepage
p_userHomepage = takeText

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




-- * Chat

type ChatID = BS.ByteString
type ChatTopic = T.Text
type ChatWindowTitle = T.Text
type ChatPasswordHint = T.Text
type ChatGuidelines = T.Text
type ChatDescription = T.Text
type ChatBlob = BS.ByteString

p_chatID :: Parser ChatID
p_chatID = takeWhile1 $ symbol
  where
    symbol c = any ($ c)
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

p_chatTopic :: Parser ChatTopic
p_chatTopic = takeText

p_chatWindowTitle :: Parser ChatWindowTitle
p_chatWindowTitle = takeText

p_chatMemberID :: Parser ChatMemberID
p_chatMemberID = decimal

p_chatPasswordHint :: Parser ChatPasswordHint
p_chatPasswordHint = takeText

p_chatGuidelines :: Parser ChatGuidelines
p_chatGuidelines = takeText

p_chatDescription :: Parser ChatDescription
p_chatDescription = takeText

p_chatBlob :: Parser ChatBlob
p_chatBlob = takeByteString




-- * Chat member

type ChatMemberID = Integer




-- * Chat message

type ChatMessageID = Integer
type ChatMessageBody = T.Text

p_chatMessageID :: Parser ChatMessageID
p_chatMessageID = decimal

p_chatMessageBody :: Parser ChatMessageBody
p_chatMessageBody = takeText




-- * Misc.

type Timestamp = EpochTime

type ErrorCode = Int
type ErrorDescription = T.Text

type ProtocolVersion = Int

p_boolean :: Parser Bool
p_boolean = (True <$ string "TRUE") <|> (False <$ string "FALSE")

p_timestamp :: Parser Timestamp
p_timestamp = CTime <$> decimal




-- * Utilities

spaces :: Parser BS.ByteString
spaces = takeWhile1 isSpace

takeText :: Parser T.Text
takeText = T.decodeUtf8 <$> takeByteString
