module Web.Skype.Parser.Types where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.Attoparsec.ByteString.Lazy
import Data.Char (chr)
import Data.Time.Calendar (fromGregorian)
import Data.Word8
import Web.Skype.Protocol.Types

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- * User

userID :: Parser UserID
userID = takeWhile1 symbol
  where
    symbol c = any ($ c)
      [ isAlpha
      , isDigit
      , (==) _numbersign
      , (==) _hyphen
      , (==) _period
      , (==) _underscore
      , (==) _colon
      ]

userFullName :: Parser UserDisplayName
userFullName = takeText

userDisplayName :: Parser UserDisplayName
userDisplayName = takeText

userBirthday :: Parser (Maybe UserBirthday)
userBirthday = Just <$> (fromGregorian <$> digit 4 <*> digit 2 <*> digit 2)
             <|> Nothing <$ (word8 _0 *> endOfInput)
  where
    digit n = read . map (chr . fromIntegral) <$> count n (satisfy isDigit)

userLanguage :: Parser (Maybe (UserLanguageISOCode, UserLanguage))
userLanguage = Just <$> ((,) <$> (tokens <* spaces) <*> tokens) <|> pure Nothing
  where
    tokens = T.decodeUtf8 <$> takeWhile1 isAlpha

userCountry :: Parser (Maybe (UserCountryISOCode, UserCountry))
userCountry = Just <$> ((,) <$> (tokens <* spaces) <*> tokens) <|> pure Nothing
  where
    tokens = T.decodeUtf8 <$> takeWhile1 isAlpha

userProvince :: Parser UserProvince
userProvince = takeText

userCity :: Parser UserCity
userCity = takeText

userPhone :: Parser UserPhone
userPhone = takeText

userAbout :: Parser UserAbout
userAbout = takeText

userHomepage :: Parser UserHomepage
userHomepage = takeText

userSpeedDial :: Parser UserSpeedDial
userSpeedDial = takeText

userAuthRequestMessage :: Parser UserAuthRequestMessage
userAuthRequestMessage = takeText

userMoodText :: Parser UserMoodText
userMoodText = takeText

userRichMoodText :: Parser UserRichMoodText
userRichMoodText = takeText

userTimezoneOffset :: Parser UserTimezoneOffset
userTimezoneOffset = decimal

-- * Chat

chatID :: Parser ChatID
chatID = takeWhile1 $ symbol
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

chatTopic :: Parser ChatTopic
chatTopic = takeText

chatWindowTitle :: Parser ChatWindowTitle
chatWindowTitle = takeText

chatPasswordHint :: Parser ChatPasswordHint
chatPasswordHint = takeText

chatGuidelines :: Parser ChatGuidelines
chatGuidelines = takeText

chatDescription :: Parser ChatDescription
chatDescription = takeText

chatBlob :: Parser ChatBlob
chatBlob = takeByteString

-- * Chat member

chatMemberID :: Parser ChatMessageID
chatMemberID = decimal

-- * Chat message

chatMessageID :: Parser ChatMessageID
chatMessageID = decimal

chatMessageBody :: Parser ChatMessageBody
chatMessageBody = takeText

-- * Misc.

boolean :: Parser Bool
boolean = (True <$ string "TRUE") <|> (False <$ string "FALSE")

timestamp :: Parser Timestamp
timestamp = decimal

spaces :: Parser BS.ByteString
spaces = takeWhile1 isSpace

takeText :: Parser T.Text
takeText = T.decodeUtf8 <$> takeByteString
