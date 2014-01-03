module Web.Skype.Protocol.Types where

import System.Posix.Types (EpochTime)

import qualified Data.Text as T
import qualified Data.ByteString as BS

type UserID = BS.ByteString
type UserHandle = T.Text
type UserDisplayName = T.Text

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
