module Web.Skype.Protocol.Types where

import System.Posix.Types (EpochTime)

import qualified Data.Text as T
import qualified Data.ByteString as BS

newtype UserID = UserID { getUserID :: BS.ByteString }
  deriving (Eq, Show)

newtype ChatID = ChatID { getChatID :: BS.ByteString}
  deriving (Eq, Show)

newtype ChatMessageID = ChatMessageID { getMessageID :: Integer }
  deriving (Eq, Show)

newtype ChatMemberID = ChatMemberID { getChatMemberID :: Integer }
  deriving (Eq, Show)

type UserHandle = T.Text
type UserDisplayName = T.Text

type ChatTopic = T.Text
type ChatWindowTitle = T.Text
type ChatPasswordHint = T.Text
type ChatGuidelines = T.Text
type ChatDescription = T.Text
type ChatBlob = BS.ByteString

type ChatMessageBody = T.Text

type Timestamp = EpochTime

type ErrorCode = Int
type ErrorDescription = T.Text
