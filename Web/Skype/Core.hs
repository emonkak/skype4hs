module Web.Skype.Core (
  MonadSkype(..),
  Command,
  CommandID,
  Skype(..),
  SkypeChannel,
  SkypeEnvironment(..),
  SkypeAttachStatus(..),
  duplicateChannel,
  runSkype,
  withSkype
) where

import Control.Applicative (Applicative)
import Control.Concurrent.STM.TChan (TChan, dupTChan)
import Control.Monad.Reader (MonadReader(..), ReaderT(..), runReaderT)
import Control.Monad.STM (atomically)
import Control.Monad.Trans (MonadIO, MonadTrans, liftIO)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

newtype Skype c m a = Skype (ReaderT (SkypeEnvironment c) m a)
  deriving (Applicative, Functor, Monad, MonadIO, MonadTrans)

instance Monad m => MonadReader (SkypeEnvironment c) (Skype c m) where
  ask = Skype ask
  local f (Skype m) = Skype $ local f m
  reader = Skype . reader

data SkypeEnvironment a = SkypeEnvironment
  { skypeTimeout :: Int
  , skypeConnection :: a
  }

data SkypeAttachStatus = SkypeAttached
                       | SkypeNotAvailable
                       | SkypeRefused
  deriving (Eq, Show)

type SkypeChannel = TChan BL.ByteString

type Command = BS.ByteString
type CommandID = BS.ByteString

-- | Provides the DSL for Skype API.
class Monad m => MonadSkype m where
  -- | Attach to the skype instance.
  attach :: m SkypeAttachStatus

  -- | Sends the command message to the Skype instance.
  sendCommand :: Command -> m ()

  -- | Gets the message channel of Skype from the event loop.
  getChannel :: m SkypeChannel

-- | runSkype
runSkype :: Skype c m a -> SkypeEnvironment c -> m a
runSkype (Skype skype) = runReaderT skype

-- | withSkype
withSkype :: SkypeEnvironment c -> Skype c m a -> m a
withSkype = flip runSkype

-- | duplicateChannel
duplicateChannel :: (MonadIO m, MonadSkype m) => m SkypeChannel
duplicateChannel = getChannel >>= liftIO . atomically . dupTChan
