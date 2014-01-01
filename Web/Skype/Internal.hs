module Web.Skype.Internal (
  Command,
  CommandID,
  MonadSkype(..),
  Skype(..),
  SkypeAttachStatus(..),
  SkypeChannel,
  SkypeEnvironment(..),
  dupSkypeChannel,
  runSkype,
  withSkype
) where

import Control.Applicative (Applicative)
import Control.Concurrent.STM.TChan (TChan, dupTChan)
import Control.Monad.Reader (MonadReader(..), ReaderT(..), runReaderT)
import Control.Monad.STM (atomically)
import Control.Monad.Trans (MonadIO, liftIO)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

type Command = BS.ByteString
type CommandID = BS.ByteString

type SkypeChannel = TChan BL.ByteString

-- | Provides the DSL for Skype API.
class (Monad m) => MonadSkype m where
  -- | Attach to the skype instance.
  attach :: m SkypeAttachStatus

  -- | Sends the command message to the Skype instance.
  sendCommand :: Command -> m ()

  -- | Gets the message channel of Skype from the event loop.
  getSkypeChannel :: m SkypeChannel

  -- | Get the timeout period of an command (milliseconds).
  getTimeout :: m Int

newtype Skype c m a = Skype (ReaderT (SkypeEnvironment c) m a)
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (SkypeEnvironment c))

data SkypeEnvironment c = SkypeEnvironment
  { skypeTimeout :: Int
  , skypeConnection :: c
  }

data SkypeAttachStatus = SkypeAttached
                       | SkypeNotAvailable
                       | SkypeRefused
  deriving (Eq, Show)

-- | runSkype
runSkype :: Skype c m a -> SkypeEnvironment c -> m a
runSkype (Skype skype) = runReaderT skype

-- | withSkype
withSkype :: SkypeEnvironment c -> Skype c m a -> m a
withSkype = flip runSkype

-- | duplicateChannel
dupSkypeChannel :: (MonadIO m, MonadSkype m) => m SkypeChannel
dupSkypeChannel = getSkypeChannel >>= liftIO . atomically . dupTChan
