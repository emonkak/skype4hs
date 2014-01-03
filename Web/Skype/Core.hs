module Web.Skype.Core (
  Command,
  CommandID,
  MonadSkype(..),
  Skype(..),
  SkypeChannel,
  SkypeEnvironment(..),
  SkypeError(..),
  dupSkypeChannel,
  runSkype,
  withSkype
) where

import Control.Applicative (Applicative)
import Control.Concurrent.STM.TChan (TChan, dupTChan)
import Control.Exception (Exception)
import Control.Monad.Error (MonadError, Error(..), ErrorT(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..), runReaderT)
import Control.Monad.STM (atomically)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.String (fromString)
import Data.Typeable (Typeable)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

type Command = BS.ByteString
type CommandID = BS.ByteString

type SkypeChannel = TChan BL.ByteString

-- | Provides the DSL for Skype API.
class (Monad m, MonadError SkypeError m) => MonadSkype m where
  -- | Attaches to the skype instance.
  attach :: m ()

  -- | Sends the command message to the Skype instance.
  sendCommand :: Command -> m ()

  -- | Gets the message channel of Skype from the event loop.
  getSkypeChannel :: m SkypeChannel

  -- | Gets the timeout period of an command (milliseconds).
  getTimeout :: m Int

newtype Skype c m a = Skype (ErrorT SkypeError (ReaderT (SkypeEnvironment c) m) a)
  deriving (Monad, MonadIO, MonadError SkypeError, MonadReader (SkypeEnvironment c))

data SkypeEnvironment c = SkypeEnvironment
  { skypeTimeout :: Int
  , skypeConnection :: c
  }

data SkypeError = SkypeError Int T.Text
                | SkypeTimeout Command
  deriving (Eq, Show, Typeable)

instance Error SkypeError where
  noMsg = SkypeError 0 ""
  strMsg = SkypeError 0 . fromString

-- | runSkype
runSkype :: Skype c m a -> SkypeEnvironment c -> m (Either SkypeError a)
runSkype (Skype skype) = runReaderT (runErrorT skype)

-- | withSkype
withSkype :: SkypeEnvironment c -> Skype c m a -> m (Either SkypeError a)
withSkype = flip runSkype

-- | duplicateChannel
dupSkypeChannel :: (MonadIO m, MonadSkype m) => m SkypeChannel
dupSkypeChannel = getSkypeChannel >>= liftIO . atomically . dupTChan
