module Web.Skype.Core (
  Command,
  CommandID,
  MonadSkype(..),
  Skype,
  SkypeChannel,
  SkypeConfig(..),
  SkypeError(..),
  defaultConfig,
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
import Control.Monad.Trans (MonadIO, MonadTrans, lift, liftIO)
import Data.String (fromString)
import Data.Typeable (Typeable)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

type Command = BS.ByteString
type CommandID = BS.ByteString

type SkypeChannel = TChan BL.ByteString

-- | Provides the DSL for Skype API.
class (Monad m) => MonadSkype m where
  -- | Sends the command message to the Skype instance.
  sendCommand :: Command -> m ()

  -- | Gets the message channel of Skype from the event loop.
  getSkypeChannel :: m SkypeChannel

newtype Skype m a = Skype (ErrorT SkypeError (ReaderT SkypeConfig m) a)
  deriving (Monad, MonadIO, MonadError SkypeError, MonadReader SkypeConfig)

instance MonadTrans Skype where
  lift = Skype . lift . lift

instance MonadSkype m => MonadSkype (Skype m) where
  sendCommand = lift . sendCommand

  getSkypeChannel = lift getSkypeChannel

data SkypeConfig = SkypeConfig
  { skypeTimeout :: Int }

defaultConfig = SkypeConfig
  { skypeTimeout = 10000 }

data SkypeError = SkypeError
  { skypeErrorCode :: Int
  , skypeErrorCommand :: Command
  , skypeErrorDescription :: T.Text
  }
  deriving (Eq, Show, Typeable)

instance Error SkypeError where
  noMsg = SkypeError 0 "" ""
  strMsg = SkypeError 0 "" . fromString

runSkype :: Skype (ReaderT c m) a
         -> c
         -> SkypeConfig
         -> m (Either SkypeError a)
runSkype (Skype skype) connection config =
  runReaderT (runReaderT (runErrorT skype) config) connection

withSkype :: c
          -> SkypeConfig
          -> Skype (ReaderT c m) a
          -> m (Either SkypeError a)
withSkype connection config skype = runSkype skype connection config

dupSkypeChannel :: (MonadIO m, MonadSkype m) => m SkypeChannel
dupSkypeChannel = getSkypeChannel >>= liftIO . atomically . dupTChan
