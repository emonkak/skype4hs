module Web.Skype.Core (
  Command,
  CommandID,
  MonadSkype(..),
  Notification,
  SkypeConfig(..),
  SkypeError(..),
  SkypeT,
  defaultConfig,
  dupNotificationChan,
  runSkype,
  runSkypeWith
) where

import Control.Applicative (Applicative)
import Control.Concurrent.STM.TChan (TChan, dupTChan)
import Control.Monad (liftM)
import Control.Monad.Error (MonadError, Error(..), ErrorT(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..), runReaderT)
import Control.Monad.STM (atomically)
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans (MonadIO, MonadTrans, lift, liftIO)
import Control.Monad.Trans.Control
import Data.String (fromString)
import Data.Typeable (Typeable)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

type Command = BS.ByteString
type CommandID = BS.ByteString

type Notification = BL.ByteString

-- | Provides the DSL for Skype API.
class (Monad m) => MonadSkype m where
  -- | Sends the command message to the Skype instance.
  sendCommand :: Command -> m ()

  -- | Gets the notification channel of Skype from the event loop.
  getNotificationChan :: m (TChan Notification)

newtype SkypeT m a = SkypeT
  { runSkypeT :: ErrorT SkypeError (ReaderT SkypeConfig m) a
  }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadIO
           , MonadError SkypeError
           , MonadReader SkypeConfig
           , MonadBase base
           )

instance MonadTrans SkypeT where
  lift = SkypeT . lift . lift

instance MonadTransControl SkypeT where
  newtype StT SkypeT a = StSkype { unStSkype :: Either SkypeError a }

  liftWith f = SkypeT . ErrorT . ReaderT $ \r ->
    liftM Right $ f $ \t ->
      liftM StSkype $ runReaderT (runErrorT (runSkypeT t)) r

  restoreT = SkypeT . ErrorT . ReaderT . const . liftM unStSkype

instance MonadBaseControl base m => MonadBaseControl base (SkypeT m) where
  newtype StM (SkypeT m) a = StMSkypeT { unStMSkypeT :: ComposeSt SkypeT m a }

  liftBaseWith = defaultLiftBaseWith StMSkypeT

  restoreM = defaultRestoreM unStMSkypeT

instance MonadSkype m => MonadSkype (SkypeT m) where
  sendCommand = lift . sendCommand

  getNotificationChan = lift getNotificationChan

data SkypeConfig = SkypeConfig
  { skypeTimeout :: Int }

defaultConfig :: SkypeConfig
defaultConfig = SkypeConfig
  { skypeTimeout = 10000 * 1000 }

data SkypeError = SkypeError
  { skypeErrorCode :: Int
  , skypeErrorCommand :: Command
  , skypeErrorDescription :: T.Text
  }
  deriving (Eq, Show, Typeable)

instance Error SkypeError where
  noMsg = SkypeError 0 "" ""
  strMsg = SkypeError 0 "" . fromString

runSkype :: (Monad m, MonadSkype (ReaderT connection m))
         => connection
         -> SkypeT (ReaderT connection m) a
         -> m (Either SkypeError a)
runSkype connection = runSkypeWith connection defaultConfig

runSkypeWith :: (Monad m, MonadSkype (ReaderT connection m))
             => connection
             -> SkypeConfig
             -> SkypeT (ReaderT connection m) a
             -> m (Either SkypeError a)
runSkypeWith connection config skype =
  runReaderT (runReaderT (runErrorT (runSkypeT skype)) config) connection

dupNotificationChan :: (MonadIO m, MonadSkype m) => m (TChan Notification)
dupNotificationChan = getNotificationChan >>= liftIO . atomically . dupTChan
