module Web.Skype.Monad (
  MonadSkype(..),
  SkypeChannel
) where

import Control.Concurrent.Chan (Chan, dupChan)
import Control.Monad.Trans (MonadIO, liftIO)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

type SkypeChannel = Chan BL.ByteString

-- | Provides the DSL for Skype API.
class Monad m => MonadSkype m where
  -- | Sends the command message to the Skype instance.
  sendMessage :: BS.ByteString -> m ()

  -- | Gets the message channel of Skype from the event loop.
  getChannel :: m SkypeChannel

duplicateChannel :: (MonadSkype m, MonadIO m) => m SkypeChannel
duplicateChannel = getChannel >>= liftIO . dupChan
