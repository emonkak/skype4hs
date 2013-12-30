module Web.Skype.Command (
  attachX11,
  protocol,
  handleCommand
) where

import Control.Concurrent.Chan (Chan, dupChan, readChan)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Maybe (fromMaybe)
import System.Timeout (timeout)
import Web.Skype.Core
import Debug.Trace

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

attachX11 :: (MonadIO m, MonadReader (SkypeConfig a) m, MonadSkype m)
          => BS.ByteString
          -> m SkypeAttachStatus
attachX11 client = handleCommand command handler >>=
                   return . fromMaybe SkypeNotAvailable
  where
    command = BS.append "NAME " client

    handler "OK"                 = Just SkypeAttached
    handler "CONNSTATUS OFFLINE" = Just SkypeNotAvailable
    handler "ERROR 68"           = Just SkypeRefused
    handler _                    = Nothing

protocol :: (MonadIO m, MonadReader (SkypeConfig a) m, MonadSkype m)
         => Int
         -> m Bool
protocol version = handleCommand command handler >>=
                   return . fromMaybe False
  where
    command = BS.append "PROTOCOL " $ BS.pack $ show version

    handler responce
      | BL.isPrefixOf "PROTOCOL " responce = Just True
      | otherwise                          = Nothing

handleCommand :: (MonadIO m, MonadReader (SkypeConfig c) m, MonadSkype m)
              => BS.ByteString
              -> (BL.ByteString -> Maybe a)
              -> m (Maybe a)
handleCommand command handler = do
  chan <- getChannel >>= liftIO . dupChan
  sendCommand command
  time <- asks $ (* 1000) . skypeTimeout
  liftIO $ timeout time $ loop chan
  where
    loop chan = do
      responce <- readChan chan
      case handler responce of
        Just x  -> return x
        Nothing -> loop chan
