module Web.Skype.Command.Internal (
  attachX11,
  handleCommand,
  handleCommandWithID
) where

import Control.Concurrent.STM.TChan (readTChan)
import Control.Monad.STM (atomically)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Unique (newUnique, hashUnique)
import System.Timeout (timeout)
import Web.Skype.Core
import Web.Skype.Parser

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

attachX11 :: (MonadIO m, MonadSkype m)
          => BS.ByteString
          -> m SkypeAttachStatus
attachX11 client = handleCommand command handler >>=
                   return . fromMaybe SkypeNotAvailable
  where
    command = "NAME " <> client

    handler "OK"                 = Just SkypeAttached
    handler "CONNSTATUS OFFLINE" = Just SkypeNotAvailable
    handler "ERROR 68"           = Just SkypeRefused
    handler _                    = Nothing

protocol :: (MonadIO m, MonadSkype m)
         => Int
         -> m Bool
protocol version = handleCommand command handler >>=
                   return . fromMaybe False
  where
    command = "PROTOCOL " <> BC.pack (show version)

    handler responce
      | BL.isPrefixOf "PROTOCOL " responce = Just True
      | otherwise                          = Nothing

handleCommand :: (MonadIO m, MonadSkype m) => Command -> (BL.ByteString -> Maybe a) -> m (Maybe a)
handleCommand command handler = do
  chan <- dupSkypeChannel

  sendCommand command

  getTimeout >>= \time -> liftIO $ timeout (time * 1000) $ loop chan
  where
    loop chan = do
      responce <- atomically $ readTChan chan

      case handler responce of
        Just x  -> return x
        Nothing -> loop chan

handleCommandWithID :: (MonadIO m, MonadSkype m)
                    => Command
                    -> (Notification -> Maybe a)
                    -> m (Maybe a)
handleCommandWithID command handler = do
  commandID <- liftIO $ (BC.pack . show . hashUnique) `fmap` newUnique

  handleCommand ("#" <> commandID <> " " <> command) $
                createHandler commandID
  where
    createHandler except = \responce ->
      case parseNotificationWithCommandID responce of
        Right (actual, result)
          | actual == except -> handler result
          | otherwise        -> Nothing
        Left _               -> Nothing
