module Web.Skype.Command.Utils (
  handleCommand,
  handleCommandWithID
) where

import Control.Concurrent.STM.TChan (readTChan)
import Control.Monad.Error (throwError)
import Control.Monad.STM (atomically)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Monoid ((<>))
import Data.Unique (newUnique, hashUnique)
import System.Timeout (timeout)
import Web.Skype.Core
import Web.Skype.Parser

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

handleCommand :: (MonadIO m, MonadSkype m)
              => Command
              -> (BL.ByteString -> Maybe a)
              -> m a
handleCommand command handler = do
  chan <- dupSkypeChannel

  sendCommand command

  time <- getTimeout
  result <- liftIO $ timeout (time * 1000) $ loop chan

  maybe (throwError $ SkypeTimeout command) return result
  where
    loop chan = do
      responce <- atomically $ readTChan chan

      case handler responce of
        Just x  -> return x
        Nothing -> loop chan

handleCommandWithID :: (MonadIO m, MonadSkype m)
                    => Command
                    -> (Notification -> Maybe a)
                    -> m a
handleCommandWithID command handler = do
  commandID <- liftIO $ (BC.pack . show . hashUnique) `fmap` newUnique

  handleCommand ("#" <> commandID <> " " <> command) $
                createHandler commandID
  where
    createHandler except = \response ->
      case parseNotificationWithCommandID response of
        Right (actual, result)
          | actual == except -> handler result
          | otherwise        -> Nothing
        Left _               -> Nothing
