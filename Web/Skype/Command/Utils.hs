module Web.Skype.Command.Utils where

import Control.Concurrent.STM.TChan (readTChan)
import Control.Monad.Error (throwError)
import Control.Monad.Reader (asks)
import Control.Monad.STM (atomically)
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Data.Unique (newUnique, hashUnique)
import System.Timeout.Lifted (timeout)
import Web.Skype.Core
import Web.Skype.Parser (parseNotification, parseNotificationWithCommandID)
import Web.Skype.Protocol

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

executeCommand :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
               => Command
               -> (SkypeNotification -> SkypeT m (Maybe a))
               -> SkypeT m a
executeCommand command handler = handleCommand command $ \notification ->
  case parseNotification notification of
    Just response -> handler response
    Nothing       -> return Nothing

executeCommandWithID :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
                     => Command
                     -> (SkypeNotification -> SkypeT m (Maybe a))
                     -> SkypeT m a
executeCommandWithID command handler = handleCommandWithID command $ \expectID notification ->
  case parseNotificationWithCommandID notification of
    Just (actualID, response)
      | actualID == expectID -> do
        result <- handler response
        if (isNothing result)
          then case response of
            Error code description -> throwError $ SkypeError code command description
            _                      -> return result
          else return result
      | otherwise -> return Nothing
    Nothing       -> return Nothing

handleCommand :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
              => Command
              -> (BL.ByteString -> SkypeT m (Maybe a))
              -> SkypeT m a
handleCommand command handler = do
  chan <- dupNotificationChan

  sendCommand command

  time <- asks skypeTimeout
  result <- timeout time $ loop chan

  maybe (throwError $ SkypeError 0 command "Command timeout") return result
  where
    loop chan = do
      response <- liftIO $ atomically $ readTChan chan
      result <- handler response
      case result of
        Just value -> return value
        Nothing    -> loop chan

handleCommandWithID :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
                    => Command
                    -> (CommandID -> BL.ByteString -> SkypeT m (Maybe a))
                    -> SkypeT m a
handleCommandWithID command handler = do
  commandID <- liftIO $ (BC.pack . show . hashUnique) `fmap` newUnique

  let command' = "#" <> commandID <> " " <> command

  handleCommand command' $ handler commandID
