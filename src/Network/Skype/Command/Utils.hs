module Network.Skype.Command.Utils where

import Control.Concurrent.STM.TChan (readTChan)
import Control.Monad.Error (throwError)
import Control.Monad.Reader (asks)
import Control.Monad.STM (atomically)
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Attoparsec.ByteString.Lazy
import Data.Monoid ((<>))
import Data.Unique (newUnique, hashUnique)
import System.Timeout.Lifted (timeout)
import Network.Skype.Core
import Network.Skype.Parser (parseNotification, parseCommandID)
import Network.Skype.Protocol

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T

executeCommand :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
               => Command
               -> (NotificationObject -> SkypeT m (Maybe a))
               -> SkypeT m a
executeCommand command handler = handleCommand command $ \notification ->
  case parseNotification notification of
    Right response -> handler response
    Left _         -> return Nothing

executeCommandWithID :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
                     => Command
                     -> (NotificationObject -> SkypeT m (Maybe a))
                     -> SkypeT m a
executeCommandWithID command handler = handleCommandWithID command $ \expectID notification ->
  case parseCommandID notification of
    Done t commandID
      | commandID == expectID -> do
        case parseNotification t of
          Left e       -> throwError $ SkypeError 0 command (T.pack e)
          Right object -> guardError object >> handler object
      | otherwise -> return Nothing
    _ -> return Nothing
  where
    guardError (Error code description) = throwError $ SkypeError code command description
    guardError _                        = return ()

handleCommand :: (MonadBaseControl IO m, MonadIO m, MonadSkype m)
              => Command
              -> (Notification -> SkypeT m (Maybe a))
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
                    -> (CommandID -> Notification -> SkypeT m (Maybe a))
                    -> SkypeT m a
handleCommandWithID command handler = do
  commandID <- liftIO $ (BC.pack . show . hashUnique) `fmap` newUnique

  let command' = "#" <> commandID <> " " <> command

  handleCommand command' $ handler commandID
