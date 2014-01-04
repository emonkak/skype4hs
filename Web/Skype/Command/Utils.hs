module Web.Skype.Command.Utils (
  HandlerResult,
  executeCommand,
  executeCommandWithID,
  handleCommand,
  handleCommandWithID
) where

import Control.Concurrent.STM.TChan (readTChan)
import Control.Monad.Error (strMsg, throwError)
import Control.Monad.STM (atomically)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Monoid ((<>))
import Data.Unique (newUnique, hashUnique)
import System.Timeout (timeout)
import Web.Skype.Core
import Web.Skype.Parser
import Web.Skype.Protocol

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

type HandlerResult a = Maybe (Either SkypeError a)

executeCommand :: (MonadIO m, MonadSkype m)
               => Command
               -> (SkypeResponse -> HandlerResult a)
               -> m a
executeCommand command handler = handleCommand command $ \response ->
  case parseResponse response of
    Just result -> handler result
    Nothing     -> Nothing

executeCommandWithID :: (MonadIO m, MonadSkype m)
                     => Command
                     -> (SkypeResponse -> HandlerResult a)
                     -> m a
executeCommandWithID command handler =
  handleCommandWithID command $ \expectID response ->
    case parseResponseWithCommandID response of
      Just (actualID, result)
        | actualID == expectID -> handler result
        | otherwise            -> Nothing
      Nothing                  -> Nothing

handleCommand :: (MonadIO m, MonadSkype m)
       => Command
       -> (BL.ByteString -> HandlerResult a)
       -> m a
handleCommand command handler = do
  chan <- dupSkypeChannel

  sendCommand command

  time <- getTimeout
  result <- liftIO $ timeout (time * 1000) $ loop chan

  maybe (throwError $ strMsg "command timeout")
        (either throwError return)
        result
  where
    loop chan = do
      response <- atomically $ readTChan chan

      case handler response of
        Just value -> return value
        Nothing    -> loop chan

handleCommandWithID :: (MonadIO m, MonadSkype m)
                    => Command
                    -> (CommandID -> BL.ByteString -> HandlerResult a)
                    -> m a
handleCommandWithID command handler = do
  commandID <- liftIO $ (BC.pack . show . hashUnique) `fmap` newUnique

  let command' = "#" <> commandID <> " " <> command

  handleCommand command' $ handler commandID
