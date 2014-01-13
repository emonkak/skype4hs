module Web.Skype.Command.Utils (
  HandlerResult,
  executeCommand,
  executeCommandWithID,
  handleCommand,
  handleCommandWithID
) where

import Control.Concurrent.STM.TChan (readTChan)
import Control.Monad.Error (strMsg, throwError)
import Control.Monad.Reader (asks)
import Control.Monad.STM (atomically)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Data.Monoid ((<>))
import Data.Unique (newUnique, hashUnique)
import System.Timeout (timeout)
import Web.Skype.Core
import Web.Skype.Parser

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

type HandlerResult a = Maybe (Either SkypeError a)

executeCommand :: (MonadIO m, MonadSkype m)
               => Command
               -> (SkypeResponse -> HandlerResult a)
               -> SkypeT m a
executeCommand command handler = handleCommand command $ \response ->
  case parseResponse response of
    Just result -> handler result
    Nothing     -> Nothing

executeCommandWithID :: (MonadIO m, MonadSkype m)
                     => Command
                     -> (SkypeResponse -> HandlerResult a)
                     -> SkypeT m a
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
       -> SkypeT m a
handleCommand command handler = do
  chan <- dupSkypeChannel

  sendCommand command

  time <- asks skypeTimeout
  result <- liftIO $ timeout time $ loop chan

  maybe (throwError $ strMsg "Command timeout")
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
                    -> SkypeT m a
handleCommandWithID command handler = do
  commandID <- liftIO $ (BC.pack . show . hashUnique) `fmap` newUnique

  let command' = "#" <> commandID <> " " <> command

  handleCommand command' $ handler commandID
