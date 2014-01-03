module Web.Skype.Command.Utils (
  HandlerResult,
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

type HandlerResult a = Maybe (Either SkypeError a)

handleCommand :: (MonadIO m, MonadSkype m)
              => Command
              -> (BL.ByteString -> HandlerResult a)
              -> m a
handleCommand command handler = do
  chan <- dupSkypeChannel

  sendCommand command

  time <- getTimeout
  result <- liftIO $ timeout (time * 1000) $ loop chan

  maybe (throwError $ SkypeTimeout command)
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
                    -> (SkypeResponse -> HandlerResult a)
                    -> m a
handleCommandWithID command handler = do
  commandID <- liftIO $ (BC.pack . show . hashUnique) `fmap` newUnique

  let command = "#" <> commandID <> " " <> command

  handleCommand command $ \response ->
    case parseResponseWithCommandID response of
      Just (actual, result)
        | actual == commandID -> handler result
        | otherwise           -> Nothing
      Nothing                 -> Nothing
