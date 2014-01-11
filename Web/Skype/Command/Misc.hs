module Web.Skype.Command.Misc (
  attachX11,
  protocol
) where

import Control.Monad.Error (strMsg)
import Control.Monad.Trans (MonadIO)
import Data.Monoid ((<>))
import Web.Skype.Command.Utils
import Web.Skype.Core
import Web.Skype.Parser
import Web.Skype.Protocol.Misc

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

attachX11 :: (MonadIO m, MonadSkype m) => BS.ByteString -> Skype m ()
attachX11 client = executeCommand command $ \response ->
  case response of
    OK                                       -> Just $ Right ()
    ConnectionStatus ConnectionStatusOffline -> Just $ Left $ strMsg "Skype is offline"
    Error code description                   -> Just $ Left $ SkypeError code command description
    _                                        -> Nothing
  where
    command = "NAME " <> client

protocol :: (MonadIO m, MonadSkype m) => Int -> Skype m ()
protocol version = executeCommand command $ \response ->
  case response of
    Protocol _ -> Just $ Right ()
    _          -> Nothing
  where
    command = "PROTOCOL " <> BC.pack (show version)
