module Web.Skype.Command.Communication (
  attachX11,
  protocol
) where

import Control.Monad.Error (strMsg)
import Control.Monad.Trans (MonadIO)
import Data.Monoid ((<>))
import Web.Skype.Command.Utils
import Web.Skype.Core
import Web.Skype.Parser

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

attachX11 :: (MonadIO m, MonadSkype m) => BS.ByteString -> m ()
attachX11 client = handleCommand command $ \response ->
  case response of
    "OK"                 -> Just $ Right ()
    "CONNSTATUS OFFLINE" -> Just $ Left $ strMsg "Skype is offline"
    "ERROR 68"           -> Just $ Left $ SkypeError 68 "Access denied"
    _                    -> Nothing
  where
    command = "NAME " <> client

protocol :: (MonadIO m, MonadSkype m) => Int -> m ()
protocol version = handleCommand command $ \responce ->
  if BL.isPrefixOf "PROTOCOL " responce
    then Just $ Right ()
    else Nothing
  where
    command = "PROTOCOL " <> BC.pack (show version)
