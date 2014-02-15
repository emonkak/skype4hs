module Web.Skype.Command.Misc (
  name,
  protocol
) where

import Control.Monad.Error (strMsg)
import Control.Monad.Trans (MonadIO)
import Data.Monoid ((<>))
import Web.Skype.Command.Utils
import Web.Skype.Core
import Web.Skype.Protocol

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

name :: (MonadIO m, MonadSkype m) => BS.ByteString -> SkypeT m ()
name clientName = handleCommand command $ \response ->
  case response of
    "OK"                 -> Just $ Right ()
    "CONNSTATUS OFFLINE" -> Just $ Left $ SkypeError 0 command "Skype is offline"
    "ERROR 68"           -> Just $ Left $ SkypeError 0 command "Connection refused"
    otherwise            -> Nothing
  where
    command = "NAME " <> clientName

protocol :: (MonadIO m, MonadSkype m) => Int -> SkypeT m ()
protocol version = handleCommand command $ \response ->
  if BL.isPrefixOf "PROTOCOL " response
    then Just $ Right ()
    else Nothing
  where
    command = "PROTOCOL " <> BC.pack (show version)
