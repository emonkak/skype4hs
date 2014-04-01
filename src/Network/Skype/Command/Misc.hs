module Network.Skype.Command.Misc where

import Control.Monad.Error
import Control.Monad.Trans.Control
import Data.Monoid ((<>))
import Network.Skype.Command.Utils
import Network.Skype.Core

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

authenticate :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => BS.ByteString -> SkypeT m ()
authenticate clientName = handleCommand command $ \response ->
  case response of
    "OK"                 -> return $ Just ()
    "CONNSTATUS OFFLINE" -> throwError $ SkypeError 0 command "Skype is offline"
    "ERROR 68"           -> throwError $ SkypeError 0 command "Connection refused"
    _                    -> return Nothing
  where
    command = "NAME " <> clientName

protocol :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => Int -> SkypeT m ()
protocol version = handleCommand command $ \response ->
  if BL.isPrefixOf "PROTOCOL " response
    then return $ Just ()
    else return Nothing
  where
    command = "PROTOCOL " <> BC.pack (show version)
