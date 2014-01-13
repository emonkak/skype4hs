module Web.Skype.Command.Misc (
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

protocol :: (MonadIO m, MonadSkype m) => Int -> SkypeT m ()
protocol version = executeCommand command $ \response ->
  case response of
    Protocol _ -> Just $ Right ()
    _          -> Nothing
  where
    command = "PROTOCOL " <> BC.pack (show version)
