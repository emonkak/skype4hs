module Web.Skype.Command.Communication (
  attachX11,
  protocol
) where

import Control.Monad.Trans (MonadIO)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Web.Skype.Command.Utils
import Web.Skype.Core
import Web.Skype.Parser

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

attachX11 :: (MonadIO m, MonadSkype m)
          => BS.ByteString
          -> m SkypeAttachStatus
attachX11 client = handleCommand command handler
  where
    command = "NAME " <> client

    handler "OK"                 = Just SkypeAttached
    handler "CONNSTATUS OFFLINE" = Just SkypeNotAvailable
    handler "ERROR 68"           = Just SkypeRefused
    handler _                    = Nothing

protocol :: (MonadIO m, MonadSkype m)
         => Int
         -> m ()
protocol version = handleCommand command handler
  where
    command = "PROTOCOL " <> BC.pack (show version)

    handler responce
      | BL.isPrefixOf "PROTOCOL " responce = Just ()
      | otherwise                          = Nothing
