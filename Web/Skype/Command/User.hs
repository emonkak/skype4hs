module Web.Skype.Command.User (
  getCurrentUserHandle
) where

import Control.Monad.Trans (MonadIO)
import Data.Monoid ((<>))
import Web.Skype.Command.Utils
import Web.Skype.Core
import Web.Skype.Protocol

import qualified Data.ByteString as BS

getCurrentUserHandle :: (MonadIO m, MonadSkype m) => SkypeT m UserID
getCurrentUserHandle = executeCommandWithID command $ \response ->
  case response of
    CurrentUserHandle userID -> Just $ Right userID
    Error code description   -> Just $ Left $ SkypeError code command description
    _                        -> Nothing
  where
    command = "GET CURRENTUSERHANDLE"

