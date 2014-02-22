module Web.Skype.Command.User where

import Control.Monad.Trans
import Control.Monad.Trans.Control
import Web.Skype.Command.Utils
import Web.Skype.Core
import Web.Skype.Protocol

getCurrentUserHandle :: (MonadBaseControl IO m, MonadIO m, MonadSkype m) => SkypeT m UserID
getCurrentUserHandle = executeCommandWithID command $ \response ->
  case response of
    CurrentUserHandle userID -> return $ Just userID
    _                        -> return Nothing
  where
    command = "GET CURRENTUSERHANDLE"
