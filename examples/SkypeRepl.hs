import Control.Applicative
import Control.Concurrent.Lifted
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.STM
import Control.Monad.Trans
import Web.Skype.API
import Web.Skype.Command.Misc
import Web.Skype.Core
import Web.Skype.Parser (parseNotification)

import qualified Data.ByteString as BS

main :: IO ()
main = do
  connection <- connect "skype-repl"

  _ <- runSkype connection $ do
    notificationChan <- dupNotificationChan

    _ <- fork $ forever $ do
      notification <- liftIO $ atomically $ readTChan notificationChan

      liftIO $ print $ parseNotification notification

    protocol 9999

    forever $ liftIO BS.getLine >>= sendCommand

  return ()
