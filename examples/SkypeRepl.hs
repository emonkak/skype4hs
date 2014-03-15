import Control.Applicative
import Control.Concurrent.Lifted
import Control.Monad
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
    _ <- fork $ onNotification $ \notification ->
      liftIO $ print $ parseNotification notification

    protocol 9999

    forever $ liftIO BS.getLine >>= sendCommand

  return ()
