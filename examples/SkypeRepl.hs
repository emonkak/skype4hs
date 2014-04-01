{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.Lifted
import Control.Monad
import Control.Monad.Trans
import Network.Skype.API
import Network.Skype.Command.Misc
import Network.Skype.Core
import Network.Skype.Parser (parseNotification)

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
