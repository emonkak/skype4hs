module Web.Skype.Monad (
  MonadSkype(..)
) where

import qualified Data.ByteString as BS

class Monad m => MonadSkype m where
  send :: BS.ByteString -> m ()
