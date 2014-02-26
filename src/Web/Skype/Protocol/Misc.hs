module Web.Skype.Protocol.Misc where

import Data.Typeable (Typeable)

data ConnectionStatus = ConnectionStatusOffline
                      | ConnectionStatusConnecting
                      | ConnectionStatusPausing
                      | ConnectionStatusOnline
  deriving (Eq, Show, Typeable)
