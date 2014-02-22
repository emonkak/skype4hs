module Web.Skype.Protocol.Misc where

data ConnectionStatus = ConnectionStatusOffline
                      | ConnectionStatusConnecting
                      | ConnectionStatusPausing
                      | ConnectionStatusOnline
  deriving (Eq, Show)
