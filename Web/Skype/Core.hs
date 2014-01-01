module Web.Skype.Core (
  Command,
  CommandID,
  MonadSkype(..),
  Skype,
  SkypeAttachStatus(..),
  SkypeChannel,
  dupSkypeChannel,
  runSkype,
  withSkype
) where

import Web.Skype.Internal
