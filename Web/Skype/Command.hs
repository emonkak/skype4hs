{-# LANGUAGE OverloadedStrings #-}

module Web.Skype.Command (
  authorize,
  name,
  protocol
) where

import Web.Skype.Monad

import qualified Data.ByteString.Char8 as BS

protocol :: MonadSkype m => Int -> m ()
protocol = send . BS.append "PROTOCOL " . BS.pack . show

name :: MonadSkype m => BS.ByteString -> m ()
name = send . BS.append "NAME "

authorize :: MonadSkype m => BS.ByteString -> m ()
authorize client = do
  name client
  protocol 8
