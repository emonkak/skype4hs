{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Maybe
import Web.Skype.API.X11

import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  (Just skype) <- runMaybeT skypeConnect
  sendMessage skype "NAME haskell-skype"
  sendMessage skype "PROTOCOL 5"
  runEventLoop skype
