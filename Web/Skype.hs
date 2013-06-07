{-# LANGUAGE DeriveDataTypeable #-}

module Web.Skype where

import Control.Exception.Base
import Control.Monad.Error
import Data.Typeable

newtype SkypeError = SkypeError String
  deriving (Eq, Show, Typeable)

instance Error SkypeError where
  noMsg = SkypeError "Unknown error"
  strMsg = SkypeError

instance Exception SkypeError
