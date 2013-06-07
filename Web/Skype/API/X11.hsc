{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction, FlexibleContexts,
             FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Web.Skype.API.X11 where

#include <X11/Xlib.h>

import Prelude hiding (catch)

import Control.Applicative
import Control.Concurrent
import Control.Exception (SomeException, IOException, throw, handle)
import Control.Exception.Lifted (catch)
import Control.Monad
import Control.Monad.Error
import Control.Monad.Failure hiding (Error)
import Control.Monad.Failure.Transformers
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe
import Data.Bits
import Data.Maybe
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.X11.Types
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Atom
import Graphics.X11.Xlib.Display
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Window
import System.Environment (getEnv)
import System.Posix hiding (getEnv)

import qualified Data.ByteString as BS

import Web.Skype

data Skype = Skype
  { skypeDisplay :: Display
  , skypeWindow :: XID
  , skypeInstance :: XID
  , skypeMessageBeginAtom :: Atom
  , skypeMessageAtom :: Atom
  }
  deriving Show

instance Alternative IO where
  empty = throw $ userError "empty"
  x <|> y = handle (\(_ :: SomeException) -> y) x

instance (Monad m, Failure e m) => Failure e (MaybeT m) where
  failure _ = MaybeT $ return Nothing

-- | Get the X11 display address running the Skype.
getDisplay :: (MonadIO m, MonadBaseControl IO m, Failure IOException m) => m String
getDisplay = liftIO (getEnv "SKYPEDISPLAY" <|> getEnv "DISPLAY") `catch`
             \(e :: IOException) -> failure e

skypeConnect :: (MonadIO m, Failure SkypeError m) => m Skype
skypeConnect = do
  display@(Display p_display) <- liftIO $ runMaybeT getDisplay >>=
                                 maybe (return $ Display nullPtr) openDisplay
  when (p_display == nullPtr) $ skypeError "Couldn't open display"

  let root   = defaultRootWindow display
      screen = defaultScreen display
      pixel  = blackPixel display screen

  window <- liftIO $ createSimpleWindow display root 0 0 1 1 0 pixel pixel
  when (window == none) $ liftIO $ closeDisplay display >>
                          skypeError "Could not create X11 messaging window"

  instanceAtom <- liftIO $ internAtom display "_SKYPE_INSTANCE" True
  when (instanceAtom == none) $ destroy display window >>
                                skypeError "Could not create skype atom"

  messageBeginAtom <- liftIO $ internAtom display "SKYPECONTROLAPI_MESSAGE_BEGIN" True
  messageAtom <- liftIO $ internAtom display "SKYPECONTROLAPI_MESSAGE" True

  status <- liftIO $ getWindowProperty32 display instanceAtom root
  case (status >>= listToMaybe) of
    Nothing -> do
      destroy display window
      skypeError "Skype instance not found"
    Just property -> return $ Skype
      { skypeDisplay          = display
      , skypeWindow           = window
      , skypeInstance         = fromIntegral property .&. 0xffffffff
      , skypeMessageBeginAtom = messageBeginAtom
      , skypeMessageAtom      = messageAtom
      }
  where
    skypeError = failure . SkypeError

    destroy display window = liftIO $ do
      destroyWindow display window
      closeDisplay display

sendMessage :: Skype -> BS.ByteString -> IO ()
sendMessage skype message = do
  liftIO $ allocaXEvent $ \p_event -> do
    let (Display display) = skypeDisplay skype
    #{poke XClientMessageEvent, type}         p_event (#{const ClientMessage} :: CInt)
    #{poke XClientMessageEvent, display}      p_event display
    #{poke XClientMessageEvent, window}       p_event $ skypeWindow skype
    #{poke XClientMessageEvent, message_type} p_event $ skypeMessageBeginAtom skype
    #{poke XClientMessageEvent, format}       p_event (8 :: CInt)
    loop p_event message
  where
    loop p_event message
      | BS.null message = return ()
      | otherwise       = do
        let (bytes, rest) = BS.splitAt 20 message
        let p_data = #{ptr XClientMessageEvent, data} p_event
        pokeArray p_data $ BS.unpack bytes
        sendEvent (skypeDisplay skype) (skypeInstance skype) False 0 p_event
        loop p_event rest

runEventLoop :: Skype -> IO ()
runEventLoop skype = do
  liftIO $ allocaXEvent loop
  where
    loop p_event = do
      isClientMessage <- checkTypedEvent (skypeDisplay skype) clientMessage p_event
      if isClientMessage
        then nextEvent (skypeDisplay skype) p_event >> print p_event
        else yield >> usleep 1000
      loop p_event
