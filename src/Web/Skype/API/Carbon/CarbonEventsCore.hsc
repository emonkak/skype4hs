module Web.Skype.API.Carbon.CarbonEventsCore where

#include "Carbon/Carbon.h"

import Foreign.C.Types

type EventTimeout = CDouble

eventDurationForever :: EventTimeout
eventDurationForever = #{const kEventDurationForever}

foreign import ccall "RunCurrentEventLoop"
  c_RunCurrentEventLoop :: EventTimeout -> IO ()
