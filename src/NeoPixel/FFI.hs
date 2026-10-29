{-# LANGUAGE ForeignFunctionInterface #-}

module NeoPixel.FFI
  ( CStrip
  , cLedOpen
  , cLedRender
  , cLedSetPixel
  , cLedShow
  , cLedClear
  , cLedSetBrightness
  , cLedClose
  , cLedErrorString
  , cInvalidHandle
  , cInvalidArgument
  , cFrameLength
  ) where

import Data.Word (Word32)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)

data CStrip

cInvalidHandle, cInvalidArgument, cFrameLength :: CInt
cInvalidHandle = -100
cInvalidArgument = -101
cFrameLength = -102

foreign import ccall safe "kvm_led_open"
  cLedOpen :: CInt -> CInt -> CInt -> CInt -> Ptr (Ptr CStrip) -> IO CInt

foreign import ccall safe "kvm_led_render"
  cLedRender :: Ptr CStrip -> Ptr Word32 -> CSize -> IO CInt

foreign import ccall safe "kvm_led_set_pixel"
  cLedSetPixel :: Ptr CStrip -> CSize -> Word32 -> IO CInt

foreign import ccall safe "kvm_led_show"
  cLedShow :: Ptr CStrip -> IO CInt

foreign import ccall safe "kvm_led_clear"
  cLedClear :: Ptr CStrip -> IO CInt

foreign import ccall safe "kvm_led_set_brightness"
  cLedSetBrightness :: Ptr CStrip -> CInt -> IO CInt

foreign import ccall safe "kvm_led_close"
  cLedClose :: Ptr CStrip -> IO ()

foreign import ccall safe "kvm_led_error_string"
  cLedErrorString :: CInt -> IO CString
