{-# LANGUAGE ForeignFunctionInterface #-}

module GPIO.FFI where

import           Foreign    (Ptr)
import           Foreign.C  (CInt (..), CString, CUInt (..))
import           GPIO.Types (CGpioChip, CGpioLine)

foreign import ccall unsafe "gpiod_chip_open_by_name"
  c_gpiod_chip_open_by_name :: CString -> IO (Ptr CGpioChip)

foreign import ccall unsafe "gpiod_chip_close"
  c_gpiod_chip_close :: Ptr CGpioChip -> IO ()

foreign import ccall unsafe "gpiod_chip_get_line"
  c_gpiod_chip_get_line :: Ptr CGpioChip -> CUInt -> IO (Ptr CGpioLine)

foreign import ccall unsafe "gpiod_line_request_output"
  c_gpiod_line_request_output :: Ptr CGpioLine -> CString -> CInt -> IO CInt

foreign import ccall unsafe "gpiod_line_request_input_flags"
  c_gpiod_line_request_input_flags :: Ptr CGpioLine -> CString -> CInt -> IO CInt

foreign import ccall unsafe "gpiod_line_set_value"
  c_gpiod_line_set_value :: Ptr CGpioLine -> CInt -> IO CInt

foreign import ccall unsafe "gpiod_line_get_value"
  c_gpiod_line_get_value :: Ptr CGpioLine -> IO CInt

