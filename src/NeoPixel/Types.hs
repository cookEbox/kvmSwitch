module NeoPixel.Types
  ( RGB(..)
  , PixelOrder(..)
  , StripConfig(..)
  , packRGB
  ) where

import Data.Word (Word8, Word32)
import Data.Bits ((.|.), shiftL)

data RGB = RGB !Word8 !Word8 !Word8
  deriving (Eq, Show)

data PixelOrder = OrderRGB | OrderGRB
  deriving (Eq, Show)

data StripConfig = StripConfig
  { count      :: !Int
  , gpio       :: !Int
  , brightness :: !Word8
  , order      :: !PixelOrder
  }
  deriving (Eq, Show)

packRGB :: RGB -> Word32
packRGB (RGB red green blue) =
     (fromIntegral red   `shiftL` 16)
  .|. (fromIntegral green `shiftL` 8)
  .|.  fromIntegral blue
