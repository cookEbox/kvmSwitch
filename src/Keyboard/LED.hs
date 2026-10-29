module Keyboard.LED 
  ( LED(..)
  , Colour(..)
  , ledIndex
  , toRGB
  , colour32
  ) where

import           Data.Word      (Word32)
import           NeoPixel       (RGB(..), packRGB)

data LED = LED1  | LED2  | LED3  | LED4
         | LED5  | LED6  | LED7  | LED8
         | LED9  | LED10 | LED11 | LED12
         | LED13 | LED14 | LED15 | LED16
         | LED17 | LED18 | LED19 | LED20
         | LED21 | LED22 | LED23 | LED24
         | LED25 | LED26 | LED27 | LED28
         | LED29 | LED30 | LED31 | LED32
         | LED33 | LED34 | LED35 | LED36
         deriving (Show, Eq, Ord)

data Colour = White | Red | Blue | Green | Off deriving (Show, Eq)

ledIndex :: LED -> Int
ledIndex LED1  = 0
ledIndex LED2  = 1
ledIndex LED3  = 2
ledIndex LED4  = 3
ledIndex LED5  = 4
ledIndex LED6  = 5
ledIndex LED7  = 6
ledIndex LED8  = 7
ledIndex LED9  = 8
ledIndex LED10 = 9
ledIndex LED11 = 10
ledIndex LED12 = 11
ledIndex LED13 = 12
ledIndex LED14 = 13
ledIndex LED15 = 14
ledIndex LED16 = 15
ledIndex LED17 = 16
ledIndex LED18 = 17
ledIndex LED19 = 18
ledIndex LED20 = 19
ledIndex LED21 = 20
ledIndex LED22 = 21
ledIndex LED23 = 22
ledIndex LED24 = 23
ledIndex LED25 = 24
ledIndex LED26 = 25
ledIndex LED27 = 26
ledIndex LED28 = 27
ledIndex LED29 = 28
ledIndex LED30 = 29
ledIndex LED31 = 30
ledIndex LED32 = 31
ledIndex LED33 = 32
ledIndex LED34 = 33
ledIndex LED35 = 34
ledIndex LED36 = 35

toRGB :: Colour -> RGB
toRGB Off   = RGB 0   0   0
toRGB White = RGB 255 255 255
toRGB Red   = RGB 255 0   0
toRGB Green = RGB 0   255 0
toRGB Blue  = RGB 0   0   255

colour32 :: Colour -> Word32
colour32 = packRGB . toRGB
