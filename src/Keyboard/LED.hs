module Keyboard.LED 
  ( LED(..)
  , Colour(..)
  , colour32
  ) where

import           Data.Word      (Word32)
import           OneWire.LedSpi (rgb)

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

colour32 :: Colour -> Word32
colour32 Off   = rgb 0   0   0
colour32 White = rgb 255 255 255
colour32 Red   = rgb 255 0   0
colour32 Blue  = rgb 0   0   255
colour32 Green = rgb 0   255 0

