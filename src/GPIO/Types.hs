{-# LANGUAGE DeriveGeneric       #-}

module GPIO.Types where

import           Data.Yaml    (FromJSON, ToJSON)
import           Foreign      (Ptr, shiftL)
import           Foreign.C    (CInt (..))
import           GHC.Generics (Generic)

data CGpioChip
data CGpioLine

type Chip = Ptr CGpioChip

data Pin = P1  | P2  | P3  | P4  | P5
         | P6  | P7  | P8  | P9  | P10
         | P11 | P12 | P13 | P14 | P15
         | P16 | P17 | P18 | P19 | P20
         | P21 | P22 | P23 | P24 | P25
         | P26
         deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON Pin
instance ToJSON Pin

data PinPtr = PinPtr
            { pin :: Pin
            , ptr :: Ptr CGpioLine
            } deriving (Eq, Ord, Show)

data Key = Key Pin Pin deriving (Eq, Ord, Show)

openDrain, openSource, activeLow :: CInt
biasPullUp, biasPullDown, biasDisable :: CInt
openDrain    = 1 `shiftL` 0
openSource   = 1 `shiftL` 1
activeLow    = 1 `shiftL` 2
biasPullUp   = 1 `shiftL` 3
biasPullDown = 1 `shiftL` 4
biasDisable  = 1 `shiftL` 5
