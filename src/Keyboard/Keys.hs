module Keyboard.Keys
  ( Peripherals(..)
  , Row(..)
  , Column(..)
  , Machine(..)
  , Machines(..)
  , KeyPos(..)
  , KBD(..)
  , keyboard
  , key2KeyPos
  , periOwnedBy
  ) where

import           Config       (Config(..))
import           Data.Map     (Map, fromList)
import           GPIO.Types   (Pin (..), Key(..))
import           Keyboard.LED (Colour (..), LED (..))

data Row     = R1 | R2 | R3 | R4 | R5                deriving (Show, Eq)
data Column  = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 deriving (Show, Eq)
data Machine = Mach1 | Mach2 | Mach3 | Mach4         deriving (Show, Eq, Ord)

data KeyPos = KeyPos Row Column     deriving (Show, Eq)
data KBD    = KBD KeyPos LED Colour deriving (Show, Eq)

data Machines = Machines 
  { all      :: Machine
  , monitor1 :: Machine
  , monitor2 :: Machine
  , monitor3 :: Machine
  , monitor4 :: Machine
  , mic      :: Machine
  , camera   :: Machine
  , kbd      :: Machine
  , mouse    :: Machine
  }

periOwnedBy :: Machines 
periOwnedBy = Machines
  { all      = Mach1 
  , monitor1 = Mach1 
  , monitor2 = Mach1 
  , monitor3 = Mach1 
  , monitor4 = Mach1 
  , mic      = Mach1 
  , camera   = Mach1 
  , kbd      = Mach1 
  , mouse    = Mach1 
  }

data Peripherals = Peripherals
  { all      :: KBD
  , monitor1 :: KBD
  , monitor2 :: KBD
  , monitor3 :: KBD
  , monitor4 :: KBD
  , mic      :: KBD
  , camera   :: KBD
  , kbd      :: KBD
  , mouse    :: KBD
  }

mach1Peri :: Peripherals
mach1Peri = Peripherals
 { all      = KBD (KeyPos R1 C1) LED1  White
 , monitor1 = KBD (KeyPos R2 C1) LED12 White
 , monitor2 = KBD (KeyPos R2 C2) LED11 White
 , monitor3 = KBD (KeyPos R3 C1) LED13 White
 , monitor4 = KBD (KeyPos R3 C2) LED14 White
 , mic      = KBD (KeyPos R4 C1) LED28 White
 , camera   = KBD (KeyPos R4 C2) LED27 White
 , kbd      = KBD (KeyPos R5 C1) LED29 White
 , mouse    = KBD (KeyPos R5 C2) LED30 White
 }

mach2Peri :: Peripherals
mach2Peri = Peripherals
 { all      = KBD (KeyPos R1 C3) LED2  Off
 , monitor1 = KBD (KeyPos R2 C3) LED10 Off
 , monitor2 = KBD (KeyPos R2 C4) LED9  Off
 , monitor3 = KBD (KeyPos R3 C3) LED15 Off
 , monitor4 = KBD (KeyPos R3 C4) LED16 Off
 , mic      = KBD (KeyPos R4 C3) LED26 Off
 , camera   = KBD (KeyPos R4 C4) LED25 Off
 , kbd      = KBD (KeyPos R5 C3) LED31 Off
 , mouse    = KBD (KeyPos R5 C4) LED32 Off
 }

mach3Peri :: Peripherals
mach3Peri = Peripherals
 { all      = KBD (KeyPos R1 C5) LED3  Off
 , monitor1 = KBD (KeyPos R2 C5) LED8  Off
 , monitor2 = KBD (KeyPos R2 C6) LED7  Off
 , monitor3 = KBD (KeyPos R3 C5) LED17 Off
 , monitor4 = KBD (KeyPos R3 C6) LED18 Off
 , mic      = KBD (KeyPos R4 C5) LED24 Off
 , camera   = KBD (KeyPos R4 C6) LED23 Off
 , kbd      = KBD (KeyPos R5 C5) LED33 Off
 , mouse    = KBD (KeyPos R5 C6) LED34 Off
 }

mach4Peri :: Peripherals
mach4Peri = Peripherals
 { all      = KBD (KeyPos R1 C7) LED3  Off
 , monitor1 = KBD (KeyPos R2 C7) LED8  Off
 , monitor2 = KBD (KeyPos R2 C8) LED7  Off
 , monitor3 = KBD (KeyPos R3 C7) LED17 Off
 , monitor4 = KBD (KeyPos R3 C8) LED18 Off
 , mic      = KBD (KeyPos R4 C7) LED24 Off
 , camera   = KBD (KeyPos R4 C8) LED23 Off
 , kbd      = KBD (KeyPos R5 C7) LED33 Off
 , mouse    = KBD (KeyPos R5 C8) LED34 Off
 }

keyboard :: Map Machine Peripherals
keyboard = fromList
  [ (Mach1, mach1Peri)
  , (Mach2, mach2Peri)
  , (Mach3, mach3Peri)
  , (Mach4, mach4Peri)
  ]

keyPosRow1 :: [KeyPos]
keyPosRow1 = 
  [ KeyPos R1 C1 
  , KeyPos R1 C3 
  , KeyPos R1 C5 
  , KeyPos R1 C7 
  ]

keyPosRow2 :: [KeyPos]
keyPosRow2 = 
  [ KeyPos R2 C1 
  , KeyPos R2 C2 
  , KeyPos R2 C3 
  , KeyPos R2 C4 
  , KeyPos R2 C5 
  , KeyPos R2 C6 
  , KeyPos R2 C7 
  , KeyPos R2 C8 
  ]

keyPosRow3 :: [KeyPos]
keyPosRow3 = 
  [ KeyPos R3 C1 
  , KeyPos R3 C2 
  , KeyPos R3 C3 
  , KeyPos R3 C4 
  , KeyPos R3 C5 
  , KeyPos R3 C6 
  , KeyPos R3 C7 
  , KeyPos R3 C8 
  ]

keyPosRow4 :: [KeyPos]
keyPosRow4 = 
  [ KeyPos R4 C1 
  , KeyPos R4 C2 
  , KeyPos R4 C3 
  , KeyPos R4 C4 
  , KeyPos R4 C5 
  , KeyPos R4 C6 
  , KeyPos R4 C7 
  , KeyPos R4 C8 
  ]

keyPosRow5 :: [KeyPos]
keyPosRow5 = 
  [ KeyPos R5 C1 
  , KeyPos R5 C2 
  , KeyPos R5 C3 
  , KeyPos R5 C4 
  , KeyPos R5 C5 
  , KeyPos R5 C6 
  , KeyPos R5 C7 
  , KeyPos R5 C8 
  ]

makeKeyForRow :: Pin -> [Pin] -> [Key]
makeKeyForRow row = map (Key row)

makeKeyForAllRows :: [Pin] -> [Pin] -> [Key]
makeKeyForAllRows rows columns = concatMap (`makeKeyForRow` columns) rows 

onlyEvenCols :: [Pin] -> [Pin]
onlyEvenCols [p0,_,p2,_,p4,_,p6] = [p0,p2,p4,p6]
onlyEvenCols _                   = error "Only accepts 8 columns"

makeKeyPosMap :: [Pin] -> [Pin] -> Map Key KeyPos
makeKeyPosMap (r1:rows) columns = fromList $
  zip (makeKeyForRow r1 (onlyEvenCols columns)) keyPosRow1
  <> zip (makeKeyForAllRows rows columns) 
         (keyPosRow2 <> keyPosRow3 <> keyPosRow4 <> keyPosRow5)
makeKeyPosMap _ _ = error "There must be at lease 1 row"

key2KeyPos :: Config -> Map Key KeyPos
key2KeyPos cfg = makeKeyPosMap cfg.rows cfg.columns
