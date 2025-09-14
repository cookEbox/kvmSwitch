module GPIO.PinMap (pinMap, pinToBCM', pinToBCM) where

import           Data.Map.Strict   (Map, fromList)
import qualified Data.Map.Strict   as M  
import           Foreign.C         (CUInt (..))
import           GPIO.Types (Pin(..))

pinMap :: Map Pin CUInt
pinMap = fromList
  [ (P1,  1)
  , (P2,  2)
  , (P3,  3)
  , (P4,  4)
  , (P5,  5)
  , (P6,  6)
  , (P7,  7)
  , (P8,  8)
  , (P9,  9)
  , (P10, 10)
  , (P11, 11)
  , (P12, 12)
  , (P13, 13)
  , (P14, 14)
  , (P15, 15)
  , (P16, 16)
  , (P17, 17)
  , (P18, 18)
  , (P19, 19)
  , (P20, 20)
  , (P21, 21)
  , (P22, 22)
  , (P23, 23)
  , (P24, 24)
  , (P25, 25)
  , (P26, 26)
  ]

pinToBCM :: Pin -> Maybe CUInt
pinToBCM p = M.lookup p pinMap

pinToBCM' :: Pin -> CUInt
pinToBCM' p = case M.lookup p pinMap of
  Just bcm -> bcm
  Nothing  -> error ("Pin " <> show p <> " not mapped!")

