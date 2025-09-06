module GPIO.PinMap (pinMap, pinToBCM', pinToBCM) where

import           Data.Map.Strict   (Map, fromList)
import qualified Data.Map.Strict   as M  
import           Foreign.C         (CUInt (..))
import           GPIO.Types (Pin(..))

pinMap :: Map Pin CUInt
pinMap = fromList
  [ (P1,  28)
  , (P2,  3)
  , (P3,  5)
  , (P4,  7)
  , (P5,  29)
  , (P6,  31)
  , (P7,  26)
  , (P8,  33)
  , (P9,  21)
  , (P10, 19)
  , (P11, 23)
  , (P12, 32)
  , (P13, 33)
  , (P14, 8)
  , (P15, 10)
  , (P16, 36)
  , (P17, 11)
  , (P18, 12)
  , (P19, 35)
  , (P20, 38)
  , (P21, 40)
  , (P22, 15)
  , (P23, 16)
  , (P24, 18)
  , (P25, 22)
  , (P26, 37)
  ]

pinToBCM :: Pin -> Maybe CUInt
pinToBCM p = M.lookup p pinMap

pinToBCM' :: Pin -> CUInt
pinToBCM' p = case M.lookup p pinMap of
  Just bcm -> bcm
  Nothing  -> error ("Pin " <> show p <> " not mapped!")

