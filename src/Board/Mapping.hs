module Board.Mapping
  ( ActionMapping
  , machines
  , machineMappings
  , allActionMappings
  , peripheralActionMappings
  , keyToAction
  , expectedActions
  ) where

import           Board.State       (KeyAction(..), Peripheral(..), peripherals)
import           Config            (Config)
import qualified Data.Map.Strict   as Map
import           GPIO.Types        (Key)
import qualified Keyboard.Keys     as Keys
import           Keyboard.Keys     (KeyPos, Machine(..), key2KeyPos, keyboard)
import           Keyboard.LED      (LED)

type ActionMapping = (KeyPos, KeyAction, LED)

machines :: [Machine]
machines = [Mach1, Mach2, Mach3, Mach4]

machineMappings :: (Machine, Keys.Peripherals) -> [ActionMapping]
machineMappings (machine, Keys.Peripherals allKeys monitor1 monitor2 monitor3
                              monitor4 mic camera kbd mouse) =
  [ mapping allKeys (SelectAll machine)
  , mapping monitor1 (SelectPeripheral Monitor1 machine)
  , mapping monitor2 (SelectPeripheral Monitor2 machine)
  , mapping monitor3 (SelectPeripheral Monitor3 machine)
  , mapping monitor4 (SelectPeripheral Monitor4 machine)
  , mapping mic      (SelectPeripheral Mic machine)
  , mapping camera   (SelectPeripheral Camera machine)
  , mapping kbd      (SelectPeripheral Keyboard machine)
  , mapping mouse    (SelectPeripheral Mouse machine)
  ]
  where
    mapping (Keys.KBD position led _) action = (position, action, led)

allActionMappings :: [ActionMapping]
allActionMappings = concatMap machineMappings (Map.toList keyboard)

peripheralActionMappings :: [(KeyPos, Peripheral, Machine, LED)]
peripheralActionMappings =
  [ (position, peripheral, machine, led)
  | (position, action, led) <- allActionMappings
  , (peripheral, machine) <- case action of
      SelectPeripheral p m -> [(p, m)]
      SelectAll _           -> []
  ]

keyToAction :: Config -> Key -> Maybe KeyAction
keyToAction cfg physicalKey = case Map.lookup physicalKey (key2KeyPos cfg) of
  Nothing -> Nothing
  Just position -> case
    [ action | (mappedPosition, action, _) <- allActionMappings
             , mappedPosition == position
    ] of
      [action] -> Just action
      _        -> Nothing

expectedActions :: [KeyAction]
expectedActions =
  [ SelectAll machine | machine <- machines ]
  <> [ SelectPeripheral peripheral machine
     | machine <- machines
     , peripheral <- peripherals
     ]
