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
machineMappings (machine, keyPeripheral) =
  [ mapping keyPeripheral.all      (SelectAll machine)
  , mapping keyPeripheral.monitor1 (SelectPeripheral Monitor1 machine)
  , mapping keyPeripheral.monitor2 (SelectPeripheral Monitor2 machine)
  , mapping keyPeripheral.monitor3 (SelectPeripheral Monitor3 machine)
  , mapping keyPeripheral.monitor4 (SelectPeripheral Monitor4 machine)
  , mapping keyPeripheral.mic      (SelectPeripheral Mic machine)
  , mapping keyPeripheral.camera   (SelectPeripheral Camera machine)
  , mapping keyPeripheral.kbd      (SelectPeripheral Keyboard machine)
  , mapping keyPeripheral.mouse    (SelectPeripheral Mouse machine)
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
      SelectAll _          -> []
  ]

keyToAction :: Config -> Key -> Maybe KeyAction
keyToAction cfg physicalKey = do
  position <- Map.lookup physicalKey (key2KeyPos cfg)
  case possibleActions position of
    [action] -> Just action
    _        -> Nothing
  where
    possibleActions position =
      [ action
      | (mappedPosition, action, _) <- allActionMappings
      , mappedPosition == position
      ]

expectedActions :: [KeyAction]
expectedActions =
  [ SelectAll machine | machine <- machines ]
  <> [ SelectPeripheral peripheral machine
     | machine <- machines
     , peripheral <- peripherals
     ]
