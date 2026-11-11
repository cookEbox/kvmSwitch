module Board.State
  ( Peripheral(..)
  , KeyAction(..)
  , BoardState(..)
  , peripherals
  , initialBoardState
  , applyAction
  , allAssignedTo
  , validBoardState
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Keyboard.Keys   (Machine(..))

data Peripheral
  = Monitor1
  | Monitor2
  | Monitor3
  | Monitor4
  | Mic
  | Camera
  | Keyboard
  | Mouse
  deriving (Eq, Ord, Show, Enum, Bounded)

data KeyAction
  = SelectAll Machine
  | SelectPeripheral Peripheral Machine
  deriving (Eq, Show)

newtype BoardState = BoardState (Map Peripheral Machine)
  deriving (Eq, Show)

peripherals :: [Peripheral]
peripherals = [minBound .. maxBound]

initialBoardState :: BoardState
initialBoardState =
  BoardState $
    Map.fromList
      [ (peripheral, Mach1)
      | peripheral <- peripherals
      ]

applyAction :: KeyAction -> BoardState -> BoardState
applyAction action (BoardState owners) = case action of
  SelectAll machine ->
    BoardState $ Map.fromList
      [ (peripheral, machine)
      | peripheral <- peripherals
      ]
  SelectPeripheral peripheral machine ->
    BoardState (Map.insert peripheral machine owners)

allAssignedTo :: Machine -> BoardState -> Bool
allAssignedTo machine (BoardState owners) =
  all (== Just machine)
    [ Map.lookup peripheral owners
    | peripheral <- peripherals
    ]

validBoardState :: BoardState -> Bool
validBoardState (BoardState owners) =
  Map.keys owners == peripherals
