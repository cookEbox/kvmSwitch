module Board.Frame
  ( frameForState
  , renderBoardState
  ) where

import           Board.Mapping       (allActionMappings,
                                      peripheralActionMappings)
import           Board.State         (BoardState(..), KeyAction(..),
                                      allAssignedTo, validBoardState)
import           Data.List            (nub, sort)
import qualified Data.Map.Strict     as Map
import           Keyboard.LED        (Colour(..), ledCount, ledIndex,
                                      toRGB)
import           NeoPixel            (PixelStrip, RGB, renderFrame)
import Data.Bifunctor (Bifunctor(first))

frameForState :: BoardState -> Either String [RGB]
frameForState state@(BoardState owners)
  | not (validBoardState state) = Left "board state does not contain exactly eight owners"
  | sort allLEDIndexes /= [0 .. ledCount - 1] =
      Left "keyboard definition contains invalid or duplicate LED assignments"
  | length selectedLEDs /= 8 && length selectedLEDs /= 9 =
      Left "board state selected LED count is invalid"
  | length (nub selectedLEDIndexes) /= length selectedLEDIndexes =
      Left "board state selects duplicate LED positions"
  | otherwise = Right
      [ if index `elem` selectedLEDIndexes then toRGB White else toRGB Off
      | index <- [0 .. ledCount - 1]
      ]
  where
    allLEDIndexes = [ledIndex led | (_, _, led) <- allActionMappings]
    selectedPeripheralLEDs =
      [ led
      | (_, peripheral, machine, led) <- peripheralActionMappings
      , Map.lookup peripheral owners == Just machine
      ]
    selectedAllLEDs =
      [ led
      | (_, action, led) <- allActionMappings
      , SelectAll machine <- [action]
      , allAssignedTo machine state
      ]
    selectedLEDs = selectedPeripheralLEDs <> selectedAllLEDs
    selectedLEDIndexes = map ledIndex selectedLEDs

renderBoardState :: PixelStrip -> BoardState -> IO (Either String ())
renderBoardState strip state = case frameForState state of
  Left err    -> pure (Left err)
  Right frame -> first show <$> renderFrame strip frame
