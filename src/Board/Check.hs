module Board.Check
  ( validateBoardConfiguration
  , boardStateChecks
  , validateBoardCheck
  , runBoardCheck
  ) where

import           Board.Frame         (frameForState)
import           Board.Mapping       (allActionMappings, expectedActions,
                                      keyToAction)
import           Board.State         (BoardState(..), KeyAction(..),
                                      Peripheral(..), allAssignedTo, applyAction,
                                      initialBoardState, peripherals,
                                      validBoardState)
import           Config              (Config, loadConfig)
import           Data.List            (nub, sort)
import qualified Data.Map.Strict     as Map
import           GPIO.Types           (Key(..), Pin(..))
import           Keyboard.Keys        (Machine(..), key2KeyPos)
import           Keyboard.LED         (Colour(..), ledCount, ledIndex, toRGB)
import           System.Exit          (exitFailure)

validateBoardConfiguration :: Config -> Either String ()
validateBoardConfiguration cfg = do
  let physicalMap = key2KeyPos cfg
      mappings = allActionMappings
      positions = [position | (position, _, _) <- mappings]
      leds = [led | (_, _, led) <- mappings]
      actions = [action | (_, action, _) <- mappings]
      physicalKeys = Map.keys physicalMap
      resolvedActions = [action | key <- physicalKeys
                                , Just action <- [keyToAction cfg key]]
  require (length mappings == 36) "keyboard definition must contain 36 entries"
  require (length (nub positions) == 36)
    "keyboard definition contains duplicate logical positions"
  require (sort (map ledIndex leds) == [0 .. ledCount - 1])
    "keyboard definition must use every LED exactly once"
  require (length physicalKeys == 36)
    "physical keyboard mapping must contain 36 keys"
  require (length resolvedActions == length physicalKeys)
    "one or more physical keys have no logical action"
  require (length (nub resolvedActions) == length resolvedActions)
    "multiple physical keys resolve to the same logical action"
  require (length actions == length expectedActions
        && all (`elem` actions) expectedActions)
    "logical action mapping is incomplete"
  require (keyToAction cfg (Key P25 P17) == Just (SelectAll Mach1))
    "R1 C1 does not map to SelectAll Mach1"
  require (keyToAction cfg (Key P25 P24) == Just (SelectAll Mach2))
    "R1 C3 does not map to SelectAll Mach2"
  require (keyToAction cfg (Key P25 P19) == Just (SelectAll Mach3))
    "R1 C5 does not map to SelectAll Mach3"
  require (keyToAction cfg (Key P25 P15) == Just (SelectAll Mach4))
    "R1 C7 does not map to SelectAll Mach4"
  where
    require True _ = Right ()
    require False message = Left message

boardStateChecks :: Either String ()
boardStateChecks = do
  require (allAssignedTo Mach1 initialBoardState)
    "initial board state is not all Mach1"
  checkFrame initialBoardState 9 "initial frame"
  let monitor3 = applyAction (SelectPeripheral Monitor1 Mach3)
        initialBoardState
      monitor3Again = applyAction (SelectPeripheral Monitor1 Mach3) monitor3
      allMach2 = applyAction (SelectAll Mach2) initialBoardState
      allMach1Again = foldl
        (\state peripheral -> applyAction (SelectPeripheral peripheral Mach1) state)
        allMach2 peripherals
  require (ownerOf Monitor1 monitor3 == Just Mach3)
    "Monitor1 was not assigned to Mach3"
  require (not (allAssignedTo Mach1 monitor3)
        && not (any (`allAssignedTo` monitor3) [Mach2, Mach3, Mach4]))
    "mixed state has an incorrect All indicator"
  require (monitor3Again == monitor3)
    "selecting an already selected peripheral toggled it"
  checkFrame allMach2 9 "all Mach2 frame"
  require (allAssignedTo Mach1 allMach1Again)
    "returning every peripheral to Mach1 did not restore All Mach1"
  require (validBoardState allMach1Again)
    "state transition produced an invalid board state"
  where
    ownerOf peripheral (BoardState owners) = Map.lookup peripheral owners
    checkFrame state expected label = case frameForState state of
      Left err -> Left (label <> ": " <> err)
      Right frame -> require (length frame == ledCount
                           && length (filter (/= toRGB Off) frame) == expected)
                           (label <> " has an invalid frame")
    require True _ = Right ()
    require False message = Left message

validateBoardCheck :: Config -> Either String ()
validateBoardCheck cfg = do
  validateBoardConfiguration cfg
  boardStateChecks

runBoardCheck :: FilePath -> IO ()
runBoardCheck configFile = do
  loaded <- loadConfig configFile
  case loaded of
    Left err -> do
      putStrLn $ "[kvmSwitch] board check failed: " <> err
      exitFailure
    Right cfg -> case validateBoardCheck cfg of
      Left err -> do
        putStrLn $ "[kvmSwitch] board check failed: " <> err
        exitFailure
      Right () -> putStrLn "[kvmSwitch] board check passed"
