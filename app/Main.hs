{-# LANGUAGE QualifiedDo #-}

module Main (main) where

import           Control.Concurrent.STM (TBQueue, TVar, TMVar, atomically,
                                         newTVarIO, orElse, readTBQueue,
                                         readTVar, takeTMVar, writeTVar)
import           Control.Concurrent     (threadDelay)
import           Control.Exception      (SomeException, onException, try)
import           Config                 (Config, defaultConfigPath, loadConfig)
import           Board.State            (BoardState(..), KeyAction(..),
                                         Peripheral(..), allAssignedTo,
                                         applyAction, initialBoardState,
                                         peripherals, validBoardState)
import           Data.List              (nub, sort)
import qualified Data.Map.Strict        as Map
import           GPIO.Error             (GpioError)
import           GPIO.PinMap            (pinToBCM')
import           GPIO.Types             (Key(..), Pin(..))
import           Keyboard.Device       (KeyboardStartupError(..),
                                         withKeyboardScanner)
import qualified Keyboard.Keys          as Keys
import           Keyboard.Keys          (KeyPos(..), Machine(..),
                                         key2KeyPos, keyboard)
import           Keyboard.LED          (Colour(..), LED(..), ledCount,
                                         ledIndex, ledStripConfig, toRGB)
import           NeoPixel               (LedError(..), PixelStrip, RGB(..),
                                         clearPixels, renderFrame, setBrightness,
                                         setPixel, showPixels, withPixelStrip)
import           System.IO              (BufferMode (..), hSetBuffering, stderr, stdout)
import           System.Environment     (getArgs)
import           System.Exit            (exitFailure)

type ActionMapping = (KeyPos, KeyAction, LED)

data TestFrame = TestFrame String [RGB]

monitorEvents :: TBQueue Key -> TMVar GpioError -> IO ()
monitorEvents keyVar errorVar = do
  event <- atomically $
    (Left <$> takeTMVar errorVar) `orElse` (Right <$> readTBQueue keyVar)
  case event of
    Left err -> printGpioError "keyboard scan failed" err
    Right (Key row column) -> do
      putStrLn $ "[kvmSwitch] key pressed: row=GPIO"
        <> show (pinToBCM' row)
        <> " column=GPIO"
        <> show (pinToBCM' column)
      monitorEvents keyVar errorVar

printGpioError :: String -> GpioError -> IO ()
printGpioError context err =
  putStrLn $ "[kvmSwitch] ERROR: " <> context <> ": " <> show err

runKeyMonitor :: FilePath -> IO ()
runKeyMonitor configFile = do
  loaded <- loadConfig configFile
  case loaded of
    Left err -> putStrLn $ "[kvmSwitch] ERROR: invalid keyboard configuration: " <> err
    Right cfg -> do
      result <- withKeyboardScanner cfg $ \keys errors -> do
        putStrLn "[kvmSwitch] Keyboard monitor started"
        monitorEvents keys errors
      case result of
        Left err -> reportKeyboardStartupError err
        Right () -> pure ()

reportKeyboardStartupError :: KeyboardStartupError -> IO ()
reportKeyboardStartupError startupError = case startupError of
  KeyboardChipOpenFailed err -> printGpioError "opening GPIO chip failed" err
  KeyboardLineConfigurationFailed err ->
    printGpioError "configuring keyboard GPIO failed" err
  KeyboardScannerStartupFailed err ->
    printGpioError "starting keyboard scanner failed" err

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
  Left err -> pure (Left err)
  Right frame -> do
    rendered <- renderFrame strip frame
    pure $ case rendered of
      Left err -> Left (show err)
      Right () -> Right ()

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

boardEventLoop
  :: PixelStrip
  -> Config
  -> TVar BoardState
  -> TBQueue Key
  -> TMVar GpioError
  -> IO (Either String ())
boardEventLoop strip cfg stateVar keyVar errorVar = do
  event <- atomically $
    (Left <$> takeTMVar errorVar) `orElse` (Right <$> readTBQueue keyVar)
  case event of
    Left err -> do
      printGpioError "keyboard scan failed" err
      pure (Left (show err))
    Right physicalKey -> do
      putStrLn $ "[kvmSwitch] key pressed: row=GPIO"
        <> show (pinToBCM' (keyRow physicalKey))
        <> " column=GPIO"
        <> show (pinToBCM' (keyColumn physicalKey))
      case keyToAction cfg physicalKey of
        Nothing -> do
          putStrLn $ "[kvmSwitch] unmapped key: " <> show physicalKey
          boardEventLoop strip cfg stateVar keyVar errorVar
        Just action -> do
          putStrLn $ "[kvmSwitch] action: " <> show action
          newState <- atomically $ do
            oldState <- readTVar stateVar
            let updatedState = applyAction action oldState
            writeTVar stateVar updatedState
            pure updatedState
          rendered <- renderBoardState strip newState
          case rendered of
            Left err -> do
              putStrLn $ "[kvmSwitch] board LED render failed: " <> err
              pure (Left err)
            Right () -> boardEventLoop strip cfg stateVar keyVar errorVar
  where
    keyRow (Key row _) = row
    keyColumn (Key _ column) = column

runBoardSession
  :: PixelStrip
  -> Config
  -> TBQueue Key
  -> TMVar GpioError
  -> IO (Either String ())
runBoardSession strip cfg keyVar errorVar = do
  stateVar <- newTVarIO initialBoardState
  initialRender <- renderBoardState strip initialBoardState
  case initialRender of
    Left err -> do
      putStrLn $ "[kvmSwitch] initial board render failed: " <> err
      pure (Left err)
    Right () -> do
      putStrLn "[kvmSwitch] Board test started"
      boardEventLoop strip cfg stateVar keyVar errorVar

runBoardTest :: FilePath -> IO ()
runBoardTest configFile = do
  loaded <- loadConfig configFile
  case loaded of
    Left err -> putStrLn $ "[kvmSwitch] invalid board-test configuration: " <> err
    Right cfg -> case validateBoardCheck cfg of
      Left err -> putStrLn $ "[kvmSwitch] board-test validation failed: " <> err
      Right () -> do
        result <- withPixelStrip ledStripConfig $ \strip -> do
          scannerResult <- (withKeyboardScanner cfg $ \keys errors ->
              runBoardSession strip cfg keys errors)
            `onException` bestEffortOff strip
          bestEffortOff strip
          case scannerResult of
            Left err -> do
              reportKeyboardStartupError err
              pure (Right (Left "keyboard scanner startup failed"))
            Right sessionResult -> pure (Right sessionResult)
        case result of
          Left err -> reportError "board test" err
          Right (Left err) -> putStrLn $ "[kvmSwitch] board test stopped: " <> err
          Right (Right ()) -> putStrLn "[kvmSwitch] Board test stopped"

offRGB :: RGB
offRGB = RGB 0 0 0

redRGB, greenRGB, blueRGB :: RGB
redRGB = RGB 255 0 0
greenRGB = RGB 0 255 0
blueRGB = RGB 0 0 255

allOff :: [RGB]
allOff = replicate ledCount offRGB

allOf :: RGB -> [RGB]
allOf = replicate ledCount

atNativeIndex :: Int -> RGB -> [RGB]
atNativeIndex nativeIndex colour =
  [ if index == nativeIndex then colour else offRGB
  | index <- [0 .. ledCount - 1]
  ]

alternating :: [RGB]
alternating =
  [ if even index then redRGB else offRGB
  | index <- [0 .. ledCount - 1]
  ]

standardFrames :: [TestFrame]
standardFrames =
  [ TestFrame "all 36 LEDs off" allOff
  , TestFrame "LED 1 red" (atNativeIndex 0 redRGB)
  , TestFrame "LED 4 red" (atNativeIndex 3 redRGB)
  , TestFrame "LED 36 red" (atNativeIndex 35 redRGB)
  , TestFrame "all 36 LEDs green" (allOf greenRGB)
  , TestFrame "all 36 LEDs blue" (allOf blueRGB)
  , TestFrame "alternating red LEDs" alternating
  , TestFrame "all 36 LEDs off" allOff
  ]
  <> concat
    [ [ TestFrame ("repeat " <> show repetition <> "/6 on") alternating
      , TestFrame ("repeat " <> show repetition <> "/6 off") allOff
      ]
    | repetition <- [1 .. (6 :: Int)]
    ]
  <> [TestFrame "final all-off frame" allOff]

walkingFrames :: [TestFrame]
walkingFrames =
  [ TestFrame ("walking pixel, LED " <> show (nativeIndex + 1))
      (atNativeIndex nativeIndex redRGB)
  | nativeIndex <- [0 .. ledCount - 1]
  ]
  <> [TestFrame "walking test final all-off frame" allOff]

renderNamed :: PixelStrip -> TestFrame -> IO (Either LedError ())
renderNamed strip (TestFrame name frame) = do
  putStrLn $ "[kvmSwitch] displaying " <> name
  result <- renderFrame strip frame
  case result of
    Left errorValue -> reportError ("rendering " <> name) errorValue
    Right () -> threadDelay 5_000_000
  pure result

runFrames :: PixelStrip -> [TestFrame] -> IO (Either LedError ())
runFrames _ [] = pure (Right ())
runFrames strip (frame : remaining) = do
  result <- renderNamed strip frame
  case result of
    Left errorValue -> pure (Left errorValue)
    Right () -> runFrames strip remaining

reportError :: String -> LedError -> IO ()
reportError operation errorValue = do
  putStrLn $ "[kvmSwitch] ERROR: " <> operation <> " failed: " <> show errorValue

runCommands
  :: [(String, IO (Either LedError ()))]
  -> IO (Either LedError ())
runCommands [] = pure (Right ())
runCommands ((name, action) : remaining) = do
  putStrLn $ "[kvmSwitch] command: " <> name
  result <- action
  case result of
    Left errorValue -> do
      reportError name errorValue
      pure (Left errorValue)
    Right () -> runCommands remaining

showAndPause :: IO (Either LedError ()) -> IO (Either LedError ())
showAndPause action = do
  result <- action
  case result of
    Left _ -> pure result
    Right () -> threadDelay 5_000_000 >> pure (Right ())

commandApiDemo :: PixelStrip -> IO (Either LedError ())
commandApiDemo strip = runCommands
  [ ("clear buffered pixels", showAndPause (clearPixels strip))
  , ("set LED 1 red", setPixel strip 0 redRGB)
  , ("show LED 1", showAndPause (showPixels strip))
  , ("set LED 4 green", setPixel strip 3 greenRGB)
  , ("set LED 36 blue", setPixel strip 35 blueRGB)
  , ("show multiple buffered pixels", showAndPause (showPixels strip))
  , ("set brightness to 16", setBrightness strip 16)
  , ("show at reduced brightness", showAndPause (showPixels strip))
  , ("clear and transmit", showAndPause (clearPixels strip))
  , ("render a complete frame", showAndPause (renderFrame strip (allOf blueRGB)))
  , ("clear complete frame", showAndPause (clearPixels strip))
  , ("restore brightness to 32", setBrightness strip 32)
  ]

runTestSequence :: PixelStrip -> [TestFrame] -> IO (Either LedError ())
runTestSequence strip frames = do
  demoResult <- commandApiDemo strip
  case demoResult of
    Left errorValue -> pure (Left errorValue)
    Right () -> runFrames strip frames

bestEffortOff :: PixelStrip -> IO ()
bestEffortOff strip = do
  result <- try (renderFrame strip allOff) :: IO (Either SomeException (Either LedError ()))
  case result of
    Left exceptionValue ->
      putStrLn $ "[kvmSwitch] WARNING: best-effort all-off failed: " <> show exceptionValue
    Right (Left errorValue) ->
      putStrLn $ "[kvmSwitch] WARNING: best-effort all-off failed: " <> show errorValue
    Right (Right ()) -> pure ()

runHardwareTest :: [TestFrame] -> IO (Either LedError ())
runHardwareTest frames = withPixelStrip ledStripConfig $ \strip -> do
  result <- runTestSequence strip frames `onException` bestEffortOff strip
  case result of
    Left errorValue -> do
      putStrLn "[kvmSwitch] stopping sequence; sending shutdown all-off frame"
      cleanupResult <- renderNamed strip (TestFrame "shutdown all-off frame" allOff)
      case cleanupResult of
        Left cleanupError ->
          putStrLn $ "[kvmSwitch] shutdown all-off also failed: " <> show cleanupError
        Right () -> pure ()
      pure (Left errorValue)
    Right () -> pure (Right ())

printResult :: Either LedError () -> IO ()
printResult result = case result of
  Left errorValue -> putStrLn $ "[kvmSwitch] LED test failed: " <> show errorValue
  Right () -> putStrLn "[kvmSwitch] LED test completed successfully"

printUsage :: IO ()
printUsage = putStrLn
  "usage: kvmSwitch --key-monitor [CONFIG] | --board-test [CONFIG] | --board-check [CONFIG] | --led-self-test | --led-walking-test"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  putStrLn "[kvmSwitch] starting…"
  args <- getArgs
  case args of
    ["--key-monitor"] -> defaultConfigPath >>= runKeyMonitor
    ["--key-monitor", configFile] -> runKeyMonitor configFile
    ["--board-test"] -> defaultConfigPath >>= runBoardTest
    ["--board-test", configFile] -> runBoardTest configFile
    ["--board-check"] -> defaultConfigPath >>= runBoardCheck
    ["--board-check", configFile] -> runBoardCheck configFile
    ["--led-self-test"] -> do
      result <- runHardwareTest standardFrames
      printResult result
    ["--led-walking-test"] -> do
      result <- runHardwareTest walkingFrames
      printResult result
    _ -> printUsage
