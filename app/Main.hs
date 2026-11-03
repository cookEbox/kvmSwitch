{-# LANGUAGE QualifiedDo #-}

module Main where

import           Control.Concurrent.STM (TMVar, TQueue, atomically, orElse,
                                         putTMVar, takeTMVar, tryTakeTMVar,
                                         writeTQueue)
import           Control.Concurrent     (threadDelay)
import           Control.Exception      (SomeException, bracket, onException, try)
import           Data.Map               (Map, findWithDefault, insert)
import           EitherDo.Edo           (IOEither, ok, traverseE_)
import qualified EitherDo.Edo           as E
import           MyLib
import           NeoPixel               (LedError(..), PixelStrip, RGB(..),
                                         PixelOrder(..), StripConfig(..),
                                         clearPixels, renderFrame, setBrightness,
                                         setPixel, showPixels, withPixelStrip)
import           System.IO              (BufferMode (..), hSetBuffering, stderr, stdout)
import           System.Environment     (getArgs)
import           GPIO.FFI              (c_gpiod_chip_close)
-- TODO: Move constants to config or config directory
-- Temporary for testing led code
type LedMap = Map LED Colour

keyToLed :: Key -> LED
keyToLed = undefined

toggleLed :: Key -> Colour -> LedMap -> LedMap
toggleLed key colour ledMap = do
  let led = keyToLed key
  case findWithDefault Off led ledMap of
    Off -> insert led colour ledMap
    _   -> insert led Off ledMap

initialLedMap :: LedMap
initialLedMap = undefined

updateLedMap :: Key -> TMVar LedMap -> TQueue LedMap -> IO ()
updateLedMap key lstV takeFromV = do
  lst <- atomically $ tryTakeTMVar lstV
  newLst <- case lst of
              Nothing  -> pure initialLedMap
              Just old -> pure $ toggleLed key White old
  atomically $ do putTMVar lstV newLst
                  writeTQueue takeFromV newLst

-- Start of Main

setup :: Config -> IOEither GpioError (Chip, [PinPtr], [PinPtr])
setup cfg = E.do
  chip <- openChipE cfg.chip
  (lnOut, lnIn) <- setupLines chip cfg
  ok (chip, lnOut, lnIn)

setupLines :: Chip -> Config -> IOEither GpioError ([PinPtr], [PinPtr])
setupLines chip cfg = E.do
  lnOut <- getPinPtrsE chip cfg.rows
  lnIn  <- getPinPtrsE chip cfg.columns
  _ <- traverseE_ outputE lnOut
  _ <- traverseE_ inputE lnIn
  ok (lnOut, lnIn)
  where
    getPinPtrsE c ps = sequence <$> mapM (getLineE c) ps
    outputE row = requestOutputE ("row GPIO" <> show (pinToBCM' row.pin))
      False row.ptr
    inputE column = requestInputE ("column GPIO" <> show (pinToBCM' column.pin))
      biasPullDown column.ptr

setRowsLow :: [PinPtr] -> IOEither GpioError ()
setRowsLow = traverseE_ (\row -> setValueE row.ptr False)

monitorEvents :: TMVar Key -> TMVar GpioError -> IO ()
monitorEvents keyVar errorVar = do
  event <- atomically $
    (Left <$> takeTMVar errorVar) `orElse` (Right <$> takeTMVar keyVar)
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
      opened <- openChipE cfg.chip
      case opened of
        Left err -> printGpioError "opening GPIO chip failed" err
        Right chip -> bracket (pure chip) c_gpiod_chip_close $ \openChip -> do
          configured <- setupLines openChip cfg
          case configured of
            Left err -> printGpioError "configuring keyboard GPIO failed" err
            Right (rows, columns) -> do
              scanner <- startScannerWithErrors openChip rows columns
                defaultScanPeriodUS defaultDebounceMS
              case scanner of
                Left err -> printGpioError "starting keyboard scanner failed" err
                Right (keys, errors, stopScanner) ->
                  bracket (pure stopScanner)
                    (\stop -> do
                      stop
                      lowered <- setRowsLow rows
                      case lowered of
                        Left err -> printGpioError "resetting keyboard rows failed" err
                        Right () -> pure ())
                    (\_ -> do
                      putStrLn "[kvmSwitch] Keyboard monitor started"
                      monitorEvents keys errors)


ledCount :: Int
ledCount = 36

ledConfig :: StripConfig
ledConfig = StripConfig
  { count = ledCount
  , gpio = 10
  , brightness = 32
  , order = OrderGRB
  }

offRGB :: RGB
offRGB = RGB 0 0 0

redRGB, greenRGB, blueRGB :: RGB
redRGB = RGB 255 0 0
greenRGB = RGB 0 255 0
blueRGB = RGB 0 0 255

allOff :: [RGB]
allOff = replicate ledCount offRGB

allOf :: RGB -> [RGB]
allOf colour = replicate ledCount colour

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

data TestFrame = TestFrame String [RGB]

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
runHardwareTest frames = withPixelStrip ledConfig $ \strip -> do
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

printUsage :: IO ()
printUsage = putStrLn
  "usage: kvmSwitch --key-monitor [CONFIG] | --led-self-test | --led-walking-test"

-- TODO: Move scanTime and debounceTime to a Constants directory
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  putStrLn "[kvmSwitch] starting…"
  args <- getArgs
  case args of
    ["--key-monitor"] -> defaultConfigPath >>= runKeyMonitor
    ["--key-monitor", configFile] -> runKeyMonitor configFile
    ["--led-self-test"] -> do
      result <- runHardwareTest standardFrames
      printResult result
    ["--led-walking-test"] -> do
      result <- runHardwareTest walkingFrames
      printResult result
    _ -> printUsage

printResult :: Either LedError () -> IO ()
printResult result = case result of
  Left errorValue -> putStrLn $ "[kvmSwitch] LED test failed: " <> show errorValue
  Right () -> putStrLn "[kvmSwitch] LED test completed successfully"
