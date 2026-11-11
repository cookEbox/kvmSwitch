module Keyboard.LED.HardwareTest
  ( TestFrame(..)
  , offRGB
  , redRGB
  , greenRGB
  , blueRGB
  , allOff
  , allOf
  , atNativeIndex
  , alternating
  , standardFrames
  , walkingFrames
  , renderNamed
  , runFrames
  , reportError
  , runCommands
  , showAndPause
  , commandApiDemo
  , runTestSequence
  , bestEffortOff
  , runHardwareTest
  , printResult
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Exception  (SomeException, onException, try)
import           Keyboard.LED        (ledCount, ledStripConfig)
import           NeoPixel           (LedError, PixelStrip, RGB(..),
                                      clearPixels, renderFrame, setBrightness,
                                      setPixel, showPixels, withPixelStrip)

data TestFrame = TestFrame String [RGB]

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
