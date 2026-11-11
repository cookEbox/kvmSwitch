module Keyboard.Diagnostics
  ( printGpioError
  , reportKeyboardStartupError
  ) where

import           GPIO.Error      (GpioError)
import           Keyboard.Device (KeyboardStartupError(..))

printGpioError :: String -> GpioError -> IO ()
printGpioError context err =
  putStrLn $ "[kvmSwitch] ERROR: " <> context <> ": " <> show err

reportKeyboardStartupError :: KeyboardStartupError -> IO ()
reportKeyboardStartupError startupError = case startupError of
  KeyboardChipOpenFailed err -> printGpioError "opening GPIO chip failed" err
  KeyboardLineConfigurationFailed err ->
    printGpioError "configuring keyboard GPIO failed" err
  KeyboardScannerStartupFailed err ->
    printGpioError "starting keyboard scanner failed" err
