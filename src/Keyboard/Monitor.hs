module Keyboard.Monitor
  ( monitorEvents
  , runKeyMonitor
  ) where

import           Config                 (loadConfig)
import           Control.Concurrent.STM (TBQueue, TMVar, atomically, orElse,
                                         readTBQueue, takeTMVar)
import           GPIO.Error             (GpioError)
import           GPIO.PinMap            (pinToBCM')
import           GPIO.Types             (Key(..))
import           Keyboard.Device       (withKeyboardScanner)
import           Keyboard.Diagnostics  (printGpioError,
                                         reportKeyboardStartupError)

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
