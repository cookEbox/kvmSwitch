module Board.Session
  ( boardEventLoop
  , runBoardSession
  , runBoardTest
  ) where

import           Board.Check         (validateBoardCheck)
import           Board.Frame         (renderBoardState)
import           Board.Mapping       (keyToAction)
import           Board.State         (BoardState, applyAction,
                                      initialBoardState, KeyAction)
import           Config              (Config, loadConfig)
import           Control.Concurrent.STM (TBQueue, TVar, TMVar, atomically,
                                         newTVarIO, orElse, readTBQueue,
                                         readTVar, takeTMVar, writeTVar)
import           Control.Exception   (onException)
import           GPIO.Error          (GpioError)
import           GPIO.PinMap         (pinToBCM')
import           GPIO.Types          (Key(..))
import           Keyboard.Device     (withKeyboardScanner)
import           Keyboard.Diagnostics (printGpioError,
                                        reportKeyboardStartupError)
import           Keyboard.LED        (ledStripConfig)
import           NeoPixel            (PixelStrip, withPixelStrip, LedError)
import           Keyboard.LED.HardwareTest (bestEffortOff,
                                             reportError)


-- Board events: read keys, resolve actions, update state and render LEDs
reportScanFailure :: GpioError -> IO (Either String ())
reportScanFailure err = do
  printGpioError "keyboard scan failed" err
  pure (Left (show err))

logKeyPress :: Key -> IO ()
logKeyPress (Key row column) =
  putStrLn $
    "[kvmSwitch] key pressed: row=GPIO"
      <> show (pinToBCM' row)
      <> " column=GPIO"
      <> show (pinToBCM' column)

updateBoardState :: TVar BoardState -> KeyAction -> IO BoardState
updateBoardState stateVar action =
  atomically $ do
    oldState <- readTVar stateVar
    let updatedState = applyAction action oldState
    writeTVar stateVar updatedState
    pure updatedState

runAction
  :: PixelStrip
  -> TVar BoardState
  -> KeyAction
  -> IO (Either String ())
runAction strip stateVar action = do
  putStrLn $ "[kvmSwitch] action: " <> show action
  newState <- updateBoardState stateVar action
  rendered <- renderBoardState strip newState
  case rendered of
    Left err -> do
      putStrLn $ "[kvmSwitch] board LED render failed: " <> err
      pure (Left err)
    Right () ->
      pure (Right ())

processKeyPress
  :: PixelStrip
  -> Config
  -> TVar BoardState
  -> Key
  -> IO (Either String ())
processKeyPress strip cfg stateVar physicalKey = do
  logKeyPress physicalKey
  case keyToAction cfg physicalKey of
    Nothing -> do
      putStrLn $ "[kvmSwitch] unmapped key: " <> show physicalKey
      pure (Right ())
    Just action ->
      runAction strip stateVar action

readBoardEvent
  :: TBQueue Key
  -> TMVar GpioError
  -> IO (Either GpioError Key)
readBoardEvent keyVar errorVar =
  atomically $
    (Left <$> takeTMVar errorVar)
      `orElse`
    (Right <$> readTBQueue keyVar)

boardEventLoop
  :: PixelStrip
  -> Config
  -> TVar BoardState
  -> TBQueue Key
  -> TMVar GpioError
  -> IO (Either String ())
boardEventLoop strip cfg stateVar keyVar errorVar = do
  event <- readBoardEvent keyVar errorVar
  result <- either reportScanFailure
                   (processKeyPress strip cfg stateVar)
                   event
  case result of
    Left err -> pure (Left err)
    Right () -> boardEventLoop strip cfg stateVar keyVar errorVar


-- Board session: initialise state and start the event loop
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


-- Board startup: configuration, hardware acquisition and cleanup
reportSessionResult :: Either String () -> IO ()
reportSessionResult result =
  case result of
    Left err ->
      putStrLn $ "[kvmSwitch] board test stopped: " <> err
    Right () ->
      putStrLn "[kvmSwitch] Board test stopped"

runWithStrip
  :: Config
  -> PixelStrip
  -> IO (Either LedError (Either String ()))
runWithStrip cfg strip = do
  scannerResult <-
    withKeyboardScanner cfg (runBoardSession strip cfg)
      `onException` bestEffortOff strip
  bestEffortOff strip
  case scannerResult of
    Left err -> do
      reportKeyboardStartupError err
      pure (Right (Left "keyboard scanner startup failed"))
    Right sessionResult ->
      pure (Right sessionResult)

reportValidationError :: String -> IO ()
reportValidationError err =
  putStrLn $ "[kvmSwitch] board-test validation failed: " <> err

reportConfigError :: String -> IO ()
reportConfigError err =
  putStrLn $ "[kvmSwitch] invalid board-test configuration: " <> err

startBoardHardware :: Config -> IO ()
startBoardHardware cfg = do
  result <- withPixelStrip ledStripConfig (runWithStrip cfg)
  either (reportError "board test") reportSessionResult result

validateAndRun :: Config -> IO ()
validateAndRun cfg =
  either reportValidationError
         (\() -> startBoardHardware cfg)
         (validateBoardCheck cfg)

runBoardTest :: FilePath -> IO ()
runBoardTest configFile = do
  loaded <- loadConfig configFile
  either reportConfigError validateAndRun loaded
