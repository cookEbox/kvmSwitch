module Board.Session
  ( boardEventLoop
  , runBoardSession
  , runBoardTest
  ) where

import           Board.Check         (validateBoardCheck)
import           Board.Frame         (renderBoardState)
import           Board.Mapping       (keyToAction)
import           Board.State         (BoardState, applyAction,
                                      initialBoardState)
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
import           NeoPixel            (PixelStrip, withPixelStrip)
import           Keyboard.LED.HardwareTest (bestEffortOff,
                                             reportError)

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
          scannerResult <- withKeyboardScanner cfg 
            (runBoardSession strip cfg)
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
