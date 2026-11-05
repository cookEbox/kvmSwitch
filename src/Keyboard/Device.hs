{-# LANGUAGE QualifiedDo #-}

module Keyboard.Device
  ( KeyboardStartupError(..)
  , withKeyboardScanner
  ) where

import           Config              (Config(..))
import           Control.Concurrent.STM (TMVar)
import           Control.Exception   (bracket, mask, onException)
import           EitherDo.Edo        (IOEither, traverseE_)
import qualified EitherDo.Edo        as E
import           GPIO.Error          (GpioError)
import           GPIO.FFI            (c_gpiod_chip_close)
import           GPIO.Libgpiod       (getLineE, openChipE, requestInputE,
                                      requestOutputE, setValueE)
import           GPIO.PinMap         (pinToBCM')
import           GPIO.Types          (Chip, Key, Pin, PinPtr(..), biasPullDown)
import           Scan                (defaultDebounceMS, defaultScanPeriodUS,
                                      startScannerWithErrors)

data KeyboardStartupError
  = KeyboardChipOpenFailed GpioError
  | KeyboardLineConfigurationFailed GpioError
  | KeyboardScannerStartupFailed GpioError
  deriving (Show)

withKeyboardScanner
  :: Config
  -> (TMVar Key -> TMVar GpioError -> IO a)
  -> IO (Either KeyboardStartupError a)
withKeyboardScanner (Config chipName rowPins columnPins) action = do
  opened <- openChipE chipName
  case opened of
    Left err -> pure (Left (KeyboardChipOpenFailed err))
    Right chip -> bracket (pure chip) closeChip withChip
  where
    withChip chip = mask $ \restore -> do
      configured <- setupLines chip (Config chipName rowPins columnPins)
      case configured of
        Left err -> do
          pure (Left (KeyboardLineConfigurationFailed err))
        Right (rows, columns) -> do
          scanner <- startScannerWithErrors chip rows columns
            defaultScanPeriodUS defaultDebounceMS
          case scanner of
            Left err -> do
              cleanupRows rows
              pure (Left (KeyboardScannerStartupFailed err))
            Right (keys, errors, stopScanner) -> do
              result <- restore (action keys errors)
                `onException` cleanup stopScanner rows
              cleanup stopScanner rows
              pure (Right result)

    cleanup :: IO () -> [PinPtr] -> IO ()
    cleanup stopScanner rows = do
      stopScanner
      cleanupRows rows

    closeChip :: Chip -> IO ()
    closeChip = c_gpiod_chip_close

    cleanupRows :: [PinPtr] -> IO ()
    cleanupRows rows = do
      lowered <- setRowsLow rows
      case lowered of
        Left err -> printCleanupError err
        Right () -> pure ()

    printCleanupError :: GpioError -> IO ()
    printCleanupError err =
      putStrLn $ "[kvmSwitch] ERROR: resetting keyboard rows failed: " <> show err

setupLines :: Chip -> Config -> IOEither GpioError ([PinPtr], [PinPtr])
setupLines chip (Config _ rowPins columnPins) = E.do
  rows <- getPinPtrsE chip rowPins
  columns <- getPinPtrsE chip columnPins
  _ <- configureRows rows
  _ <- configureColumns columns
  E.ok (rows, columns)
  where
    getPinPtrsE :: Chip -> [Pin] -> IOEither GpioError [PinPtr]
    getPinPtrsE c pins = sequence <$> mapM (getLineE c) pins

    configureRows :: [PinPtr] -> IOEither GpioError ()
    configureRows = traverseE_ $ \row ->
      requestOutputE ("row GPIO" <> show (pinToBCM' row.pin)) False row.ptr

    configureColumns :: [PinPtr] -> IOEither GpioError ()
    configureColumns = traverseE_ $ \column ->
      requestInputE ("column GPIO" <> show (pinToBCM' column.pin))
        biasPullDown column.ptr

setRowsLow :: [PinPtr] -> IOEither GpioError ()
setRowsLow = traverseE_ (\row -> setValueE row.ptr False)
