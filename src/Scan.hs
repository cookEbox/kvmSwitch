{-# LANGUAGE QualifiedDo #-}
module Scan where

import           Config
import           Control.Concurrent       (forkIO, killThread, threadDelay)
import           Control.Concurrent.STM
import qualified Control.Monad            as M
import           Data.Map.Strict          (notMember)
import           Data.Maybe               (catMaybes, listToMaybe)
import           EitherDo.Edo             (IOEither)
import qualified EitherDo.Edo             as E
import           GPIO.Error               (GpioError)
import           GPIO.Libgpiod            (getValueE, setValueE)
import           GPIO.PinMap              (pinMap)
import           GPIO.Types               (Chip, Key (..), PinPtr (..))
import           System.Clock             (Clock(Monotonic), getTime, toNanoSecs)

type Timestamp = Integer

defaultScanPeriodUS :: Int
defaultScanPeriodUS = 1_000

defaultDebounceMS :: Int
defaultDebounceMS = 20

rowSettleUS :: Int
rowSettleUS = 100

nowNS :: IO Timestamp
nowNS = toNanoSecs <$> getTime Monotonic

msToNS :: Int -> Integer
msToNS ms = fromIntegral ms * 1_000_000

usToNS :: Int -> Integer
usToNS us = fromIntegral us * 1_000

isRowOnFor :: PinPtr -> [PinPtr] -> IOEither GpioError (Maybe Key)
isRowOnFor row cols = do
  raised <- setValueE row.ptr True
  case raised of
    Left err -> pure (Left err)
    Right () -> do
      threadDelay rowSettleUS
      sampled <- E.traverseE (getValueE . (.ptr)) cols
      lowered <- setValueE row.ptr False
      case lowered of
        Left err -> pure (Left err)
        Right () -> pure $ case sampled of
          Left err -> Left err
          Right bools -> Right (listToMaybe
            [ Key row.pin col.pin
            | (col, pressed) <- zip cols bools
            , pressed
            ])

scanOnce :: [PinPtr] -> [PinPtr] -> IOEither GpioError (Maybe Key)
scanOnce rws cols = E.do
  perRow <- E.traverseE (`isRowOnFor` cols) rws
  let found = catMaybes perRow
  E.ok $ case found of
    [k] -> Just k
    _   -> Nothing

scanLoopSTM
  :: Chip
  -> [PinPtr]
  -> [PinPtr]
  -> TBQueue (Maybe Key, Timestamp)
  -> Int
  -> IOEither GpioError ()
scanLoopSTM _chip outs ins q scanPeriodUS = M.forever $ E.do
  _  <- Right <$> threadDelay scanPeriodUS
  mk <- scanOnce outs ins
  Right <$> do
    t <- nowNS
    atomically $ writeTBQueue q (mk, t)

data DebounceState = DebounceState
  { stableSample :: Maybe Key
  , candidate    :: Maybe (Maybe Key, Timestamp)
  } deriving (Eq, Show)

initialDebounceState :: DebounceState
initialDebounceState = DebounceState Nothing Nothing

debounceStep
  :: Integer
  -> DebounceState
  -> (Maybe Key, Timestamp)
  -> (DebounceState, Maybe Key)
debounceStep thresholdNS state (sample, timestamp)
  | sample == stable =
      (state { candidate = Nothing }, Nothing)
  | otherwise = case pendingCandidate of
      Nothing -> (state { candidate = Just (sample, timestamp) }, Nothing)
      Just (pending, started)
        | sample /= pending ->
            (state { candidate = Just (sample, timestamp) }, Nothing)
        | timestamp - started < thresholdNS -> (state, Nothing)
        | otherwise ->
            ( DebounceState sample Nothing
            , case sample of
                Just key -> Just key
                Nothing  -> Nothing
            )
  where
    DebounceState stable pendingCandidate = state

debounceLoop
  :: TBQueue (Maybe Key, Timestamp)
  -> TMVar Key
  -> Integer
  -> IO ()
debounceLoop q outVar thresholdNS = do
  stateVar <- newTVarIO initialDebounceState
  M.forever $ do
    sample <- atomically $ readTBQueue q
    mEmit <- atomically $ do
      state <- readTVar stateVar
      let (nextState, emitted) = debounceStep thresholdNS state sample
      writeTVar stateVar nextState
      pure emitted
    case mEmit of
      Just key -> atomically $ do
        _ <- tryPutTMVar outVar key
        pure ()
      Nothing -> pure ()

startScannerWithErrors
  :: Chip
  -> [PinPtr]
  -> [PinPtr]
  -> Int
  -> Int
  -> IOEither GpioError (TMVar Key, TMVar GpioError, IO ())
startScannerWithErrors chip rows cols scanUS debounceMS = do
  q        <- newTBQueueIO 128
  outVar   <- newEmptyTMVarIO
  errorVar <- newEmptyTMVarIO
  scanThread <- forkIO (reportScanError errorVar =<< scanLoopSTM chip rows cols q scanUS)
  debounceThread <- forkIO (debounceLoop q outVar (msToNS debounceMS))
  let stop = killThread scanThread >> killThread debounceThread
  pure (Right (outVar, errorVar, stop))
  where
    reportScanError :: TMVar GpioError -> Either GpioError () -> IO ()
    reportScanError errors result = case result of
      Left err -> atomically $ do
        _ <- tryPutTMVar errors err
        pure ()
      Right () -> pure ()

startScanner
  :: Chip
  -> [PinPtr]
  -> [PinPtr]
  -> Int
  -> Int
  -> IOEither GpioError (TMVar Key)
startScanner chip rows cols scanUS debounceMS = do
  result <- startScannerWithErrors chip rows cols scanUS debounceMS
  pure $ case result of
    Left err -> Left err
    Right (outVar, _errors, _stop) -> Right outVar

validatePinMap :: Config -> IO ()
validatePinMap (Config _ rs ws) = do
  let missing = filter (`notMember` pinMap) (rs <> ws)
  if null missing
  then putStrLn "✅ All pins mapped correctly"
  else error $ "❌ Missing mappings for: " <> show missing
