{-# LANGUAGE QualifiedDo #-}
module Scan where

import           Config
import           Control.Concurrent       (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Exception        (bracket_)
import qualified Control.Monad            as M
import           Data.Map.Strict          (notMember)
import           Data.Maybe               (catMaybes, listToMaybe)
import           EitherDo.Edo             (IOEither)
import qualified EitherDo.Edo             as E
import           GPIO.Error               (GpioError)
import           GPIO.FFI                 (c_gpiod_chip_close)
import           GPIO.Libgpiod            (getValueE, setValueE)
import           GPIO.PinMap              (pinMap)
import           GPIO.Types               (Chip, Key (..), PinPtr (..))
import           System.Clock             (Clock(Monotonic), getTime, toNanoSecs)
import           System.Directory         (getTemporaryDirectory)
import           System.FilePath          ((</>))
import           Data.Time.Clock          (getCurrentTime)
import           Data.Time.Format         (defaultTimeLocale, formatTime)

type Timestamp = Integer

-- TODO: move magic numbers and default values to Constants.hs

nowNS :: IO Timestamp
nowNS = toNanoSecs <$> getTime Monotonic

msToNS :: Int -> Integer
msToNS ms = fromIntegral ms * 1_000_000

usToNS :: Int -> Integer
usToNS us = fromIntegral us * 1_000

isRowOnFor :: PinPtr -> [PinPtr] -> IOEither GpioError (Maybe Key)
isRowOnFor row cols = E.do
  _     <- setValueE (ptr row) True
  _     <- Right <$> threadDelay 100
  bools <- E.traverseE (getValueE . ptr) cols
  _     <- setValueE (ptr row) False
  E.ok $ listToMaybe
          [ Key (pin row) (pin col)
          | (col, pressed) <- zip cols bools
          , pressed
          ]

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
scanLoopSTM chip outs ins q scanPeriodUS =
  bracket_ (pure ()) (c_gpiod_chip_close chip) $
    M.forever $ E.do
      _  <- Right <$> threadDelay scanPeriodUS
      mk <- scanOnce outs ins
      Right <$> do t <- nowNS
                   atomically $ writeTBQueue q (mk, t)

debounceLoop
  :: TBQueue (Maybe Key, Timestamp)
  -> TMVar Key
  -> Integer
  -> IO ()
debounceLoop q outVar thresholdNS = do
  stableVar <- newTVarIO (Nothing :: Maybe Key)
  candVar <- newTVarIO (Nothing :: Maybe (Maybe Key, Timestamp))
  M.forever $ do
    (samp, t) <- atomically $ readTBQueue q
    mEmit <- atomically $ do
      stable <- readTVar stableVar
      cand <- readTVar candVar
      if samp == stable 
      then do writeTVar candVar Nothing
              pure Nothing
      else case cand of 
        Nothing -> do 
          writeTVar candVar (Just (samp, t))
          pure Nothing
        Just (candState, t0)
          | samp /= candState -> do 
              writeTVar stableVar samp 
              writeTVar candVar Nothing 
              case samp of 
                Just k -> pure (Just k)
                Nothing -> pure Nothing 
          | t - t0 < thresholdNS -> pure Nothing  
          | otherwise -> do
              writeTVar stableVar samp
              writeTVar candVar   Nothing
              case samp of
                Just k  -> pure (Just k)
                Nothing -> pure Nothing
    case mEmit of 
      Just k -> do 
        _ <- atomically $ tryPutTMVar outVar k
        pure ()
      Nothing -> pure ()

mkGpioLogger :: Maybe FilePath -> IO (GpioError -> IO ())
mkGpioLogger mPath = do
  path <- case mPath of
            Just p  -> pure p
            Nothing -> do
              tmp <- getTemporaryDirectory
              pure (tmp </> "kvmSwitch-gpio-errors.log")
  pure $ \e -> do
    ts <- getCurrentTime
    let stamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" ts
    appendFile path (stamp <> "  GPIO error: " <> show e <> "\n")

restartForeverLogged :: (GpioError -> IO ())
                     -> IOEither GpioError ()
                     -> IO ()
restartForeverLogged logE act = do
  r <- act
  case r of
    Right _ -> restartForeverLogged logE act
    Left  e -> do
      logE e
      threadDelay 100_000  -- 100ms backoff
      restartForeverLogged logE act

startScanner
  :: Chip
  -> [PinPtr]
  -> [PinPtr]
  -> Int   -- scan (us)
  -> Int   -- debounce (ms)
  -> IOEither GpioError (TMVar Key)
startScanner chip rows cols scanUS debounceMS = E.do
  q        <- Right <$> newTBQueueIO 128
  outVar   <- Right <$> newEmptyTMVarIO
  logger   <- Right <$> mkGpioLogger Nothing
  let thresholdNS = msToNS debounceMS
  _ <- fmap Right . forkIO  
                  . restartForeverLogged logger 
                  $ scanLoopSTM chip rows cols q scanUS
  _ <- fmap Right . forkIO 
                  $ debounceLoop q outVar thresholdNS
  E.ok outVar

validatePinMap :: Config -> IO ()
validatePinMap (Config rs ws) = do
  let missing = filter (`notMember` pinMap) (rs<>ws)
  if null missing
  then putStrLn "✅ All pins mapped correctly"
  else error $ "❌ Missing mappings for: " <> show missing

