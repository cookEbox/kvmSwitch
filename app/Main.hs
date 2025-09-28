{-# LANGUAGE QualifiedDo #-}

module Main where

import           Control.Concurrent.STM (TMVar, TQueue, atomically, putTMVar,
                                         takeTMVar, tryTakeTMVar, writeTQueue)
import           Control.Exception      (bracket)
import           Control.Monad          (forever)
import           Data.Map               (Map, findWithDefault, insert)
import           Data.Word              (Word32)
import           Data.Yaml              (decodeFileThrow)
import           EitherDo.Edo           (IOEither, ok, traverseE_)
import qualified EitherDo.Edo           as E
import           MyLib
import           OneWire.LedSpi         (closeSPI, off, openSPI, renderSK6812,
                                         rgb)
import           System.Directory       (getHomeDirectory)
import           System.FilePath        ((</>))
import           System.IO              (BufferMode (..), hSetBuffering, stderr,
                                         stdout)

-- TODO: Move constants to config or donfig directory
-- Temporary for testing led code
type LedMap = Map LED Colour

ledSet :: [Word32] -> IO ()
ledSet colour = bracket (openSPI "/dev/spidev0.0" 3_200_000) closeSPI $ \spi -> do
  let n = 1
      grb = True
      s0 = replicate n off
      s1 = colour
  renderSK6812 spi grb s1

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
  lnOut <- getPinPtrsE chip cfg.rows
  lnIn  <- getPinPtrsE chip cfg.columns
  _ <- traverseE_ outputE lnOut
  _ <- traverseE_ inputE lnIn
  ok (chip, lnOut, lnIn)
  where
    getPinPtrsE c ps = sequence <$> mapM (getLineE c) ps
    outputE          = requestOutputE "rows" False . (.ptr)
    inputE           = requestInputE "columns" biasPullDown . (.ptr)

-- TODO: Move scanTime and debounceTime to a Constants directory
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  putStrLn "[kvmSwitch] starting…"
  home <- getHomeDirectory
  config <- decodeFileThrow $ home </> ".config/kvm/keys.yml"
  validatePinMap config
  print config
  sl <- E.do
    (chip, outs, ins) <- setup config
    startScanner chip outs ins 3000 3
  case sl of
    Left err    -> putStrLn $ "Error: " <> show err
    Right tmvar -> forever $ do
      key <- atomically $ takeTMVar tmvar
      putStrLn $ "Key pressed: " <> show key
      case key of
        Key P24 P18 -> ledSet [rgb 255 0 0]
        Key P24 P23 -> ledSet [rgb 0 0 0]
        _           -> putStrLn "Not a key"
