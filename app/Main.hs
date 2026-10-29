{-# LANGUAGE QualifiedDo #-}

module Main where

import           Control.Concurrent.STM (TMVar, TQueue, atomically, putTMVar,
                                         tryTakeTMVar, writeTQueue)
import           Control.Concurrent     (threadDelay)
import           Control.Exception      (bracket)
import           Data.Map               (Map, findWithDefault, insert)
import           Data.Word              (Word32)
import           EitherDo.Edo           (IOEither, ok, traverseE_)
import qualified EitherDo.Edo           as E
import           MyLib
import           OneWire.LedSpi         (Spi, closeSPI, openSPI, renderSK6812,
                                         rgb)
import           System.IO              (BufferMode (..), hSetBuffering, stderr,
                                         stdout)

-- TODO: Move constants to config or config directory
-- Temporary for testing led code
type LedMap = Map LED Colour

ledSet :: Spi -> [Word32] -> IO ()
ledSet spi colour = do
  let grb = True
  renderSK6812 spi grb colour

showFrame :: Spi -> String -> [Word32] -> IO ()
showFrame spi name frame = do
  putStrLn $ "[kvmSwitch] displaying " <> name
  ledSet spi frame
  threadDelay 5_000_000

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


whiteLed, offLed :: Word32
whiteLed = rgb 255 255 255
offLed   = rgb 0 0 0

testLed1 :: [Word32]
testLed1 = replicate 3 whiteLed <> [offLed] <> replicate 32 whiteLed

testLed2 :: [Word32]
testLed2 = concat (replicate 18 [whiteLed, offLed])

-- TODO: Move scanTime and debounceTime to a Constants directory
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  putStrLn "[kvmSwitch] starting…"
  -- LED-only test: do not start the GPIO keyboard scanner while testing SPI.
  bracket (openSPI "/dev/spidev0.0" 2_400_000) closeSPI $ \spi -> do
    let allOff = replicate 36 (rgb 0 0 0)
        allOn  = replicate 36 (rgb 255 255 255)
    -- Display each frame for five seconds before moving to the next.
    showFrame spi "all on" allOn
    showFrame spi "all but LED 4 on" testLed1
    showFrame spi "alternating LEDs off" testLed2
    showFrame spi "all off" allOff
    showFrame spi "all off (final latch)" allOff
    showFrame spi "all off (Hellooooo)" allOff

    -- Keep the SPI device open long enough to observe the final state.
    threadDelay 60_000_000
