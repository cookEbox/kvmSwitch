{-# LANGUAGE QualifiedDo #-}

module Main where

import           Control.Concurrent.STM (atomically, takeTMVar)
import           Control.Monad          (forever)
import           Data.Yaml              (decodeFileThrow)
import           EitherDo.Edo           (IOEither, ok, traverseE_)
import qualified EitherDo.Edo           as E
import           MyLib
import           System.Directory       (getHomeDirectory)
import           System.FilePath        ((</>))
import           System.IO              (BufferMode (..), hSetBuffering, stderr,
                                         stdout)
import Control.Exception (bracket)
import OneWire.LedSpi (openSPI, rgb, renderSK6812, off, closeSPI)
import Data.Word (Word32)

setup :: Config -> IOEither GpioError (Chip, [PinPtr], [PinPtr])
setup cfg = E.do
  chip <- openChipE (chip cfg)
  lnOut <- getPinPtrsE chip (rows cfg)
  lnIn  <- getPinPtrsE chip (columns cfg)
  _ <- traverseE_ outputE lnOut
  _ <- traverseE_ inputE lnIn
  ok (chip, lnOut, lnIn)
  where
    getPinPtrsE c ps = sequence <$> mapM (getLineE c) ps
    outputE          = requestOutputE "rows" False . ptr
    inputE           = requestInputE "columns" biasPullDown . ptr

led :: [Word32] -> IO () 
led colour = bracket (openSPI "/dev/spidev0.0" 2_400_000) closeSPI $ \spi -> do 
  let n = 1
      grb = True 
      s0 = replicate n off
      s1 = colour
  renderSK6812 spi grb s1

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
        Key P24 P18 -> led [rgb 255 0 0]
        Key P24 P23 -> led [rgb 0 0 0]
        _           -> putStrLn "Not a key"
