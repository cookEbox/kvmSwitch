{-# LANGUAGE QualifiedDo #-}

module Main where

import           Data.Yaml          (decodeFileThrow)
import           EitherDo.Edo       (IOEither, ok, traverseE_)
import qualified EitherDo.Edo       as E
import           MyLib
import           System.Directory   (getHomeDirectory)
import           System.FilePath    ((</>))
import System.IO (hSetBuffering, BufferMode(..), stdout, stderr)

setup :: Config -> IOEither GpioError (Chip, [PinPtr], [PinPtr])
setup cfg = E.do
  chip <- openChipE "gpiochip0"
  lnOut <- getPinPtrsE chip (rows cfg)
  lnIn  <- getPinPtrsE chip (columns cfg)
  _ <- traverseE_ outputE lnOut
  _ <- traverseE_ inputE lnIn
  ok (chip, lnOut, lnIn)
  where
    getPinPtrsE c ps = sequence <$> mapM (getLineE c) ps
    outputE          = requestOutputE "rows" False . ptr
    inputE           = requestInputE "columns" biasPullDown . ptr

-- need to use STM to act as a store for last clicked button 
-- and read of that everytime it changes
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
    scanLoop chip outs ins
  case sl of
    Left err      -> putStrLn $ "Error: " <>  show err
    Right Nothing -> pure ()
    Right key     -> putStrLn $ "Key pressed: " <> show key
