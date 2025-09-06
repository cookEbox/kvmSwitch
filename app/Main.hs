{-# LANGUAGE QualifiedDo #-}

module Main where

import           Data.Yaml          (decodeFileThrow)
import           EitherDo.Edo       (IOEither, ok, traverseE_)
import qualified EitherDo.Edo       as E
import           Foreign            ((.|.))
import           MyLib
import           System.Directory   (getHomeDirectory)
import           System.FilePath    ((</>))

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
    inputE           = requestInputE "columns" (biasPullUp .|. activeLow) . ptr

main :: IO ()
main = do
  home <- getHomeDirectory
  config <- decodeFileThrow $ home </> ".config/kvm/keys.yml"
  validatePinMap config
  sl <- E.do
    (chip, outs, ins) <- setup config
    scanLoop chip outs ins
  case sl of
    Left err      -> putStrLn $ "Error: " <>  show err
    Right Nothing -> pure ()
    Right key     -> putStrLn $ "Key pressed: " <> show key
