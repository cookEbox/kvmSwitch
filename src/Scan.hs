{-# LANGUAGE QualifiedDo #-}
module Scan where

import           Config
import           Control.Concurrent (threadDelay)
import           Control.Exception  (bracket_, finally)
import qualified Control.Monad      as M
import           Data.Map.Strict    (notMember)
import           Data.Maybe         (catMaybes, listToMaybe)
import           EitherDo.Edo       (IOEither)
import qualified EitherDo.Edo       as E
import           GPIO.Error         (GpioError)
import           GPIO.FFI           (c_gpiod_chip_close)
import           GPIO.Libgpiod      (getValueE, setValueE)
import           GPIO.PinMap        (pinMap)
import           GPIO.Types         (Chip, Key (..), PinPtr (..))

isRowOnFor :: PinPtr -> [PinPtr] -> IOEither GpioError (Maybe Key)
isRowOnFor row cols = do
  _ <- powerUp row
  finally (action cols) (powerDwn row)
  where
    powerUp r = setValueE (ptr r) True
    powerDwn r = M.void (setValueE (ptr r) False)
    action c = do
      boolsE <- sequence <$> mapM (getValueE . ptr) c
      pure (mkKey <$> boolsE)
    mkKey bs = listToMaybe [ Key (pin row) (pin col)
                                     | (col, b) <- zip cols bs
                                     , b
                                     ]

scanOnce
  :: [PinPtr]
  -> [PinPtr]
  -> IOEither GpioError (Maybe Key)
scanOnce rws cols = do
  results <- mapM (`isRowOnFor` cols) rws
  pure $ do
    keys <- sequence results
    let found = catMaybes keys
    case found of
      [k] -> Right (Just k)
      _   -> Right Nothing

scanLoop :: Chip -> [PinPtr] -> [PinPtr] -> IOEither GpioError (Maybe Key)
scanLoop chip outs ins =
  bracket_ (pure ()) (c_gpiod_chip_close chip) $ do
  M.forever $ E.do
    _ <- Right <$> threadDelay 3000
    scanOnce outs ins

validatePinMap :: Config -> IO ()
validatePinMap (Config rs ws) = do
  let missing = filter (`notMember` pinMap) (rs<>ws)
  if null missing
  then putStrLn "✅ All pins mapped correctly"
  else error $ "❌ Missing mappings for: " <> show missing

