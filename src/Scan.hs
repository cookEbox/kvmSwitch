{-# LANGUAGE QualifiedDo #-}
module Scan where

import           Config
import           Control.Concurrent (threadDelay)
import           Control.Exception  (bracket_)
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
isRowOnFor row cols = E.do
  _     <- setValueE (ptr row) True          
  bools <- E.traverseE (getValueE . ptr) cols
  _     <- setValueE (ptr row) False         
  E.ok $ listToMaybe
          [ Key (pin row) (pin col)
          | (col, pressed) <- zip cols bools
          , pressed
          ]

scanOnce
  :: [PinPtr]
  -> [PinPtr]
  -> IOEither GpioError (Maybe Key)
scanOnce rws cols = E.do
  perRow <- E.traverseE (`isRowOnFor` cols) rws
  let found = catMaybes perRow
  E.ok $ case found of
    [k] -> Just k
    _   -> Nothing

scanLoop :: Chip -> [PinPtr] -> [PinPtr] -> IOEither GpioError (Maybe Key)
scanLoop chip outs ins =
  bracket_ (pure ()) (c_gpiod_chip_close chip) $
    M.forever $ E.do
      _ <- Right <$> threadDelay 3000
      mk <- scanOnce outs ins
      case mk of 
        Just k -> Right <$> putStrLn ("Key: " <> show k)
        Nothing -> pure $ Right ()

validatePinMap :: Config -> IO ()
validatePinMap (Config rs ws) = do
  let missing = filter (`notMember` pinMap) (rs<>ws)
  if null missing
  then putStrLn "✅ All pins mapped correctly"
  else error $ "❌ Missing mappings for: " <> show missing

