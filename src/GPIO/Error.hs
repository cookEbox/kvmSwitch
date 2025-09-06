module GPIO.Error where

import           Control.Exception (Exception)
import           Foreign.C         (CInt (..))

data GpioError = NullPtrErr String
               | StatusErr String CInt
                 deriving (Show)
instance Exception GpioError

toEitherUnit :: String -> CInt -> Either GpioError ()
toEitherUnit what rc
  | rc == 0   = Right ()
  | otherwise = Left (StatusErr (what <> " failed (rc=" <> show rc <> ")") rc)

