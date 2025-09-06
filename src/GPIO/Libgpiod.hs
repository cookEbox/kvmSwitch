module GPIO.Libgpiod where

import           EitherDo.Edo      (IOEither)
import           Foreign           (Ptr, fromBool, nullPtr)
import           Foreign.C         (CInt (..), withCString)
import           GPIO.Error        (GpioError (..), toEitherUnit)
import           GPIO.FFI
import           GPIO.PinMap       (pinToBCM')
import           GPIO.Types

expectNonNull :: String -> IO (Ptr a) -> IOEither GpioError (Ptr a)
expectNonNull what act = do
  p <- act
  pure $ if p == nullPtr then Left (NullPtrErr what) else Right p

expectOk :: String -> IO CInt -> IO (Either GpioError ())
expectOk what act = do
  rc <- act
  pure $ if rc == 0 then Right () else Left (StatusErr what rc)

openChipE :: String -> IO (Either GpioError Chip)
openChipE name =
  withCString name $ \cname ->
    expectNonNull "gpiod_chip_open_by_name" (c_gpiod_chip_open_by_name cname)

getLineE :: Ptr CGpioChip -> Pin -> IOEither GpioError PinPtr
getLineE chip off = do
  ptrE <- expectNonNull "gpiod_chip_get_line"
          $ c_gpiod_chip_get_line chip
          $ pinToBCM' off
  pure $ PinPtr off <$> ptrE

requestOutputE :: String -> Bool -> Ptr CGpioLine -> IOEither GpioError ()
requestOutputE who initial line =
  withCString who $ \cw ->
    expectOk "gpiod_line_request_output"
      (c_gpiod_line_request_output line cw (if initial then 1 else 0))

getValueE :: Ptr CGpioLine -> IOEither GpioError Bool
getValueE line = do
  rc <- c_gpiod_line_get_value line
  pure $ case compare rc 0 of
    LT -> Left (StatusErr "gpiod_line_get_value" rc)
    EQ -> Right False
    GT -> Right True

setValueE :: Ptr CGpioLine -> Bool -> IO (Either GpioError ())
setValueE line b = do
  rc <- c_gpiod_line_set_value line (fromBool b)
  pure (toEitherUnit "gpiod_line_set_value" rc)

requestInputE :: String -> CInt -> Ptr CGpioLine -> IOEither GpioError ()
requestInputE consumer flags line =
  withCString consumer $ \cstr -> do
    rc <- c_gpiod_line_request_input_flags line cstr flags
    pure (toEitherUnit "gpiod_line_request_input_flags" rc)

