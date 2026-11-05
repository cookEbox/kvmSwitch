{-# LANGUAGE LambdaCase #-}

module NeoPixel
  ( RGB(..)
  , PixelOrder(..)
  , StripConfig(..)
  , LedError(..)
  , PixelStrip
  , packRGB
  , validateConfig
  , validateBrightness
  , validatePixelIndex
  , validateFrameLength
  , zeroFrame
  , updateFramePixel
  , withPixelStrip
  , setPixel
  , showPixels
  , renderFrame
  , clearPixels
  , setBrightness
  , pixelCount
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, withMVar)
import Control.Exception (bracket, mask)
import Foreign.C.String (peekCString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import Data.Word (Word8)

import NeoPixel.Error (LedError(..))
import NeoPixel.FFI
import NeoPixel.Types

data PixelStrip = PixelStrip (MVar (Maybe (Ptr CStrip, Int))) Int

validateConfig :: StripConfig -> Either LedError ()
validateConfig config
  | config.count <= 0 = Left (InvalidConfiguration "pixel count must be positive")
  | config.count > fromIntegral (maxBound :: CInt) =
      Left (InvalidConfiguration "pixel count exceeds native integer range")
  | config.gpio `notElem` [10, 12, 13, 18, 19, 21, 31] =
      Left (InvalidConfiguration ("unsupported native GPIO: " ++ show config.gpio))
  | otherwise = Right ()

validateBrightness :: Word8 -> Either LedError ()
validateBrightness _ = Right ()

withPixelStrip
  :: StripConfig
  -> (PixelStrip -> IO (Either LedError a))
  -> IO (Either LedError a)
withPixelStrip config action =
  case validateConfig config of
    Left errorValue -> pure (Left errorValue)
    Right () -> mask $ \restore -> do
      opened <- open config
      case opened of
        Left errorValue -> pure (Left errorValue)
        Right acquired -> bracket (pure acquired) closeStrip (restore . action)
  where
    open cfg = alloca $ \out -> do
      poke out nullPtr
      result <- cLedOpen (fromIntegral cfg.gpio) (fromIntegral cfg.count)
        (fromIntegral cfg.brightness) (stripType cfg.order) out
      if result == 0
        then do
          ptr <- peek out
          if ptr == nullPtr
            then pure (Left InvalidNativeHandle)
            else do
              lock <- newMVar (Just (ptr, cfg.count))
              pure (Right (PixelStrip lock cfg.count))
        else Left <$> nativeError result

    closeStrip (PixelStrip lock _) = modifyMVar_ lock $ \state -> do
      case state of
        Nothing -> pure Nothing
        Just (ptr, _) -> cLedClose ptr >> pure Nothing

stripType :: PixelOrder -> CInt
stripType OrderRGB = 0x00100800
stripType OrderGRB = 0x00081000

pixelCount :: PixelStrip -> Int
pixelCount (PixelStrip _ configuredCount) = configuredCount

validatePixelIndex :: Int -> Int -> Either LedError ()
validatePixelIndex expected index
  | index < 0 || index >= expected =
      Left (InvalidPixelIndex index expected)
  | otherwise = Right ()

zeroFrame :: Int -> [RGB]
zeroFrame count = replicate count (RGB 0 0 0)

updateFramePixel :: Int -> Int -> RGB -> [RGB] -> Either LedError [RGB]
updateFramePixel count index colour pixels = do
  validateFrameLength count pixels
  validatePixelIndex count index
  pure
    [ if current == index then colour else old
    | (current, old) <- zip [0 .. count - 1] pixels
    ]

setPixel :: PixelStrip -> Int -> RGB -> IO (Either LedError ())
setPixel (PixelStrip lock expected) index colour = withMVar lock $ \state ->
  case validatePixelIndex expected index of
    Left errorValue -> pure (Left errorValue)
    Right () -> case state of
      Nothing -> pure (Left InvalidNativeHandle)
      Just (ptr, _) -> nativeResult =<< cLedSetPixel ptr (fromIntegral index) (packRGB colour)

showPixels :: PixelStrip -> IO (Either LedError ())
showPixels (PixelStrip lock _) = withMVar lock $ \case
    Nothing -> pure (Left InvalidNativeHandle)
    Just (ptr, _) -> nativeResult =<< cLedShow ptr

renderFrame :: PixelStrip -> [RGB] -> IO (Either LedError ())
renderFrame (PixelStrip lock expected) pixels = withMVar lock $ \case
    Nothing -> pure (Left InvalidNativeHandle)
    Just (ptr, _) ->
      case validateFrameLength expected pixels of
        Left errorValue -> pure (Left errorValue)
        Right () -> withArray (map packRGB pixels) $ \buffer -> do
          result <- cLedRender ptr buffer (fromIntegral expected)
          nativeResult result

clearPixels :: PixelStrip -> IO (Either LedError ())
clearPixels (PixelStrip lock _) = withMVar lock $ \case
    Nothing -> pure (Left InvalidNativeHandle)
    Just (ptr, _) -> nativeResult =<< cLedClear ptr

setBrightness :: PixelStrip -> Word8 -> IO (Either LedError ())
setBrightness (PixelStrip lock _) brightnessValue =
  case validateBrightness brightnessValue of
    Left errorValue -> pure (Left errorValue)
    Right () -> withMVar lock $ \case
        Nothing -> pure (Left InvalidNativeHandle)
        Just (ptr, _) ->
          nativeResult =<< cLedSetBrightness ptr (fromIntegral brightnessValue)

validateFrameLength :: Int -> [RGB] -> Either LedError ()
validateFrameLength expected pixels
  | actual == expected = Right ()
  | otherwise = Left (FrameLengthMismatch expected actual)
  where
    actual = length pixels

nativeError :: CInt -> IO LedError
nativeError result = do
  description <- peekCString =<< cLedErrorString result
  pure (NativeDriverError (fromIntegral result) description)

nativeResult :: CInt -> IO (Either LedError ())
nativeResult result
  | result == 0 = pure (Right ())
  | otherwise = Left <$> nativeError result
