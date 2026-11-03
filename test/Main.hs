module Main (main) where

import Data.Word (Word32)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.Ptr (Ptr, nullPtr)
import qualified Config as KeyboardConfig
import GPIO.PinMap (pinToBCM)
import GPIO.Types (Key(..), Pin(..))
import Keyboard.LED
import NeoPixel
import qualified Scan

data CStrip

foreign import ccall safe "kvm_led_render"
  cRenderNull :: Ptr CStrip -> Ptr Word32 -> CSize -> IO CInt

foreign import ccall safe "kvm_led_show"
  cShowNull :: Ptr CStrip -> IO CInt

foreign import ccall safe "kvm_led_set_pixel"
  cSetPixelNull :: Ptr CStrip -> CSize -> Word32 -> IO CInt

main :: IO ()
main = do
  configResult <- KeyboardConfig.loadConfig "config/keyboard.yaml"
  assert "repository keyboard YAML loads"
    (configResult == Right (KeyboardConfig.Config "gpiochip0"
      [P4, P27, P9, P5, P13]
      [P17, P22, P11, P6, P19, P26, P14, P15]))
  assert "P27 maps to BCM 27" (pinToBCM P27 == Just 27)
  assert "explicit config path wins"
    (KeyboardConfig.configPath (Just "/tmp/keyboard.yaml") (Just "/xdg") "/home/user"
      == "/tmp/keyboard.yaml")
  assert "XDG config path is used"
    (KeyboardConfig.configPath Nothing (Just "/xdg") "/home/user"
      == "/xdg/kvm/keyboard.yaml")
  assert "default config path uses .config"
    (KeyboardConfig.configPath Nothing Nothing "/home/user"
      == "/home/user/.config/kvm/keyboard.yaml")
  assert "invalid keyboard configuration is rejected"
    (case KeyboardConfig.validateConfig (KeyboardConfig.Config "gpiochip0" [] []) of
       Left _ -> True
       Right () -> False)
  let key = Key P4 P17
      otherKey = Key P27 P22
      first = Scan.debounceStep (Scan.msToNS 20)
        Scan.initialDebounceState (Just key, 0)
      heldBefore = Scan.debounceStep (Scan.msToNS 20) (fst first) (Just key, 10)
      held = Scan.debounceStep (Scan.msToNS 20) (fst heldBefore) (Just key, 20_000_000)
      changedCandidate = Scan.debounceStep (Scan.msToNS 20) (fst first) (Just otherKey, 1)
      changedAccepted = Scan.debounceStep (Scan.msToNS 20) (fst changedCandidate)
        (Just otherKey, 20_000_001)
      releaseStart = Scan.debounceStep (Scan.msToNS 20) (fst held)
        (Nothing, 20_000_002)
      releaseAccepted = Scan.debounceStep (Scan.msToNS 20) (fst releaseStart)
        (Nothing, 40_000_003)
      pressAgainStart = Scan.debounceStep (Scan.msToNS 20) (fst releaseAccepted)
        (Just key, 40_000_004)
      pressAgain = Scan.debounceStep (Scan.msToNS 20) (fst pressAgainStart)
        (Just key, 60_000_005)
  assert "first debounce sample emits nothing" (snd first == Nothing)
  assert "changed debounce candidate does not emit immediately"
    (snd changedCandidate == Nothing)
  assert "stable press emits once after debounce" (snd held == Just key)
  assert "held key does not repeat" (snd (Scan.debounceStep (Scan.msToNS 20)
    (fst held) (Just key, 30_000_000)) == Nothing)
  assert "changed candidate eventually emits its new key" (snd changedAccepted == Just otherKey)
  assert "release emits no key event" (snd releaseAccepted == Nothing)
  assert "press after release emits a new event" (snd pressAgain == Just key)
  assert "RGB packing" (packRGB (RGB 255 0 0) == 0x00ff0000)
  assert "RGB order does not swap Haskell bytes"
    (packRGB (RGB 1 2 3) == 0x00010203)
  assert "Colour conversion is RGB" (toRGB Red == RGB 255 0 0)
  assert "LED1 maps to native index 0" (ledIndex LED1 == 0)
  assert "LED36 maps to native index 35" (ledIndex LED36 == 35)
  assert "all application LEDs map to a 36-pixel frame"
    (length applicationLEDs == 36 && map ledIndex applicationLEDs == [0 .. 35])
  assert "application all-off is zero-valued"
    (all ((== RGB 0 0 0) . toRGB) applicationColours)
  assert "zero count rejected" (isInvalid (validateConfig config { count = 0 }))
  assert "negative count rejected" (isInvalid (validateConfig config { count = -1 }))
  assert "invalid GPIO rejected" (isInvalid (validateConfig config { gpio = 11 }))
  assert "pixel index -1 rejected" (isIndexInvalid (validatePixelIndex 36 (-1)))
  assert "pixel index 0 accepted" (validatePixelIndex 36 0 == Right ())
  assert "pixel index 35 accepted" (validatePixelIndex 36 35 == Right ())
  assert "pixel index 36 rejected" (isIndexInvalid (validatePixelIndex 36 36))
  assert "short frame rejected" =<< frameLength 35
  assert "long frame rejected" =<< frameLength 37
  assert "exact frame accepted" =<< frameLength 36
  assert "clear produces 36 zero pixels"
    (length (zeroFrame 36) == 36 && all (== RGB 0 0 0) (zeroFrame 36))
  assert "single pixel update preserves other pixels"
    (singlePixelUpdatePreservesOthers)
  assert "invalid update does not produce a frame"
    (case updateFramePixel 36 36 (RGB 1 2 3) originalFrame of
       Left (InvalidPixelIndex 36 36) -> True
       _ -> False)
  assert "brightness zero accepted" (validateBrightness 0 == Right ())
  assert "brightness 255 accepted" (validateBrightness 255 == Right ())
  assert "native error code is preserved"
    (case NativeDriverError (-100) "Invalid LED strip handle" of
       NativeDriverError code _ -> code == -100
       _ -> False)
  nullResult <- cRenderNull nullPtr nullPtr 0
  assert "null native handle rejected" (nullResult == -100)
  assert "null show handle rejected" =<< (== -100) <$> cShowNull nullPtr
  assert "null set-pixel handle rejected"
    =<< (== -100) <$> cSetPixelNull nullPtr 0 0

  where
    config = StripConfig 36 10 255 OrderGRB
    applicationLEDs =
      [ LED1, LED2, LED3, LED4, LED5, LED6, LED7, LED8, LED9
      , LED10, LED11, LED12, LED13, LED14, LED15, LED16, LED17, LED18
      , LED19, LED20, LED21, LED22, LED23, LED24, LED25, LED26, LED27
      , LED28, LED29, LED30, LED31, LED32, LED33, LED34, LED35, LED36
      ]
    applicationColours = replicate 36 Off
    originalFrame = [RGB (fromIntegral index) 0 0 | index <- [0 .. (35 :: Int)]]
    singlePixelUpdatePreservesOthers =
      case updateFramePixel 36 7 (RGB 255 1 2) originalFrame of
        Right updated ->
          length updated == 36
            && case drop 7 updated of
                 (pixel : _) -> pixel == RGB 255 1 2
                 _ -> False
            && and [actual == expected
                   | (index, (actual, expected)) <-
                       zip [0 .. (35 :: Int)] (zip updated originalFrame)
                   , index /= 7]
        Left _ -> False
    frameLength n = do
      let result = validateFrameLength 36 (replicate n (RGB 0 0 0))
      pure (case (n, result) of
        (36, Right ()) -> True
        (_, Left (FrameLengthMismatch 36 actual)) -> actual == n
        _ -> False)
    isInvalid (Left (InvalidConfiguration _)) = True
    isInvalid _ = False
    isIndexInvalid (Left (InvalidPixelIndex _ _)) = True
    isIndexInvalid _ = False
    assert label condition = if condition then pure () else error (label ++ " failed")
