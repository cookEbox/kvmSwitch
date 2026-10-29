module NeoPixel.Error
  ( LedError(..)
  ) where

data LedError
  = InvalidConfiguration String
  | InvalidPixelIndex { pixelIndex :: !Int, pixelCountForError :: !Int }
  | FrameLengthMismatch { expectedPixels :: !Int, actualPixels :: !Int }
  | InvalidNativeHandle
  | NativeDriverError { nativeErrorCode :: !Int, nativeErrorDescription :: String }
  deriving (Eq, Show)
