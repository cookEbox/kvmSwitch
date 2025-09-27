{-# LANGUAGE ForeignFunctionInterface #-}
module OneWire.LedSpi
  ( Spi(..)
  , openSPI
  , closeSPI
  , renderSK6812
  , rgb, off
  ) where

import           Control.Exception (bracketOnError)
import           Control.Monad     (when)
import           Data.Bits
import           Data.Word
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Internal   as BSI
import           Foreign
import           Foreign.C.Types
import           Foreign.C.Error            (throwErrnoIfMinus1_)
import           System.Posix.IO            (OpenMode(..), defaultFileFlags, openFd, closeFd, fdWrite)
import           System.Posix.Types         (Fd(..))
import           Control.Concurrent         (threadDelay)

-- Minimal ioctl bindings
foreign import ccall unsafe "ioctl"
  c_ioctl :: CInt -> CULong -> Ptr a -> IO CInt

-- These constants are stable across kernels:
-- SPI_IOC_WR_MODE (u8), SPI_IOC_WR_BITS_PER_WORD (u8), SPI_IOC_WR_MAX_SPEED_HZ (u32)
spi_IOC_WR_MODE         :: CULong; spi_IOC_WR_MODE         = 0x40016b01
spi_IOC_WR_BITS_PERWORD :: CULong; spi_IOC_WR_BITS_PERWORD = 0x40016b03
spi_IOC_WR_MAX_SPEED    :: CULong; spi_IOC_WR_MAX_SPEED    = 0x40046b04

newtype Spi = Spi { spiFd :: Fd }

-- Public helpers
rgb :: Word8 -> Word8 -> Word8 -> Word32
rgb r g b =  (fromIntegral r `shiftL` 16)
          .|.(fromIntegral g `shiftL` 8)
          .|. fromIntegral b
off :: Word32
off = 0

openSPI :: FilePath -> Int -> IO Spi
openSPI dev hz = bracketOnError
  (openFd dev WriteOnly defaultFileFlags)
  (\fd -> closeFd fd >> ioError (userError "openSPI failed"))
  (\fd -> do
      -- mode 0
      alloca $ \(p :: Ptr Word8) -> do
        poke (castPtr p :: Ptr Word8) 0
        throwErrnoIfMinus1_ "ioctl(SPI_IOC_WR_MODE)" (c_ioctl (fromIntegral (fdToInt fd)) spi_IOC_WR_MODE p)
      -- 8 bits per word
      alloca $ \(p :: Ptr Word8) -> do
        poke (castPtr p :: Ptr Word8) 8
        throwErrnoIfMinus1_ "ioctl(SPI_IOC_WR_BITS_PER_WORD)" (c_ioctl (fromIntegral (fdToInt fd)) spi_IOC_WR_BITS_PERWORD p)
      -- speed
      alloca $ \(p :: Ptr Word32) -> do
        poke (castPtr p :: Ptr Word32) (fromIntegral hz)
        throwErrnoIfMinus1_ "ioctl(SPI_IOC_WR_MAX_SPEED_HZ)" (c_ioctl (fromIntegral (fdToInt fd)) spi_IOC_WR_MAX_SPEED p)
      pure (Spi fd)
  )
  where fdToInt (Fd i) = i

closeSPI :: Spi -> IO ()
closeSPI = closeFd . spiFd

-- Render a full frame to SK6812/WS2812 using SPI bit-expansion.
-- rgbOrderGRB=True for most SK6812/WS2812; False if your device expects RGB order.
renderSK6812 :: Spi -> Bool -> [Word32] -> IO ()
renderSK6812 (Spi fd) rgbOrderGRB pixels = do
  let triples = concatMap (encodePixel rgbOrderGRB) pixels         -- [3-bit symbols] as bits
      payload = pack3Bits triples                                  -- strict ByteString
  _ <- fdWrite fd (toString payload)                                -- write() is enough
  threadDelay 200   -- >=80–100 µs reset/latch; we give 200 µs
  where
    toString :: BS.ByteString -> String
    toString = map (toEnum . fromIntegral) . BS.unpack

-- Encode one pixel (24 bits) into 24*3 = 72 "SPI bits", where each input bit -> 3 output bits.
-- For '0' we emit 100, for '1' we emit 110. We emit bits MSB-first per colour byte.
encodePixel :: Bool -> Word32 -> [Bool]
encodePixel grb w =
  let (r,g,b) = ((w `shiftR` 16) .&. 0xff
                ,(w `shiftR`  8) .&. 0xff
                , w             .&. 0xff)
      order = if grb then [g,r,b] else [r,g,b]
  in concatMap encodeByte order

encodeByte :: Word32 -> [Bool]
encodeByte byte =
  concatMap step [7,6..0]
  where
    step i = if testBit byte i then one else zero
    zero = [True,False,False]  -- 1 0 0
    one  = [True,True, False]  -- 1 1 0

-- Pack a stream of booleans (each one SPI bit) into bytes, MSB-first per byte.
pack3Bits :: [Bool] -> BS.ByteString
pack3Bits bits = BSI.unsafeCreate outBytes $ \ptr -> do
  let go _ _ _ [] = pure ()
      go !byte !nb !ix (b:bs) = do
        let byte' = (byte `shiftL` 1) .|. (if b then 1 else 0)
            nb'   = nb + 1
        if nb' == 8
          then pokeElemOff ptr ix (fromIntegral byte') >> go 0 0 (ix+1) bs
          else go byte' nb' ix bs
  go (0::Word8) (0 :: Integer) 0 bits
  -- pad final partial byte with zeros if needed
  when (remBits /= 0) $
    pokeElemOff ptr (outBytes - 1) (fromIntegral (padByte (drop (lenBits - remBits) bits)))
  where
    lenBits  = length bits
    outBytes = (lenBits + 7) `div` 8
    remBits  = lenBits `mod` 8
    padByte  = foldl (\acc b -> (acc `shiftL` 1) .|. (if b then 1 else 0)) (0::Word8)

