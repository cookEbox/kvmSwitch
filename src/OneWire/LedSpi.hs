{-# LANGUAGE ForeignFunctionInterface #-}
module OneWire.LedSpi
  ( Spi(..)
  , openSPI
  , closeSPI
  , renderSK6812
  , rgb, off
  ) where

import           Control.Concurrent         (threadDelay)
import           Control.Exception          (bracketOnError)
import           Control.Monad              (when)
import           Data.Bits
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Internal   as BSI
import           Data.Word
import           Foreign
import           Foreign.C.Error            (throwErrnoIfMinus1_)
import           Foreign.C.Types
import           System.Posix.IO            (OpenMode (..), closeFd,
                                             defaultFileFlags, openFd)
import           System.Posix.IO.ByteString (fdWrite)
import           System.Posix.Types         (Fd (..))

foreign import ccall unsafe "ioctl"
  c_ioctl :: CInt -> CULong -> Ptr a -> IO CInt

spi_IOC_WR_MODE         :: CULong; spi_IOC_WR_MODE         = 0x40016b01
spi_IOC_WR_BITS_PERWORD :: CULong; spi_IOC_WR_BITS_PERWORD = 0x40016b03
spi_IOC_WR_MAX_SPEED    :: CULong; spi_IOC_WR_MAX_SPEED    = 0x40046b04

newtype Spi = Spi { spiFd :: Fd }

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
      alloca $ \(p :: Ptr Word8) -> do
        poke (castPtr p :: Ptr Word8) 0
        throwErrnoIfMinus1_ "ioctl(SPI_IOC_WR_MODE)" (c_ioctl (fromIntegral (fdToInt fd)) spi_IOC_WR_MODE p)
      alloca $ \(p :: Ptr Word8) -> do
        poke (castPtr p :: Ptr Word8) 8
        throwErrnoIfMinus1_ "ioctl(SPI_IOC_WR_BITS_PER_WORD)" (c_ioctl (fromIntegral (fdToInt fd)) spi_IOC_WR_BITS_PERWORD p)
      alloca $ \(p :: Ptr Word32) -> do
        poke (castPtr p :: Ptr Word32) (fromIntegral hz)
        throwErrnoIfMinus1_ "ioctl(SPI_IOC_WR_MAX_SPEED_HZ)" (c_ioctl (fromIntegral (fdToInt fd)) spi_IOC_WR_MAX_SPEED p)
      pure (Spi fd)
  )
  where fdToInt (Fd i) = i

closeSPI :: Spi -> IO ()
closeSPI = closeFd . spiFd

renderSK6812 :: Spi -> Bool -> [Word32] -> IO ()
renderSK6812 (Spi fd) rgbOrderGRB pixels = do
  let triples = concatMap (encodePixel rgbOrderGRB) pixels
      payload = pack3Bits triples
  _ <- fdWrite fd payload
  threadDelay 200

encodePixel :: Bool -> Word32 -> [Bool]
encodePixel grb w = concatMap encodeByte order
  where (r,g,b) = ( (w `shiftR` 16) .&. 0xff
                  , (w `shiftR`  8) .&. 0xff
                  ,  w              .&. 0xff
                  )
        order = if grb then [g,r,b] else [r,g,b]

encodeByte :: Word32 -> [Bool]
encodeByte byte =
  concatMap step [7,6..0]
  where
    step i = if testBit byte i then one else zero
    zero = [True,False,False]  -- 1 0 0
    one  = [True,True, False]  -- 1 1 0

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
  when (remBits /= 0) $
    pokeElemOff ptr (outBytes - 1) (fromIntegral (padByte (drop (lenBits - remBits) bits)))
  where
    lenBits  = length bits
    outBytes = (lenBits + 7) `div` 8
    remBits  = lenBits `mod` 8
    padByte  = foldl (\acc b -> (acc `shiftL` 1) .|. (if b then 1 else 0)) (0::Word8)

