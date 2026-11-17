{-# LANGUAGE CApiFFI #-}

module Kronor.SmartCsv.Deflate
  ( SegmentEnd (..),
    crc32,
    deflateSegment,
  )
where

import Data.ByteString.Lazy qualified as LByteString
import Data.ByteString.Unsafe qualified as ByteString.Unsafe
import Data.Streaming.Zlib (Popper, PopperRes (..), WindowBits (..))
import Data.Streaming.Zlib qualified as Zlib
import Foreign.C.Types (CUInt (..), CULong (..))
import Foreign.Ptr (Ptr, castPtr)
import RIO

foreign import capi unsafe "zlib.h crc32"
  c_crc32 :: CULong -> Ptr Word8 -> CUInt -> IO CULong

-- | How a segment ends.  'KeepOpen' ends it with a full flush: the bytes stop on a
-- byte boundary and nothing after them refers back into them, so the next segment
-- can come from a fresh compressor and the two still read as one deflate stream.
-- 'Final' closes the stream; only the last segment of an entry may use it.
data SegmentEnd = KeepOpen | Final
  deriving stock (Eq, Show)

-- | Compress bytes into one raw deflate segment (see 'SegmentEnd').
deflateSegment :: Int -> SegmentEnd -> LByteString -> IO LByteString
deflateSegment level segmentEnd input = do
  -- Negative window bits: raw deflate, as a ZIP entry stores it.
  deflate <- Zlib.initDeflate level (WindowBits (-15))
  revCompressed <- foldM (\revOutput chunk -> Zlib.feedDeflate deflate chunk >>= drain revOutput) [] (LByteString.toChunks input)
  revSegment <-
    drain revCompressed $ case segmentEnd of
      KeepOpen -> Zlib.fullFlushDeflate deflate
      Final -> Zlib.finishDeflate deflate
  pure (LByteString.fromChunks (reverse revSegment))
  where
    -- Every popper has to be emptied before the compressor takes more input.
    drain :: [ByteString] -> Popper -> IO [ByteString]
    drain revOutput popper =
      popper >>= \case
        PRDone -> pure revOutput
        PRNext chunk -> drain (chunk : revOutput) popper
        PRError err -> throwIO err

-- | Update a CRC-32 (the ZIP checksum) with more bytes.
crc32 :: Word32 -> LByteString -> IO Word32
crc32 initial input = foldM update initial (LByteString.toChunks input)
  where
    update crc chunk =
      ByteString.Unsafe.unsafeUseAsCStringLen chunk \(chunkPtr, chunkLength) ->
        fromIntegral <$> c_crc32 (fromIntegral crc) (castPtr chunkPtr) (fromIntegral chunkLength)
