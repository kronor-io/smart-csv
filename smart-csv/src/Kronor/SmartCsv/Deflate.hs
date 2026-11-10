{-# LANGUAGE CApiFFI #-}

module Kronor.SmartCsv.Deflate
  ( SegmentEnd (..),
    crc32,
    deflateSegment,
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LByteString
import Data.ByteString.Unsafe qualified as ByteString.Unsafe
import Foreign.C.Types (CInt (..), CUInt (..), CULong (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peek)
import RIO

data ZStream

foreign import ccall unsafe "smart_csv_deflate_new"
  c_deflateNew :: CInt -> IO (Ptr ZStream)

foreign import ccall unsafe "smart_csv_deflate"
  c_deflate :: Ptr ZStream -> Ptr Word8 -> CUInt -> CInt -> Ptr Word8 -> CUInt -> Ptr CUInt -> Ptr CUInt -> IO CInt

foreign import ccall unsafe "smart_csv_deflate_free"
  c_deflateFree :: Ptr ZStream -> IO ()

foreign import capi unsafe "zlib.h crc32"
  c_crc32 :: CULong -> Ptr Word8 -> CUInt -> IO CULong

-- | How a segment ends.  'KeepOpen' ends it with a full flush: the bytes stop on a
-- byte boundary and nothing after them refers back into them, so the next segment
-- can come from a fresh compressor and the two still read as one deflate stream.
-- 'Final' closes the stream; only the last segment of an entry may use it.
data SegmentEnd = KeepOpen | Final
  deriving stock (Eq, Show)

zNoFlush, zFullFlush, zFinish, zOk, zStreamEnd, zBufError :: CInt
zNoFlush = 0
zFullFlush = 3
zFinish = 4
zOk = 0
zStreamEnd = 1
zBufError = -5

outputBufferBytes :: Int
outputBufferBytes = 65_536

-- | Compress bytes into one raw deflate segment (see 'SegmentEnd').
deflateSegment :: Int -> SegmentEnd -> LByteString -> IO LByteString
deflateSegment level segmentEnd input =
  bracket acquire c_deflateFree \stream ->
    allocaBytes outputBufferBytes \outputBuffer ->
      alloca \inputUsedPtr ->
        alloca \outputUsedPtr -> do
          let -- One deflate call; returns its status, how much input it took and
              -- how much output it wrote, keeping that output.
              step flush inputPtr inputLength revOutput = do
                status <- c_deflate stream inputPtr (fromIntegral inputLength) flush outputBuffer (fromIntegral outputBufferBytes) inputUsedPtr outputUsedPtr
                when (status /= zOk && status /= zStreamEnd && status /= zBufError)
                  $ throwString ("deflate failed with status " <> show status)
                inputUsed <- fromIntegral <$> peek inputUsedPtr
                outputUsed <- fromIntegral <$> peek outputUsedPtr
                revOutput' <-
                  if outputUsed == 0
                    then pure revOutput
                    else (: revOutput) <$> ByteString.packCStringLen (castPtr outputBuffer, outputUsed)
                pure (status, inputUsed, outputUsed, revOutput')

              compressChunk revOutput chunk =
                ByteString.Unsafe.unsafeUseAsCStringLen chunk \(chunkPtr, chunkLength) ->
                  let go offset acc
                        | offset >= chunkLength = pure acc
                        | otherwise = do
                            (_, inputUsed, _, acc') <- step zNoFlush (castPtr chunkPtr `plusPtr` offset) (chunkLength - offset) acc
                            go (offset + inputUsed) acc'
                   in go 0 revOutput

              -- A full flush is complete once a call leaves room in the output
              -- buffer, a finish once deflate reports the end of the stream.
              flushAll flush revOutput = do
                (status, _, outputUsed, revOutput') <- step flush nullPtr (0 :: Int) revOutput
                let done
                      | flush == zFinish = status == zStreamEnd
                      | otherwise = outputUsed < outputBufferBytes
                if done then pure revOutput' else flushAll flush revOutput'

          revCompressed <- foldM compressChunk [] (LByteString.toChunks input)
          revFlushed <- flushAll (if segmentEnd == Final then zFinish else zFullFlush) revCompressed
          pure (LByteString.fromChunks (reverse revFlushed))
  where
    acquire = do
      stream <- c_deflateNew (fromIntegral level)
      when (stream == nullPtr) $ throwString "deflateInit2 failed"
      pure stream

-- | Update a CRC-32 (the ZIP checksum) with more bytes.
crc32 :: Word32 -> LByteString -> IO Word32
crc32 initial input = foldM update initial (LByteString.toChunks input)
  where
    update crc chunk =
      ByteString.Unsafe.unsafeUseAsCStringLen chunk \(chunkPtr, chunkLength) ->
        fromIntegral <$> c_crc32 (fromIntegral crc) (castPtr chunkPtr) (fromIntegral chunkLength)
