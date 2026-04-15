{-# LANGUAGE CPP #-}

module DataFrame.IO.Parquet.Brotli (decompress) where

import qualified Data.ByteString as BS

#ifdef mingw32_HOST_OS
decompress :: Int -> BS.ByteString -> IO BS.ByteString
decompress _ _ =
    error
        "BROTLI decompression requires libbrotlidec and is not supported on Windows in this build"
#else
import Control.Exception (SomeException, try)
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.List (intercalate)
import Data.Word (Word8)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (FunPtr, Ptr, castPtr)
import Foreign.Storable (peek, poke)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.DynamicLinker (DL, RTLDFlags (RTLD_NOW), dlclose, dlopen, dlsym)

data BrotliDecoder = BrotliDecoder
    { _decoderHandle :: DL
    , decoderDecompress :: BrotliDecoderDecompressFn
    }

type BrotliDecoderDecompressFn =
    CSize -> Ptr Word8 -> Ptr CSize -> Ptr Word8 -> IO CInt

foreign import ccall unsafe "dynamic"
    mkBrotliDecoderDecompressFn ::
        FunPtr BrotliDecoderDecompressFn -> BrotliDecoderDecompressFn

brotliDecoder :: Either String BrotliDecoder
brotliDecoder = unsafePerformIO loadBrotliDecoder
{-# NOINLINE brotliDecoder #-}

brotliLibraryCandidates :: [FilePath]
brotliLibraryCandidates =
    [ "libbrotlidec.so.1"
    , "libbrotlidec.so"
    , "libbrotlidec.dylib"
    , "/opt/homebrew/opt/brotli/lib/libbrotlidec.1.dylib"
    , "/opt/homebrew/lib/libbrotlidec.dylib"
    , "/usr/local/lib/libbrotlidec.dylib"
    ]

loadBrotliDecoder :: IO (Either String BrotliDecoder)
loadBrotliDecoder = go brotliLibraryCandidates []
  where
    go [] errorsSeen =
        pure $
            Left $
                unlines
                    [ "Unable to load libbrotlidec for Parquet BROTLI decoding."
                    , "Tried: " ++ intercalate ", " brotliLibraryCandidates
                    , "Errors:"
                    , unlines (map ("  " ++) (reverse errorsSeen))
                    ]
    go (candidate : rest) errorsSeen = do
        opened <- try (dlopen candidate [RTLD_NOW]) :: IO (Either SomeException DL)
        case opened of
            Left err ->
                go rest (formatError candidate err : errorsSeen)
            Right handle -> do
                symbolResult <-
                    try (dlsym handle "BrotliDecoderDecompress") ::
                        IO (Either SomeException (FunPtr BrotliDecoderDecompressFn))
                case symbolResult of
                    Left err -> do
                        dlclose handle
                        go rest (formatError candidate err : errorsSeen)
                    Right fnPtr ->
                        pure $
                            Right $
                                BrotliDecoder
                                    { _decoderHandle = handle
                                    , decoderDecompress = mkBrotliDecoderDecompressFn fnPtr
                                    }

    formatError candidate err = candidate ++ ": " ++ show err

brotliDecoderSuccess :: CInt
brotliDecoderSuccess = 1

decompress :: Int -> BS.ByteString -> IO BS.ByteString
decompress expectedSize compressed
    | expectedSize < 0 =
        error ("BROTLI decompression requires a non-negative size, got " ++ show expectedSize)
    | otherwise =
        case brotliDecoder of
            Left err -> error err
            Right decoder ->
                BSU.unsafeUseAsCStringLen compressed $ \(inputPtr, inputLen) ->
                    withOutputBuffer expectedSize $ \outputPtr -> do
                        actualSize <-
                            runDecoder
                                decoder
                                (fromIntegral inputLen)
                                (castPtr inputPtr)
                                outputPtr
                                expectedSize
                        validateDecodedSize expectedSize actualSize

withOutputBuffer :: Int -> (Ptr Word8 -> IO ()) -> IO BS.ByteString
withOutputBuffer expectedSize useOutputPtr
    | expectedSize == 0 =
        allocaBytes 1 $ \outputPtr -> do
            useOutputPtr outputPtr
            pure BS.empty
    | otherwise = do
        fp <- BSI.mallocByteString expectedSize
        withForeignPtr fp useOutputPtr
        pure (BSI.fromForeignPtr fp 0 expectedSize)

runDecoder :: BrotliDecoder -> CSize -> Ptr Word8 -> Ptr Word8 -> Int -> IO Int
runDecoder decoder inputLen inputPtr outputPtr expectedSize =
    alloca $ \outputSizePtr -> do
        poke outputSizePtr (fromIntegral expectedSize)
        result <-
            decoderDecompress decoder inputLen inputPtr outputSizePtr outputPtr
        validateDecoderResult result
        fromIntegral <$> peek outputSizePtr

validateDecoderResult :: CInt -> IO ()
validateDecoderResult result
    | result == brotliDecoderSuccess = pure ()
    | otherwise =
        error
            ("BROTLI decompression failed with result code " ++ show result)

validateDecodedSize :: Int -> Int -> IO ()
validateDecodedSize expectedSize actualSize
    | actualSize == expectedSize = pure ()
    | otherwise =
        error
            ( "BROTLI decompressed size mismatch: expected "
                ++ show expectedSize
                ++ " bytes, got "
                ++ show actualSize
            )
#endif
