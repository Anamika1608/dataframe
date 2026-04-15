module ParquetTestHelpers (
    assertFirstColumnCodec,
    buildDataPageV1,
    encodePlainInt32Payload,
) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor)
import qualified Data.ByteString as BS
import Data.Int
import Data.Word
import qualified DataFrame.IO.Parquet as DP
import DataFrame.IO.Parquet.Thrift (
    columnCodec,
    columnMetaData,
    compactI32,
    compactStruct,
    rowGroupColumns,
    rowGroups,
 )
import DataFrame.IO.Parquet.Types (CompressionCodec)
import DataFrame.Internal.Binary (word32ToLittleEndian)
import Test.HUnit (Assertion, assertEqual, assertFailure)

assertFirstColumnCodec :: String -> CompressionCodec -> FilePath -> Assertion
assertFirstColumnCodec label expected path = do
    (metadata, _) <- DP.readMetadataFromPath path
    case rowGroups metadata of
        [] ->
            assertFailure (label ++ ": parquet file has no row groups")
        rowGroup : _ -> case rowGroupColumns rowGroup of
            [] ->
                assertFailure (label ++ ": first row group has no columns")
            columnChunk : _ ->
                assertEqual label expected (columnCodec (columnMetaData columnChunk))

buildDataPageV1 :: Int32 -> BS.ByteString -> BS.ByteString -> BS.ByteString
buildDataPageV1 numValues payload compressedPayload =
    BS.pack
        ( field 1 compactI32 (zigZag32 0)
            ++ field 1 compactI32 (zigZag32 (fromIntegral (BS.length payload)))
            ++ field 1 compactI32 (zigZag32 (fromIntegral (BS.length compressedPayload)))
            ++ [fieldHeader 2 compactStruct]
            ++ field 1 compactI32 (zigZag32 numValues)
            ++ field 1 compactI32 (zigZag32 0)
            ++ field 1 compactI32 (zigZag32 0)
            ++ field 1 compactI32 (zigZag32 0)
            ++ [0x00, 0x00]
        )
        <> compressedPayload

encodePlainInt32Payload :: [Int32] -> BS.ByteString
encodePlainInt32Payload =
    BS.concat
        . map (word32ToLittleEndian . fromIntegral)

field :: Word8 -> Word8 -> [Word8] -> [Word8]
field delta encodedType contents = fieldHeader delta encodedType : contents

fieldHeader :: Word8 -> Word8 -> Word8
fieldHeader delta encodedType = (delta `shiftL` 4) .|. encodedType

zigZag32 :: Int32 -> [Word8]
zigZag32 n =
    encodeVarInt
        (fromIntegral (((fromIntegral n :: Word32) `shiftL` 1) `xor` fromIntegral (n `shiftR` 31)))

encodeVarInt :: Word64 -> [Word8]
encodeVarInt n
    | n < 0x80 = [fromIntegral n]
    | otherwise =
        fromIntegral ((n .&. 0x7F) .|. 0x80) : encodeVarInt (n `shiftR` 7)
