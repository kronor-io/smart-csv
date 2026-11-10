module Test.Kronor.SmartCsv.Xlsx (tests) where

import Codec.Compression.Zlib.Raw qualified as Raw
import Data.Aeson qualified as Aeson
import Data.Bits (shiftL, shiftR, testBit, (.|.))
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.ByteString.Lazy qualified as LByteString
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Kronor.SmartCsv.ColumnConfig (ColumnConfig, ColumnSettings (..))
import Kronor.SmartCsv.Deflate (SegmentEnd (..), crc32, deflateSegment)
import Kronor.SmartCsv.Xlsx (SheetEntry (..), XlsxCell (..), appendRows, finishArchive, mkXlsxStyles, sheetNameFor, startArchive, xlsxRowBytes, xlsxify)
import RIO
import RIO.List (find, iterate)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Xlsx"
    [ testCase "deflate segments ending in a full flush join into one stream" testSegmentsJoin,
      testCase "crc32 gives the standard check value" testCrc32,
      testCase "xlsxify keeps numbers as numbers" testXlsxifyNumbers,
      testCase "xlsxify writes numbers Excel would round as text" testXlsxifyPrecision,
      testCase "xlsxify follows data paths and joins arrays like csvify" testXlsxifyPathsAndArrays,
      testCase "xlsxRowBytes writes every cell so empty ones keep their column" testRowBytes,
      testCase "xlsxRowBytes escapes markup and characters XML cannot hold" testEscaping,
      testCase "sheetNameFor makes a valid sheet name" testSheetName,
      testCase "an archive built page by page reads back as a valid ZIP" testArchiveRoundTrip
    ]

testSegmentsJoin :: IO ()
testSegmentsJoin = do
  -- Large enough that a segment needs several passes of the 64 KB output buffer.
  let pages = ["first page ", pseudoRandomBytes 300_000, " last page"]
  segments <-
    zipWithM
      (\index page -> deflateSegment 6 (if index == length pages then Final else KeepOpen) page)
      [1 :: Int ..]
      pages
  Raw.decompress (mconcat segments) @?= mconcat pages

testCrc32 :: IO ()
testCrc32 = do
  checksum <- crc32 0 "123456789"
  checksum @?= 0xCBF43926
  -- Updating page by page gives the checksum of the whole.
  split <- crc32 0 "12345" >>= (`crc32` "6789")
  split @?= 0xCBF43926

testXlsxifyNumbers :: IO ()
testXlsxifyNumbers = do
  let row =
        Aeson.object
          [ ("id", Aeson.Number 7),
            ("amount", Aeson.Number 12.345),
            ("rate", Aeson.String "0.50"),
            ("reference", Aeson.String "0083810"),
            ("approved", Aeson.Bool True),
            ("memo", Aeson.Null)
          ]
  xlsxify numberConfig row
    @?= Map.fromList
      [ ("id", CellNumber "7" Nothing),
        ("Amount", CellNumber "12.34" (Just 2)),
        ("rate", CellNumber "0.5" Nothing),
        ("reference", CellText "0083810"),
        ("approved", CellBool True),
        ("memo", CellEmpty)
      ]

testXlsxifyPrecision :: IO ()
testXlsxifyPrecision = do
  let row =
        Aeson.object
          [ ("id", Aeson.Number 1234567890123456789),
            ("amount", Aeson.Number 12345678901234.567),
            ("rate", Aeson.String "0.12345678901234567"),
            ("big", Aeson.Number 1e20)
          ]
  xlsxify numberConfig row
    @?= Map.fromList
      [ ("id", CellText "1234567890123456789"),
        ("Amount", CellText "12345678901234.56"),
        ("rate", CellText "0.12345678901234567"),
        ("big", CellNumber "1E20" Nothing)
      ]

testXlsxifyPathsAndArrays :: IO ()
testXlsxifyPathsAndArrays = do
  let config =
        Map.fromList
          [ ("attempts", ColumnSettings Nothing (Just "Card Type") (Just "payment.cardType") False),
            ("customer", ColumnSettings Nothing (Just "Customer Email") (Just "profile.email") False)
          ]
      attempt cardType = Aeson.object [("payment", Aeson.object [("cardType", cardType)])]
      row =
        Aeson.object
          [ ("attempts", Aeson.Array (Vector.fromList [attempt "VISA", Aeson.Null, attempt Aeson.Null, attempt "MC"])),
            ("customer", Aeson.object [("profile", Aeson.object [("email", "ada@example.com"), ("name", "Ada")])]),
            ("tags", Aeson.Array Vector.empty)
          ]
  xlsxify config row
    @?= Map.fromList
      [ ("Card Type", CellText "VISA, MC"),
        ("Customer Email", CellText "ada@example.com"),
        ("tags", CellEmpty)
      ]

testRowBytes :: IO ()
testRowBytes = do
  let styles = mkXlsxStyles numberConfig
      headers = Vector.fromList ["reference", "memo", "Amount", "approved"]
      row = Aeson.object [("reference", "x"), ("amount", Aeson.Number 1.5), ("approved", Aeson.Bool False)]
  xlsxRowBytes numberConfig styles headers row
    @?= "<row><c t=\"inlineStr\"><is><t>x</t></is></c><c/><c s=\"1\"><v>1.5</v></c><c t=\"b\"><v>0</v></c></row>"

testEscaping :: IO ()
testEscaping = do
  let styles = mkXlsxStyles mempty
      headers = Vector.fromList ["a", "b"]
      row = Aeson.object [("a", "1 < 2 & 3 > 2\v_x0041_ \8364"), ("b", " padded")]
  xlsxRowBytes mempty styles headers row
    @?= LByteString.fromStrict
      ( encodeUtf8
          "<row><c t=\"inlineStr\"><is><t>1 &lt; 2 &amp; 3 &gt; 2_x000B__x005F_x0041_ \8364</t></is></c>\
          \<c t=\"inlineStr\"><is><t xml:space=\"preserve\"> padded</t></is></c></row>"
      )

testSheetName :: IO ()
testSheetName = do
  sheetNameFor "vendorBills" @?= "vendorBills"
  sheetNameFor "a/b:c[d]" @?= "a_b_c_d_"
  sheetNameFor (Text.replicate 40 "x") @?= Text.replicate 31 "x"
  sheetNameFor "''" @?= "Sheet1"

testArchiveRoundTrip :: IO ()
testArchiveRoundTrip = do
  let styles = mkXlsxStyles numberConfig
      headers = Vector.fromList ["reference", "Amount"]
      rowBytes reference amount = xlsxRowBytes numberConfig styles headers (Aeson.object [("reference", reference), ("amount", Aeson.Number amount)])
      firstPage = rowBytes "a" 1 <> rowBytes "b" 2
      secondPage = rowBytes "c" 3
  (prefix, archive, entry0) <- startArchive 6 "payments" styles headers
  (firstSegment, entry1) <- appendRows archive entry0 firstPage
  (secondSegment, entry2) <- appendRows archive entry1 secondPage
  trailer <- finishArchive archive entry2
  entry2.uncompressedBytes > entry0.uncompressedBytes @?= True
  let file = LByteString.toStrict (prefix <> firstSegment <> secondSegment <> trailer)

  entries <- either assertFailure pure (readZip file)
  map (.name) entries @?= ["[Content_Types].xml", "_rels/.rels", "xl/workbook.xml", "xl/_rels/workbook.xml.rels", "xl/styles.xml", "xl/worksheets/sheet1.xml"]
  for_ entries \entry -> do
    checksum <- crc32 0 entry.contents
    (entry.name, checksum) @?= (entry.name, entry.crc)

  let sheetXml = maybe "" (.contents) (find ((== "xl/worksheets/sheet1.xml") . (.name)) entries)
  sheetXml
    @?= "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n\
        \<worksheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\"><sheetData>\
        \<row><c t=\"inlineStr\"><is><t>reference</t></is></c><c t=\"inlineStr\"><is><t>Amount</t></is></c></row>"
    <> firstPage
    <> secondPage
    <> "</sheetData></worksheet>"
  let workbookXml = maybe "" (.contents) (find ((== "xl/workbook.xml") . (.name)) entries)
  ByteString.Char8.isInfixOf "<sheet name=\"payments\"" (LByteString.toStrict workbookXml) @?= True

numberConfig :: ColumnConfig
numberConfig =
  Map.fromList
    [ ("amount", ColumnSettings (Just 2) (Just "Amount") Nothing False),
      ("rate", ColumnSettings Nothing Nothing Nothing True)
    ]

-- | Bytes that deflate barely compresses.
pseudoRandomBytes :: Int -> LByteString
pseudoRandomBytes count = LByteString.pack (take count (map (fromIntegral . (`shiftR` 16)) (iterate step 12345)))
  where
    step :: Word32 -> Word32
    step x = x * 1103515245 + 12345

data ZipFile = ZipFile
  { name :: ByteString,
    crc :: Word32,
    contents :: LByteString
  }

-- | Read a ZIP the way an unzip tool does: from the directory at the end, then
-- each entry's local header and data.  Entries whose sizes follow their data
-- must have a matching data descriptor.
readZip :: ByteString -> Either String [ZipFile]
readZip file = do
  let endOffset = ByteString.length file - 22
  expectSignature endOffset 0x06054b50 "end of central directory"
  let entryCount = word16At (endOffset + 10)
      directoryOffset = word32At (endOffset + 16)
  go entryCount directoryOffset
  where
    go 0 _ = Right []
    go remaining offset = do
      expectSignature offset 0x02014b50 "central directory header"
      let flags = word16At (offset + 8)
          checksum = fromIntegral (word32At (offset + 16))
          compressedSize = word32At (offset + 20)
          uncompressedSize = word32At (offset + 24)
          nameLength = word16At (offset + 28)
          extraLength = word16At (offset + 30)
          commentLength = word16At (offset + 32)
          localOffset = word32At (offset + 42)
          name = ByteString.take nameLength (ByteString.drop (offset + 46) file)
      expectSignature localOffset 0x04034b50 "local file header"
      let dataOffset = localOffset + 30 + word16At (localOffset + 26) + word16At (localOffset + 28)
          compressed = LByteString.fromStrict (ByteString.take compressedSize (ByteString.drop dataOffset file))
          contents = Raw.decompress compressed
      when (fromIntegral (LByteString.length contents) /= uncompressedSize)
        $ Left ("size mismatch for " <> show name)
      when (testBit flags 3) do
        let descriptorOffset = dataOffset + compressedSize
        expectSignature descriptorOffset 0x08074b50 "data descriptor"
        when ((word32At (descriptorOffset + 4), word32At (descriptorOffset + 8), word32At (descriptorOffset + 12)) /= (fromIntegral checksum, compressedSize, uncompressedSize))
          $ Left ("data descriptor disagrees with the directory for " <> show name)
      rest <- go (remaining - 1) (offset + 46 + nameLength + extraLength + commentLength)
      pure (ZipFile {name, crc = checksum, contents} : rest)

    expectSignature offset signature what =
      unless (word32At offset == signature) $ Left ("no " <> what <> " at " <> show offset)

    byteAt offset = fromIntegral (ByteString.index file offset) :: Int
    word16At offset = byteAt offset .|. (byteAt (offset + 1) `shiftL` 8)
    word32At offset = word16At offset .|. (word16At (offset + 2) `shiftL` 16)
