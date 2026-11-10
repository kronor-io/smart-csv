-- | Excel (XLSX) output, written page by page so it can be uploaded as it is made.
--
-- An XLSX file is a ZIP archive.  The small files go first with their sizes known;
-- the sheet comes last as one entry whose checksum and sizes follow its data (a
-- "data descriptor"), so nothing already uploaded is ever patched.  The sheet's
-- XML is compressed page by page ('appendRows'): each page is a deflate segment
-- of its own that ends with a full flush, so the segments join into one deflate
-- stream and only 'SheetEntry' (a checksum and two byte counts) carries over from
-- page to page.
module Kronor.SmartCsv.Xlsx
  ( -- * Cells
    XlsxCell (..),
    xlsxify,
    XlsxStyles,
    mkXlsxStyles,
    xlsxRowBytes,
    excelMaxDataRows,
    sheetNameFor,

    -- * Archive
    XlsxArchive,
    SheetEntry (..),
    startArchive,
    appendRows,
    finishArchive,
    archiveFits,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.ByteString.Lazy qualified as LByteString
import Data.Char (isHexDigit, isSpace, toUpper)
import Data.Map.Strict qualified as Map
import Data.Scientific (FPFormat (Exponent, Fixed), Scientific, base10Exponent, coefficient, formatScientific, normalize, scientific)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Unsafe qualified as Text.Unsafe
import Kronor.SmartCsv.ColumnConfig (ColumnConfig, ColumnSettings (..), columnDataPath, columnDecimalPlaces, columnHeader, columnIsNumeric)
import Kronor.SmartCsv.Deflate (SegmentEnd (..), crc32, deflateSegment)
import Kronor.SmartCsv.Flatten (resolveValue)
import Numeric (showHex)
import RIO

-- * Cells

-- | One cell of the sheet.
data XlsxCell
  = CellText Text
  | -- | A number as Excel reads it (see 'numberCell'), with the decimal places its
    -- column shows when the column config sets them.
    CellNumber ByteString (Maybe Int)
  | CellBool Bool
  | CellEmpty
  deriving stock (Eq, Show)

-- | Convert one GraphQL response row into cells keyed by header, the way
-- 'Kronor.SmartCsv.Flatten.csvify' does for CSV: the same data paths, array
-- joining and decimal places, but numbers stay numbers.
xlsxify :: ColumnConfig -> Aeson.Value -> Map Text XlsxCell
xlsxify colConfig (Aeson.Object obj) =
  -- The first column wins when two share a header, as in 'csvify'.
  Map.fromListWith (\_ firstCell -> firstCell)
    $ mapMaybe extractColumn (Aeson.KeyMap.toList obj)
  where
    extractColumn (key, value) = do
      let columnId = Aeson.Key.toText key
      cell <- renderValue columnId (columnDataPath columnId colConfig) value
      pure (columnHeader columnId colConfig, cell)

    renderValue :: Text -> [Text] -> Aeson.Value -> Maybe XlsxCell
    renderValue columnId path (Aeson.Array array) =
      case mapMaybe (cellText <=< renderValue columnId path) (filter (/= Aeson.Null) (toList array)) of
        [] -> Just CellEmpty
        texts -> Just (CellText (Text.intercalate ", " texts))
    renderValue columnId path value = resolveValue path value >>= renderLeaf columnId

    renderLeaf :: Text -> Aeson.Value -> Maybe XlsxCell
    renderLeaf columnId = \case
      Aeson.String text
        | Just decimals <- configuredDecimals columnId ->
            Just (maybe (CellText text) (\value -> numberCell (truncateScientific decimals value) (Just decimals)) (readScientific text))
        -- A number Hasura sent as a string; its own digits are the fallback.
        | columnIsNumeric columnId colConfig ->
            Just (maybe (CellText text) (`CellNumber` Nothing) (readScientific text >>= excelNumber))
        | otherwise -> Just (CellText text)
      Aeson.Number value -> Just $ case configuredDecimals columnId of
        Just decimals -> numberCell (truncateScientific decimals value) (Just decimals)
        Nothing -> numberCell value Nothing
      Aeson.Bool flag -> Just (CellBool flag)
      Aeson.Null -> Just CellEmpty
      Aeson.Array _ -> Nothing
      Aeson.Object _ -> Nothing

    configuredDecimals columnId = clampDecimals <$> columnDecimalPlaces columnId colConfig

    readScientific :: Text -> Maybe Scientific
    readScientific = readMaybe . Text.unpack

    cellText = \case
      CellText text | not (Text.null text) -> Just text
      CellText _ -> Nothing
      CellNumber literal _ -> Just (decodeUtf8Lenient literal)
      CellBool flag -> Just (if flag then "True" else "False")
      CellEmpty -> Nothing
xlsxify _ v = error ("xlsxify: expected a JSON object, but received: " <> show v)

-- | Same truncation as the CSV output, so both files show the same digits.
truncateScientific :: Int -> Scientific -> Scientific
truncateScientific decimals value =
  let factor = scientific 1 decimals
      truncated = truncate (value * factor) :: Integer
   in scientific truncated (negate decimals)

clampDecimals :: Int -> Int
clampDecimals = max 0 . min 30

-- | A number cell, or a text cell with the exact digits when Excel would change
-- the number (see 'excelNumber').
numberCell :: Scientific -> Maybe Int -> XlsxCell
numberCell value mDecimals =
  case excelNumber value of
    Just literal -> CellNumber literal mDecimals
    Nothing -> CellText (exactNumberText value mDecimals)

-- | The number as a cell value, when Excel can hold it unchanged: Excel keeps 15
-- significant digits and exponents up to about ±308.
excelNumber :: Scientific -> Maybe ByteString
excelNumber value
  | digits == 0 = Just "0"
  | digitCount > 15 || exponent' + digitCount > 308 || exponent' < -307 = Nothing
  | exponent' >= 0 && digitCount + exponent' <= 15 = Just (ByteString.Char8.pack (show (digits * 10 ^ exponent')))
  | exponent' < 0 && exponent' >= -20 = Just (ByteString.Char8.pack (formatScientific Fixed Nothing normalized))
  | otherwise = Just (ByteString.Char8.pack (show digits <> "E" <> show exponent'))
  where
    normalized = normalize value
    digits = coefficient normalized
    exponent' = base10Exponent normalized
    digitCount = length (show (abs digits))

exactNumberText :: Scientific -> Maybe Int -> Text
exactNumberText value = \case
  Just decimals -> Text.pack (formatScientific Fixed (Just decimals) value)
  Nothing
    | exponent' >= 0 && exponent' <= 100 -> tshow (coefficient normalized * 10 ^ exponent')
    | exponent' > 100 -> Text.pack (formatScientific Exponent Nothing value)
    | otherwise -> Text.pack (formatScientific Fixed Nothing value)
  where
    normalized = normalize value
    exponent' = base10Exponent normalized

-- | Cell styles: the default one, and one number format per number of decimal
-- places the column config uses.
data XlsxStyles = XlsxStyles
  { styleOfDecimals :: Map Int Int,
    stylesXml :: LByteString
  }

mkXlsxStyles :: ColumnConfig -> XlsxStyles
mkXlsxStyles colConfig =
  XlsxStyles
    { styleOfDecimals = Map.fromList (zip decimalsUsed [1 ..]),
      stylesXml = Builder.toLazyByteString xml
    }
  where
    decimalsUsed = Set.toAscList (Set.fromList (mapMaybe (fmap clampDecimals . (.decimalPlaces)) (Map.elems colConfig)))
    formats = zip [164 :: Int ..] decimalsUsed
    formatCode decimals = "#,##0" <> (if decimals == 0 then "" else "." <> Builder.string7 (replicate decimals '0'))
    xml =
      xmlDeclaration
        <> "<styleSheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\">"
        <> ( if null formats
               then mempty
               else
                 "<numFmts count=\""
                   <> Builder.intDec (length formats)
                   <> "\">"
                   <> foldMap (\(formatId, decimals) -> "<numFmt numFmtId=\"" <> Builder.intDec formatId <> "\" formatCode=\"" <> formatCode decimals <> "\"/>") formats
                   <> "</numFmts>"
           )
        <> "<fonts count=\"1\"><font><sz val=\"11\"/><name val=\"Calibri\"/></font></fonts>"
        <> "<fills count=\"2\"><fill><patternFill patternType=\"none\"/></fill><fill><patternFill patternType=\"gray125\"/></fill></fills>"
        <> "<borders count=\"1\"><border><left/><right/><top/><bottom/><diagonal/></border></borders>"
        <> "<cellStyleXfs count=\"1\"><xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\"/></cellStyleXfs>"
        <> "<cellXfs count=\""
        <> Builder.intDec (1 + length formats)
        <> "\"><xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\" xfId=\"0\"/>"
        <> foldMap (\(formatId, _) -> "<xf numFmtId=\"" <> Builder.intDec formatId <> "\" fontId=\"0\" fillId=\"0\" borderId=\"0\" xfId=\"0\" applyNumberFormat=\"1\"/>") formats
        <> "</cellXfs>"
        <> "<cellStyles count=\"1\"><cellStyle name=\"Normal\" xfId=\"0\" builtinId=\"0\"/></cellStyles>"
        <> "</styleSheet>"

-- | One response row as a sheet row, its cells in @headers@ order.
xlsxRowBytes :: ColumnConfig -> XlsxStyles -> Vector Text -> Aeson.Value -> LByteString
xlsxRowBytes colConfig styles headers rowValue =
  Builder.toLazyByteString (rowXml styles headers (xlsxify colConfig rowValue))

-- | Rows and cells carry no positions (@r="B7"@): every cell is written, an
-- empty one as @<c/>@, so each lands in the column after the previous one.
rowXml :: XlsxStyles -> Vector Text -> Map Text XlsxCell -> Builder
rowXml styles headers cells =
  "<row>" <> foldMap (\header -> cellXml styles (Map.findWithDefault CellEmpty header cells)) headers <> "</row>"

headerRowXml :: XlsxStyles -> Vector Text -> Builder
headerRowXml styles headers = "<row>" <> foldMap (cellXml styles . CellText) headers <> "</row>"

cellXml :: XlsxStyles -> XlsxCell -> Builder
cellXml styles = \case
  CellEmpty -> "<c/>"
  CellBool True -> "<c t=\"b\"><v>1</v></c>"
  CellBool False -> "<c t=\"b\"><v>0</v></c>"
  CellNumber literal mDecimals ->
    "<c" <> styleAttribute mDecimals <> "><v>" <> Builder.byteString literal <> "</v></c>"
  CellText text
    | Text.null text -> "<c/>"
    | otherwise ->
        "<c t=\"inlineStr\"><is><t"
          <> (if keepsOuterSpace text then " xml:space=\"preserve\"" else mempty)
          <> ">"
          <> cellTextXml text
          <> "</t></is></c>"
  where
    styleAttribute mDecimals =
      case mDecimals >>= (`Map.lookup` styles.styleOfDecimals) of
        Just style -> " s=\"" <> Builder.intDec style <> "\""
        Nothing -> mempty

    keepsOuterSpace text = isSpace (Text.head text) || isSpace (Text.last text)

-- | Text as it goes into a cell: cut to Excel's limit, markup escaped, and the
-- characters XML cannot hold written as Excel's @_xHHHH_@ escapes.  A literal
-- @_xHHHH_@ in the data gets its underscore escaped, or Excel would decode it.
cellTextXml :: Text -> Builder
cellTextXml text0
  | Text.all plainChar text && not ("_x" `Text.isInfixOf` text) = Text.encodeUtf8Builder text
  | otherwise = escape (Text.unpack text)
  where
    text = excelLengthText text0

    plainChar char = (char >= ' ' && char /= '&' && char /= '<' && char /= '>' && char < '\xFFFE') || char == '\t' || char == '\n' || char > '\xFFFF'

    escape = \case
      [] -> mempty
      '_' : rest@('x' : a : b : c : d : '_' : _)
        | all isHexDigit [a, b, c, d] -> "_x005F_" <> escape rest
      char : rest -> escapeChar char <> escape rest

    escapeChar = \case
      '&' -> "&amp;"
      '<' -> "&lt;"
      '>' -> "&gt;"
      char
        | char == '\t' || char == '\n' -> Builder.charUtf8 char
        | char < ' ' || char == '\xFFFE' || char == '\xFFFF' -> "_x" <> Builder.string7 (hex4 char) <> "_"
        | otherwise -> Builder.charUtf8 char

    hex4 char = let digits = map toUpper (showHex (fromEnum char) "") in replicate (4 - length digits) '0' <> digits

-- | Excel holds at most 32,767 UTF-16 code units in a cell.
excelLengthText :: Text -> Text
excelLengthText text
  -- A UTF-8 byte count never undercounts UTF-16 code units, and it is O(1).
  | Text.Unsafe.lengthWord8 text <= excelMaxTextUnits = text
  | otherwise = Text.pack (takeUnits excelMaxTextUnits (Text.unpack text))
  where
    takeUnits _ [] = []
    takeUnits remaining (char : rest)
      | units char > remaining = []
      | otherwise = char : takeUnits (remaining - units char) rest
    units char = if char > '\xFFFF' then 2 else 1

excelMaxTextUnits :: Int
excelMaxTextUnits = 32_767

-- | Rows a sheet holds below its header row.
excelMaxDataRows :: Int
excelMaxDataRows = 1_048_575

-- | A valid sheet name: at most 31 characters, none of @[]:*?/\\@, not starting or
-- ending with an apostrophe.
sheetNameFor :: Text -> Text
sheetNameFor name =
  let cleaned = Text.take 31 (Text.dropAround (== '\'') (Text.map (\char -> if char `elem` ("[]:*?/\\" :: String) then '_' else char) name))
   in if Text.null cleaned then "Sheet1" else cleaned

-- * Archive

-- | The archive's fixed part, which the trailer refers back to.
data XlsxArchive = XlsxArchive
  { compressionLevel :: Int,
    fixedEntries :: [ZipEntry],
    sheetOffset :: Word64,
    sheetDataOffset :: Word64
  }

-- | What the sheet entry holds so far; all that carries over from page to page.
data SheetEntry = SheetEntry
  { crc :: Word32,
    uncompressedBytes :: Word64,
    compressedBytes :: Word64
  }
  deriving stock (Eq, Show)

data ZipEntry = ZipEntry
  { name :: ByteString,
    flags :: Word16,
    crc :: Word32,
    compressedBytes :: Word64,
    uncompressedBytes :: Word64,
    offset :: Word64
  }

-- | The bytes that open the file: the fixed files, the sheet entry's header, and
-- the sheet up to and including its header row.
startArchive :: Int -> Text -> XlsxStyles -> Vector Text -> IO (LByteString, XlsxArchive, SheetEntry)
startArchive level sheetName styles headers = do
  (revFixedBytes, revFixedEntries, sheetOffset) <- foldM addFixedFile ([], [], 0) (fixedFiles sheetName styles)
  let sheetHeader = Builder.toLazyByteString (localFileHeader sheetPath dataDescriptorFlag 0 0 0)
      sheetDataOffset = sheetOffset + fromIntegral (LByteString.length sheetHeader)
  (opening, entry) <- appendSegment level KeepOpen (Builder.toLazyByteString (sheetPrologue <> headerRowXml styles headers)) (SheetEntry 0 0 0)
  pure
    ( LByteString.concat (reverse revFixedBytes) <> sheetHeader <> opening,
      XlsxArchive
        { compressionLevel = level,
          fixedEntries = reverse revFixedEntries,
          sheetOffset,
          sheetDataOffset
        },
      entry
    )
  where
    addFixedFile (revBytes, revEntries, offset) (name, contents) = do
      compressed <- deflateSegment level Final contents
      checksum <- crc32 0 contents
      let entry =
            ZipEntry
              { name,
                flags = 0,
                crc = checksum,
                compressedBytes = fromIntegral (LByteString.length compressed),
                uncompressedBytes = fromIntegral (LByteString.length contents),
                offset
              }
          bytes = Builder.toLazyByteString (localFileHeader name 0 checksum entry.compressedBytes entry.uncompressedBytes) <> compressed
      pure (bytes : revBytes, entry : revEntries, offset + fromIntegral (LByteString.length bytes))

-- | Compress a page of rows (see 'xlsxRowBytes') into the sheet entry.
appendRows :: XlsxArchive -> SheetEntry -> LByteString -> IO (LByteString, SheetEntry)
appendRows archive = flip (appendSegment archive.compressionLevel KeepOpen)

-- | The bytes that close the file: the end of the sheet, its checksum and sizes,
-- and the ZIP directory.
finishArchive :: XlsxArchive -> SheetEntry -> IO LByteString
finishArchive archive entry0 = do
  (closing, entry) <- appendSegment archive.compressionLevel Final sheetEpilogue entry0
  let sheetEntry =
        ZipEntry
          { name = sheetPath,
            flags = dataDescriptorFlag,
            crc = entry.crc,
            compressedBytes = entry.compressedBytes,
            uncompressedBytes = entry.uncompressedBytes,
            offset = archive.sheetOffset
          }
      descriptor =
        Builder.word32LE 0x08074b50
          <> Builder.word32LE entry.crc
          <> Builder.word32LE (fromIntegral entry.compressedBytes)
          <> Builder.word32LE (fromIntegral entry.uncompressedBytes)
      entries = archive.fixedEntries <> [sheetEntry]
      directory = Builder.toLazyByteString (foldMap centralDirectoryHeader entries)
      directoryOffset = archive.sheetDataOffset + entry.compressedBytes + 16
      endOfDirectory =
        Builder.word32LE 0x06054b50
          <> Builder.word16LE 0
          <> Builder.word16LE 0
          <> Builder.word16LE (fromIntegral (length entries))
          <> Builder.word16LE (fromIntegral (length entries))
          <> Builder.word32LE (fromIntegral (LByteString.length directory))
          <> Builder.word32LE (fromIntegral directoryOffset)
          <> Builder.word16LE 0
  pure (closing <> Builder.toLazyByteString descriptor <> directory <> Builder.toLazyByteString endOfDirectory)

-- | Whether the archive still fits the 4 GB limits of a ZIP without the ZIP64
-- extension, with room left for the trailer.  Excel has handled ZIP64 unevenly,
-- so the export stops instead.
archiveFits :: XlsxArchive -> SheetEntry -> Bool
archiveFits archive entry =
  entry.uncompressedBytes + slack <= zipLimit && archive.sheetDataOffset + entry.compressedBytes + slack <= zipLimit
  where
    zipLimit = 0xFFFFFFFF
    slack = 1_048_576

appendSegment :: Int -> SegmentEnd -> LByteString -> SheetEntry -> IO (LByteString, SheetEntry)
appendSegment level segmentEnd raw entry = do
  segment <- deflateSegment level segmentEnd raw
  checksum <- crc32 entry.crc raw
  pure
    ( segment,
      SheetEntry
        { crc = checksum,
          uncompressedBytes = entry.uncompressedBytes + fromIntegral (LByteString.length raw),
          compressedBytes = entry.compressedBytes + fromIntegral (LByteString.length segment)
        }
    )

-- | General purpose bit 3: the checksum and sizes follow the data.
dataDescriptorFlag :: Word16
dataDescriptorFlag = 0x08

deflateMethod, dosTime, dosDate, zipVersion :: Word16
deflateMethod = 8
dosTime = 0
-- 1980-01-01, the earliest date a ZIP entry can carry.
dosDate = 0x21
zipVersion = 20

localFileHeader :: ByteString -> Word16 -> Word32 -> Word64 -> Word64 -> Builder
localFileHeader name flags checksum compressedBytes uncompressedBytes =
  Builder.word32LE 0x04034b50
    <> Builder.word16LE zipVersion
    <> Builder.word16LE flags
    <> Builder.word16LE deflateMethod
    <> Builder.word16LE dosTime
    <> Builder.word16LE dosDate
    <> Builder.word32LE checksum
    <> Builder.word32LE (fromIntegral compressedBytes)
    <> Builder.word32LE (fromIntegral uncompressedBytes)
    <> Builder.word16LE (fromIntegral (ByteString.Char8.length name))
    <> Builder.word16LE 0
    <> Builder.byteString name

centralDirectoryHeader :: ZipEntry -> Builder
centralDirectoryHeader entry =
  Builder.word32LE 0x02014b50
    <> Builder.word16LE zipVersion
    <> Builder.word16LE zipVersion
    <> Builder.word16LE entry.flags
    <> Builder.word16LE deflateMethod
    <> Builder.word16LE dosTime
    <> Builder.word16LE dosDate
    <> Builder.word32LE entry.crc
    <> Builder.word32LE (fromIntegral entry.compressedBytes)
    <> Builder.word32LE (fromIntegral entry.uncompressedBytes)
    <> Builder.word16LE (fromIntegral (ByteString.Char8.length entry.name))
    <> Builder.word16LE 0
    <> Builder.word16LE 0
    <> Builder.word16LE 0
    <> Builder.word16LE 0
    <> Builder.word32LE 0
    <> Builder.word32LE (fromIntegral entry.offset)
    <> Builder.byteString entry.name

sheetPath :: ByteString
sheetPath = "xl/worksheets/sheet1.xml"

xmlDeclaration :: Builder
xmlDeclaration = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"

sheetPrologue :: Builder
sheetPrologue = xmlDeclaration <> "<worksheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\"><sheetData>"

sheetEpilogue :: LByteString
sheetEpilogue = "</sheetData></worksheet>"

fixedFiles :: Text -> XlsxStyles -> [(ByteString, LByteString)]
fixedFiles sheetName styles =
  [ ( "[Content_Types].xml",
      xmlFile
        $ "<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\">"
        <> "<Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/>"
        <> "<Default Extension=\"xml\" ContentType=\"application/xml\"/>"
        <> "<Override PartName=\"/xl/workbook.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml\"/>"
        <> "<Override PartName=\"/xl/worksheets/sheet1.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml\"/>"
        <> "<Override PartName=\"/xl/styles.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml\"/>"
        <> "</Types>"
    ),
    ( "_rels/.rels",
      xmlFile
        $ "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
        <> "<Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument\" Target=\"xl/workbook.xml\"/>"
        <> "</Relationships>"
    ),
    ( "xl/workbook.xml",
      xmlFile
        $ "<workbook xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\">"
        <> "<sheets><sheet name=\""
        <> attributeText (sheetNameFor sheetName)
        <> "\" sheetId=\"1\" r:id=\"rId1\"/></sheets>"
        <> "</workbook>"
    ),
    ( "xl/_rels/workbook.xml.rels",
      xmlFile
        $ "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
        <> "<Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet\" Target=\"worksheets/sheet1.xml\"/>"
        <> "<Relationship Id=\"rId2\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles\" Target=\"styles.xml\"/>"
        <> "</Relationships>"
    ),
    ("xl/styles.xml", styles.stylesXml)
  ]
  where
    xmlFile body = Builder.toLazyByteString (xmlDeclaration <> body)

attributeText :: Text -> Builder
attributeText = Text.encodeUtf8Builder . Text.concatMap escapeChar
  where
    escapeChar = \case
      '&' -> "&amp;"
      '<' -> "&lt;"
      '>' -> "&gt;"
      '"' -> "&quot;"
      char -> Text.singleton char
