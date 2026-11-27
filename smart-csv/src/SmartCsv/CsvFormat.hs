module SmartCsv.CsvFormat
  ( CsvFormat (..),
    DecimalSeparator (..),
    defaultCsvFormat,
    escapeFormula,
    withDecimalSeparator,
  )
where

import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Csv qualified as Csv
import RIO

-- | How a CSV file writes its cells.  'defaultCsvFormat' writes the files
-- smart-csv has always written.
--
-- Excel files need neither setting: numbers are stored as numbers, which Excel
-- shows in the reader's locale, and text is stored as text, which Excel never
-- evaluates as a formula.
data CsvFormat = CsvFormat
  { decimalSeparator :: DecimalSeparator,
    -- | Write a text cell that a spreadsheet would evaluate as a formula with a
    -- leading @'@, so it opens as the text it is (see 'escapeFormula').  Cells
    -- that start with a number are never changed: @-5@ stays @-5@.
    escapeFormulas :: Bool
  }
  deriving stock (Eq, Show)

-- | The character between a number's whole and fractional part.
data DecimalSeparator
  = -- | @1234,5@
    DecimalComma
  | -- | @1234.5@
    DecimalPoint
  deriving stock (Bounded, Enum, Eq, Show)

-- | Decimal comma, no formula escaping.
defaultCsvFormat :: CsvFormat
defaultCsvFormat = CsvFormat {decimalSeparator = DecimalComma, escapeFormulas = False}

-- | A number written with a decimal point, with the separator the format uses.
withDecimalSeparator :: DecimalSeparator -> String -> String
withDecimalSeparator DecimalPoint = id
withDecimalSeparator DecimalComma = map (\char -> if char == '.' then ',' else char)

-- | Prefix a cell with @'@ when it starts with a character that makes a
-- spreadsheet read it as a formula: @=@, @+@, @-@, @\@@, a tab or a carriage
-- return (OWASP's list for CSV injection).  Spreadsheets hide the @'@ and show
-- the rest as text.
escapeFormula :: Csv.Field -> Csv.Field
escapeFormula field = case ByteString.Char8.uncons field of
  Just (char, _) | char `elem` ['=', '+', '-', '@', '\t', '\r'] -> "'" <> field
  _ -> field
