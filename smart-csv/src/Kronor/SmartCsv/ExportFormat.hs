module Kronor.SmartCsv.ExportFormat
  ( ExportFormat (..),
    exportFormatContentType,
    exportFormatExtension,
    exportFormatFileKind,
    exportFormatText,
    parseExportFormat,
  )
where

import Data.Aeson qualified as Aeson
import RIO
import RIO.List (find)

-- | The file an export produces.
data ExportFormat = Csv | Xlsx
  deriving stock (Eq, Show, Enum, Bounded)

instance Aeson.FromJSON ExportFormat where
  parseJSON = Aeson.withText "ExportFormat" \text ->
    maybe (fail ("unknown format " <> show text <> ", expected \"csv\" or \"xlsx\"")) pure (parseExportFormat text)

instance Aeson.ToJSON ExportFormat where
  toJSON = Aeson.String . exportFormatText

-- | The format's name in requests and in the database.
exportFormatText :: ExportFormat -> Text
exportFormatText = \case
  Csv -> "csv"
  Xlsx -> "xlsx"

parseExportFormat :: Text -> Maybe ExportFormat
parseExportFormat text = find ((== text) . exportFormatText) [minBound .. maxBound]

exportFormatExtension :: ExportFormat -> Text
exportFormatExtension = exportFormatText

-- | The Content-Type the uploaded object gets.  CSV uploads never set one, so
-- they keep the bucket's default.
exportFormatContentType :: ExportFormat -> Maybe Text
exportFormatContentType = \case
  Csv -> Nothing
  Xlsx -> Just "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"

-- | How the completion email names the file.
exportFormatFileKind :: ExportFormat -> Text
exportFormatFileKind = \case
  Csv -> "CSV"
  Xlsx -> "Excel"
