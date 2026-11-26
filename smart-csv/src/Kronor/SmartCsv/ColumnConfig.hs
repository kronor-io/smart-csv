module Kronor.SmartCsv.ColumnConfig
  ( ColumnConfig,
    ColumnSettings (..),
    OutputColumn (..),
    columnDataPath,
    columnDecimalPlaces,
    columnHeader,
    columnIsNumeric,
    defaultColumnSettings,
    outputColumns,
    parseColumnConfig,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.Aeson.Types qualified as Aeson.Types
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import RIO

type ColumnConfig = Map Text ColumnSettings

data ColumnSettings = ColumnSettings
  { decimalPlaces :: Maybe Int,
    header :: Maybe Text,
    dataPath :: Maybe Text,
    -- | The column prints a numeric scalar.  Not read from the request: it comes
    -- from the GraphQL schema (see 'Kronor.SmartCsv.Schema'), because Hasura can
    -- send numbers as strings.
    numeric :: Bool,
    -- | Print the field as several columns, one per entry, in this order.  Each
    -- entry has its own 'header', 'dataPath' and 'decimalPlaces'; the field's own
    -- are not used.  An entry's own 'columns' is never set.
    columns :: Maybe (NonEmpty ColumnSettings)
  }
  deriving stock (Eq, Show)

-- | Settings that change nothing: the field's name as header, its value as is.
defaultColumnSettings :: ColumnSettings
defaultColumnSettings = ColumnSettings Nothing Nothing Nothing False Nothing

-- | One column of the file.  A selected field gives one, or one per entry of its
-- 'columns'.
data OutputColumn = OutputColumn
  { -- | The selected field (or alias) the column's value comes from.
    columnId :: Text,
    header :: Text,
    dataPath :: [Text],
    decimalPlaces :: Maybe Int,
    numeric :: Bool
  }
  deriving stock (Eq, Show)

-- | The columns a selected field prints, in file order.  An entry of 'columns'
-- without a header is headed by the field and its data path, @customer.email@.
outputColumns :: ColumnConfig -> Text -> NonEmpty OutputColumn
outputColumns colConfig columnId =
  case Map.lookup columnId colConfig of
    Just ColumnSettings {columns = Just entries} -> entryColumn <$> entries
    mSettings -> pure (fieldColumn (fromMaybe defaultColumnSettings mSettings))
  where
    fieldColumn settings =
      OutputColumn
        { columnId,
          header = fromMaybe columnId settings.header,
          dataPath = splitDataPath settings.dataPath,
          decimalPlaces = settings.decimalPlaces,
          numeric = settings.numeric
        }

    entryColumn entry =
      OutputColumn
        { columnId,
          header = fromMaybe (Text.intercalate "." (columnId : splitDataPath entry.dataPath)) entry.header,
          dataPath = splitDataPath entry.dataPath,
          decimalPlaces = entry.decimalPlaces,
          numeric = entry.numeric
        }

-- | Parse a JSONB value into a column config map.
-- Each entry is keyed by the external column id / GraphQL alias.
-- Returns an empty map (pass-through) for non-object input.  An entry that does
-- not parse is left out, so its field prints as if it had no entry.
parseColumnConfig :: Aeson.Value -> ColumnConfig
parseColumnConfig (Aeson.Object obj) =
  Map.fromList $ mapMaybe parseEntry (Aeson.KeyMap.toList obj)
  where
    parseEntry (k, value) =
      fmap ((Aeson.Key.toText k),) (Aeson.Types.parseMaybe parseSettings value)

    parseSettings =
      Aeson.withObject "ColumnSettings" $ \settings -> do
        ownSettings <- parseOwnSettings settings
        mEntries <- settings Aeson..:? "columns"
        entries <- for mEntries $ \case
          [] -> fail "columns must list at least one column"
          entry : rest -> traverse (Aeson.withObject "column" parseOwnSettings) (entry :| rest)
        pure ownSettings {columns = entries}

    parseOwnSettings settings =
      ColumnSettings
        <$> settings Aeson..:? "decimalPlaces"
        <*> settings Aeson..:? "header"
        <*> settings Aeson..:? "dataPath"
        <*> pure False
        <*> pure Nothing
parseColumnConfig _ = Map.empty

-- | The header of the field's single column, or of its first one when it has
-- 'columns'.
columnHeader :: Text -> ColumnConfig -> Text
columnHeader columnId colConfig = (NonEmpty.head (outputColumns colConfig columnId)).header

columnDecimalPlaces :: Text -> ColumnConfig -> Maybe Int
columnDecimalPlaces columnId colConfig = do
  settings <- Map.lookup columnId colConfig
  settings.decimalPlaces

columnIsNumeric :: Text -> ColumnConfig -> Bool
columnIsNumeric columnId colConfig =
  maybe False (.numeric) (Map.lookup columnId colConfig)

columnDataPath :: Text -> ColumnConfig -> [Text]
columnDataPath columnId colConfig =
  splitDataPath $ do
    settings <- Map.lookup columnId colConfig
    settings.dataPath

splitDataPath :: Maybe Text -> [Text]
splitDataPath = maybe [] (filter (not . Text.null) . Text.splitOn ".")
