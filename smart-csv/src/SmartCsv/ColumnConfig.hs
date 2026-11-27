module SmartCsv.ColumnConfig
  ( ColumnConfig,
    ColumnSettings (..),
    ColumnKey (..),
    OutputColumn (..),
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
    -- from the GraphQL schema (see 'SmartCsv.Schema'), because Hasura can
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

-- | Which column of the file a cell belongs to: the selected field (or alias),
-- and for a field with 'columns' the entry's position.  Unlike a header it is
-- unique, so cells are keyed by it and two columns that share a header keep
-- their own values.
data ColumnKey = ColumnKey Text (Maybe Int)
  deriving stock (Eq, Ord, Show)

-- | One column of the file.  A selected field gives one, or one per entry of its
-- 'columns'.
data OutputColumn = OutputColumn
  { key :: ColumnKey,
    -- | The selected field (or alias) the column's value comes from.
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
    Just ColumnSettings {columns = Just entries} -> NonEmpty.zipWith entryColumn (0 :| [1 ..]) entries
    mSettings -> pure (fieldColumn (fromMaybe defaultColumnSettings mSettings))
  where
    fieldColumn settings =
      OutputColumn
        { key = ColumnKey columnId Nothing,
          columnId,
          header = fromMaybe columnId settings.header,
          dataPath = splitDataPath settings.dataPath,
          decimalPlaces = settings.decimalPlaces,
          numeric = settings.numeric
        }

    entryColumn index entry =
      OutputColumn
        { key = ColumnKey columnId (Just index),
          columnId,
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

    -- A field with 'columns' prints only its entries, so its own settings are not
    -- read: one that does not parse cannot drop the entries.
    parseSettings =
      Aeson.withObject "ColumnSettings" $ \settings -> do
        mEntries <- settings Aeson..:? "columns"
        case mEntries of
          Nothing -> parseOwnSettings settings
          Just [] -> fail "columns must list at least one column"
          Just (entry : rest) -> do
            entries <- traverse (Aeson.withObject "column" parseOwnSettings) (entry :| rest)
            pure defaultColumnSettings {columns = Just entries}

    parseOwnSettings settings =
      ColumnSettings
        <$> settings Aeson..:? "decimalPlaces"
        <*> settings Aeson..:? "header"
        <*> settings Aeson..:? "dataPath"
        <*> pure False
        <*> pure Nothing
parseColumnConfig _ = Map.empty

splitDataPath :: Maybe Text -> [Text]
splitDataPath = maybe [] (filter (not . Text.null) . Text.splitOn ".")
