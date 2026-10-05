module Kronor.SmartCsv.ColumnConfig
  ( ColumnConfig,
    ColumnSettings (..),
    columnDataPath,
    columnDecimalPlaces,
    columnHeader,
    columnIsNumeric,
    parseColumnConfig,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.Aeson.Types qualified as Aeson.Types
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
    numeric :: Bool
  }
  deriving stock (Eq, Show)

-- | Parse a JSONB value into a column config map.
-- Each entry is keyed by the external column id / GraphQL alias.
-- Returns an empty map (pass-through) for non-object input.
parseColumnConfig :: Aeson.Value -> ColumnConfig
parseColumnConfig (Aeson.Object obj) =
  Map.fromList $ mapMaybe parseEntry (Aeson.KeyMap.toList obj)
  where
    parseEntry (k, value) =
      fmap ((Aeson.Key.toText k),) (Aeson.Types.parseMaybe parseSettings value)

    parseSettings =
      Aeson.withObject "ColumnSettings" $ \settings ->
        ColumnSettings
          <$> settings Aeson..:? "decimalPlaces"
          <*> settings Aeson..:? "header"
          <*> settings Aeson..:? "dataPath"
          <*> pure False
parseColumnConfig _ = Map.empty

columnHeader :: Text -> ColumnConfig -> Text
columnHeader columnId colConfig = fromMaybe columnId $ do
  settings <- Map.lookup columnId colConfig
  settings.header

columnDecimalPlaces :: Text -> ColumnConfig -> Maybe Int
columnDecimalPlaces columnId colConfig = do
  settings <- Map.lookup columnId colConfig
  settings.decimalPlaces

columnIsNumeric :: Text -> ColumnConfig -> Bool
columnIsNumeric columnId colConfig =
  maybe False (.numeric) (Map.lookup columnId colConfig)

columnDataPath :: Text -> ColumnConfig -> [Text]
columnDataPath columnId colConfig =
  maybe [] (filter (not . Text.null) . Text.splitOn ".") $ do
    settings <- Map.lookup columnId colConfig
    settings.dataPath
