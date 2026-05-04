module Kronor.SmartCsv.ColumnConfig
  ( ColumnConfig,
    ColumnSettings (..),
    columnConfig,
    columnDataPath,
    columnDecimalPlaces,
    columnHeader,
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
    dataPath :: Maybe Text
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
parseColumnConfig _ = Map.empty

columnHeader :: Text -> ColumnConfig -> Text
columnHeader columnId colConfig = fromMaybe columnId $ do
  settings <- Map.lookup columnId colConfig
  settings.header

columnDecimalPlaces :: Text -> ColumnConfig -> Maybe Int
columnDecimalPlaces columnId colConfig = do
  settings <- Map.lookup columnId colConfig
  settings.decimalPlaces

columnDataPath :: Text -> ColumnConfig -> [Text]
columnDataPath columnId colConfig =
  maybe [] (filter (not . Text.null) . Text.splitOn ".") $ do
    settings <- Map.lookup columnId colConfig
    settings.dataPath

-- | Hardcoded column config, kept for test reference.
columnConfig :: ColumnConfig
columnConfig =
  Map.fromList
    [ ("payment_request_id", ColumnSettings Nothing (Just "Payment Request ID") Nothing),
      ("transaction_id", ColumnSettings Nothing (Just "Transaction ID") Nothing),
      ("merchant_id", ColumnSettings Nothing (Just "Merchant ID") Nothing),
      ("placed_at", ColumnSettings Nothing (Just "Placed At") Nothing),
      ("reference", ColumnSettings Nothing (Just "Reference") Nothing),
      ("payment_method", ColumnSettings Nothing (Just "Payment Method") Nothing),
      ("attempts", ColumnSettings Nothing (Just "Card Type") (Just "payment.cardType")),
      ("customer", ColumnSettings Nothing (Just "Customer Email") (Just "profile.email")),
      ("latest_status", ColumnSettings Nothing (Just "Latest Status") Nothing),
      ("currency", ColumnSettings Nothing (Just "Currency") Nothing),
      ("amount", ColumnSettings Nothing (Just "Amount") Nothing)
    ]
