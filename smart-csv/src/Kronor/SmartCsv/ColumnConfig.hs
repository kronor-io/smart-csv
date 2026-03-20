module Kronor.SmartCsv.ColumnConfig
  ( columnConfig,
    parseColumnConfig,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.Map.Strict qualified as Map
import RIO

-- | Parse a JSONB value into a column config map.
-- String values rename the column; null values suppress it.
-- Returns an empty map (pass-through) for non-object input.
parseColumnConfig :: Aeson.Value -> Map Text (Maybe Text)
parseColumnConfig (Aeson.Object obj) =
  Map.fromList $ map parseEntry (Aeson.KeyMap.toList obj)
  where
    parseEntry (k, Aeson.String v) = (Aeson.Key.toText k, Just v)
    parseEntry (k, _) = (Aeson.Key.toText k, Nothing)
parseColumnConfig _ = Map.empty

-- | Legacy hardcoded column config, kept for test reference.
columnConfig :: Map Text (Maybe Text)
columnConfig =
  Map.fromList
    [ ("paymentRequests_waitToken", Just "Payment Request ID"),
      ("paymentRequests_transactionId", Just "Transaction ID"),
      ("paymentRequests_merchantId", Just "Merchant ID"),
      ("paymentRequests_createdAt", Just "Placed At"),
      ("paymentRequests_reference", Just "Reference"),
      ("paymentRequests_paymentProvider", Just "Payment Method"),
      ("paymentRequests_attempts_cardType", Just "Card Type"),
      ("paymentRequests_customer_name", Just "Customer Name"),
      ("paymentRequests_customer_email", Just "Customer Email"),
      ("paymentRequests_currentStatus", Just "Latest Status"),
      ("paymentRequests_currency", Just "Currency"),
      ("paymentRequests_amount", Just "Amount")
    ]
