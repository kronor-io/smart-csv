{-# LANGUAGE ViewPatterns #-}

module Kronor.SmartCsv.Flatten
  ( csvify,
    gatherSelectionNames,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.Csv qualified as Csv
import Data.Map.Strict qualified as Map
import Data.Morpheus.Types.Internal.AST (RAW, Selection (..), SelectionContent (..), unpackName)
import Data.Vector qualified
import RIO

-- |
-- { "paymentRequests" : [], "otherRootQuery" : [] }
--
-- convert "paymentRequests" {"transactionId" : "PA3532115", "customer": { "email" : "amil@email.com", "name" : "Email Person"}, "attempts": [{"cardType": "VISA"}]}
--
-- to
--
-- { "paymentRequests_transactionId": "", "paymentRequests_customer_email": "", "paymentRequests_customer_name": "", "paymentRequests_attempts_cardType": "VISA" }
csvify :: Map Text (Maybe Text) -> Text -> Aeson.Value -> Map Text Csv.Field
csvify colConfig rootPrefix obj1@(Aeson.Object _) =
  go rootPrefix obj1
  where
    singletonOrEmpty :: Text -> Csv.Field -> Map Text Csv.Field
    singletonOrEmpty prefix v =
      case Map.lookup prefix colConfig of
        Just Nothing -> mempty
        Nothing -> Map.singleton prefix v
        Just (Just colName) -> Map.singleton colName v

    go prefix (Aeson.String t) = singletonOrEmpty prefix (Csv.toField t)
    go prefix (Aeson.Number sc) = singletonOrEmpty prefix (Csv.toField sc)
    go prefix (Aeson.Bool b) = singletonOrEmpty prefix (if b then "True" else "False")
    go prefix Aeson.Null = singletonOrEmpty prefix mempty
    go prefix (Aeson.Array array) =
      case array Data.Vector.!? 0 of
        Nothing -> mempty
        Just value -> go prefix value
    go prefix (Aeson.Object (Aeson.KeyMap.toMapText -> obj)) =
      Map.unions $ (\(k, v) -> go (prefix <> "_" <> k) v) <$> Map.toList obj
csvify _ _ _ = error "Expects an Aeson.Object, but instead received :"

gatherSelectionNames :: Selection RAW -> [Text]
gatherSelectionNames = go Nothing
  where
    go :: Maybe Text -> Selection RAW -> [Text]
    go _ InlineFragment {} = mempty
    go _ Spread {} = mempty
    go mPrefix sel@(Selection {}) =
      let sn = unpackName sel.selectionName
          prefixedName = maybe sn (<> "_" <> sn) mPrefix
       in case sel.selectionContent of
            SelectionSet sss -> concatMap (go (Just prefixedName)) (toList sss)
            _ -> pure prefixedName
