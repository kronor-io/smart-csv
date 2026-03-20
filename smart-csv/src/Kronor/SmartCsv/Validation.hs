{-# LANGUAGE OrPatterns #-}

module Kronor.SmartCsv.Validation
  ( validateGraphqlQueryBody,
    validateQueryVariables,
  )
where

import Data.Aeson qualified as JSON
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as JSON
import Data.ByteString.Lazy qualified as LB
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty qualified as NE
import Data.Morpheus.Core (parseRequest)
import Data.Morpheus.Internal.Ext (Result (..))
import Data.Morpheus.Internal.Utils (IsMap (member))
import Data.Morpheus.Types.IO (GQLRequest (..))
import Data.Morpheus.Types.Internal.AST (ExecutableDocument (..), Operation (..))
import Data.Time
import RIO

validateGraphqlQueryBody :: Text -> Either (NonEmpty Text) ()
validateGraphqlQueryBody graphqlQueryBody =
  case parseRequest (GQLRequest {query = graphqlQueryBody, operationName = Nothing, variables = Nothing}) of
    Failure errs -> Left $ NE.map tshow errs
    Success ExecutableDocument {operation = Operation {operationSelection, operationArguments = operationArgs}} warnings ->
      let rootSelections = Foldable.toList operationSelection
       in case NE.nonEmpty warnings of
            Just ne -> Left (NE.map tshow ne)
            Nothing ->
              case rootSelections of
                [_] ->
                  if "rowLimit" `member` operationArgs
                    then
                      if "paginationCondition" `member` operationArgs
                        then Right ()
                        else Left $ NE.singleton "The query must define a paginationCondition variable."
                    else Left $ NE.singleton "The query must define rowLimit to limit the number of rows ."
                _ -> Left $ NE.singleton "The query must contain exactly one root field."

validateQueryVariables :: Key.Key -> Text -> Either Text JSON.Value
validateQueryVariables paginationKey queryVariablesText =
  case JSON.decode (LB.fromStrict (encodeUtf8 queryVariablesText)) of
    Nothing -> Left "Invalid Json"
    Just queryVariables ->
      let limits = utcTimeLimits paginationKey queryVariables
       in case (limits.lo, limits.hi) of
            (Just lo, Just hi) ->
              let durationThreshold = 33 * nominalDay -- "one" month
               in if diffUTCTime hi lo <= durationThreshold
                    then Right queryVariables
                    else Left ("The " <> Key.toText paginationKey <> " range is too wide.")
            _ ->
              Left
                ( mconcat
                    [ "The query must filter on ",
                      Key.toText paginationKey,
                      " in both directions, but found: ",
                      "hi: ",
                      tshow limits.hi,
                      ", lo: ",
                      tshow limits.lo,
                      "in: ",
                      queryVariablesText
                    ]
                )

data Limits = Limits
  { -- found in _gt or _gte
    lo :: Maybe UTCTime,
    -- found in _lt or _lte
    hi :: Maybe UTCTime
  }

joinMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
joinMaybe _ Nothing y = y
joinMaybe _ x Nothing = x
joinMaybe f (Just x) (Just y) = Just (f x y)

maxMaybe :: (Ord a) => Maybe a -> Maybe a -> Maybe a
maxMaybe = joinMaybe max

minMaybe :: (Ord a) => Maybe a -> Maybe a -> Maybe a
minMaybe = joinMaybe min

instance Semigroup Limits where
  (Limits lo1 hi1) <> (Limits lo2 hi2) =
    Limits (maxMaybe lo1 lo2) (minMaybe hi1 hi2)

instance Monoid Limits where
  mempty = Limits Nothing Nothing
  mappend = (<>)

utcTimeLimits :: Key.Key -> JSON.Value -> Limits
utcTimeLimits k = \case
  JSON.Object o -> maybe mempty andLimits (KeyMap.lookup "conditions" o)
  _ -> mempty
  where
    andLimits :: JSON.Value -> Limits
    andLimits = \case
      JSON.Object o ->
        mkLimits o
          <> ( case KeyMap.lookup "_and" o of
                 Just (JSON.Array as) -> foldMap andLimits as
                 (Just _; Nothing) -> mempty
             )
      _ -> mempty

    mkLimits :: KeyMap.KeyMap JSON.Value -> Limits
    mkLimits cond =
      Limits
        { lo = maxMaybe (lookupOp "_gte") (lookupOp "_gt"),
          hi = minMaybe (lookupOp "_lte") (lookupOp "_lt")
        }
      where
        lookupOp :: Key.Key -> Maybe UTCTime
        lookupOp op =
          KeyMap.lookup k cond
            >>= asObject
            >>= KeyMap.lookup op
            >>= JSON.parseMaybe JSON.parseJSON

    asObject :: JSON.Value -> Maybe (KeyMap.KeyMap JSON.Value)
    asObject (JSON.Object o) = Just o
    asObject _ = Nothing
