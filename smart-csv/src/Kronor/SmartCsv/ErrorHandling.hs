{-# LANGUAGE OverloadedStrings #-}

module Kronor.SmartCsv.ErrorHandling (
    ErrorAction (..),
    classifyResponseError,
    classifyCursorError,
    classifyTokenClaimsError,
    classifyJsonDecodeError,
) where

import Kronor.SmartCsv.Pagination (CursorError (..))
import Kronor.SmartCsv.Query (ResponseError (..))
import Kronor.SmartCsv.TokenClaims (TokenClaimsError (..))
import RIO


-- | ErrorAction represents the decision made for handling an error.
-- Retry means attempt the operation again after a delay.
-- Giveup means terminate the job with failure.
data ErrorAction
    = Retry Text
    | Giveup Text
    deriving stock (Eq, Show)


-- | Classify GraphQL response errors.
-- ResponseContainsError is retryable (GraphQL error, might be transient).
-- ResponseMissingData and ResponseMissingRootData are non-retryable (structural issues).
classifyResponseError :: ResponseError -> ErrorAction
classifyResponseError (ResponseContainsError errMsg) =
    Retry errMsg
classifyResponseError ResponseMissingData =
    Giveup "GraphQL response does not contain data."
classifyResponseError ResponseMissingRootData =
    Giveup "GraphQL response does not contain expected root query data."


-- | Classify pagination cursor extraction errors.
-- CursorKeyDeleted indicates misconfiguration and is non-retryable.
-- CursorValueMissing indicates missing column in data (non-retryable).
classifyCursorError :: CursorError -> ErrorAction
classifyCursorError (CursorKeyDeleted colName) =
    Giveup $ "Cursor key is marked to be deleted: " <> colName
classifyCursorError (CursorValueMissing colName) =
    Giveup $ "If you see this that means you need to look in columnConfig for missing pagination key column mapping (" <> colName <> ")"


-- | Classify token claims parsing errors.
-- TokenClaimsNotObject indicates malformed JWT claims (non-retryable).
classifyTokenClaimsError :: TokenClaimsError -> ErrorAction
classifyTokenClaimsError TokenClaimsNotObject =
    Giveup "Token claims are not in a valid json format."


-- | Classify JSON decode errors (e.g., from response parsing).
-- JSON decode errors are treated as retryable (likely transient network/parsing issues).
classifyJsonDecodeError :: String -> ErrorAction
classifyJsonDecodeError err =
    Retry (fromString err)
