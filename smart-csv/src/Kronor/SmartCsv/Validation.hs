module Kronor.SmartCsv.Validation
  ( validateGraphqlQueryBody,
    validateGraphqlQueryBodyAndGetRootField,
    validateQueryVariables,
  )
where

import Data.Aeson qualified as JSON
import Data.ByteString.Lazy qualified as LB
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty qualified as NE
import Data.Morpheus.Core (parseRequest)
import Data.Morpheus.Internal.Ext (Result (..))
import Data.Morpheus.Internal.Utils (IsMap (member))
import Data.Morpheus.Types.IO (GQLRequest (..))
import Data.Morpheus.Types.Internal.AST (ExecutableDocument (..), Operation (..), RAW, Selection (..), unpackName)
import RIO

validateGraphqlQueryBody :: Text -> Either (NonEmpty Text) ()
validateGraphqlQueryBody = void . validateGraphqlQueryBodyAndGetRootField

validateGraphqlQueryBodyAndGetRootField :: Text -> Either (NonEmpty Text) Text
validateGraphqlQueryBodyAndGetRootField graphqlQueryBody =
  case parseRequest (GQLRequest {query = graphqlQueryBody, operationName = Nothing, variables = Nothing}) of
    Failure errs -> Left $ NE.map tshow errs
    Success ExecutableDocument {operation = Operation {operationSelection, operationArguments = operationArgs}} warnings ->
      let rootSelections = Foldable.toList operationSelection
       in case NE.nonEmpty warnings of
            Just ne -> Left (NE.map tshow ne)
            Nothing ->
              case rootSelections of
                [rootSelection] ->
                  case rootSelectionName rootSelection of
                    Nothing -> Left $ NE.singleton "The query root must be a field selection."
                    Just rootFieldName ->
                      if "rowLimit" `member` operationArgs
                        then
                          if "paginationCondition" `member` operationArgs
                            then Right rootFieldName
                            else Left $ NE.singleton "The query must define a paginationCondition variable."
                        else Left $ NE.singleton "The query must define rowLimit to limit the number of rows."
                _ -> Left $ NE.singleton "The query must contain exactly one root field."
  where
    rootSelectionName :: Selection RAW -> Maybe Text
    rootSelectionName Selection {selectionName} = Just (unpackName selectionName)
    rootSelectionName _ = Nothing

-- | Parse the GraphQL query variables JSON. Row and time limits now bound export
-- jobs at generation time, so the query's date range is no longer validated here.
-- The value must be a JSON object: pagination variables (rowLimit,
-- paginationCondition, injected orderBy) are merged into it, so a non-object would
-- silently drop them and fail the job later in a less obvious way.
validateQueryVariables :: Text -> Either Text JSON.Value
validateQueryVariables queryVariablesText =
  case JSON.decode (LB.fromStrict (encodeUtf8 queryVariablesText)) of
    Nothing -> Left "Invalid JSON"
    Just queryVariables@(JSON.Object _) -> Right queryVariables
    Just _ -> Left "Query variables must be a JSON object"
