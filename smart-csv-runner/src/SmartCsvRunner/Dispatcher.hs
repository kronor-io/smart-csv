{-# OPTIONS_GHC -Wno-orphans #-}

module SmartCsvRunner.Dispatcher (
    Meta (..),
    JobItem (..),
    SmartCsvJobType,
    withAddedContextFromAnnotations,
    withContextFromExceptionAnnotations,
) where

import Control.Exception.Annotated (AnnotatedException (..))
import Data.Aeson (FromJSON (..), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.Aeson.Lens (_String)
import Data.Annotation qualified as AnnException
import Data.Bifunctor qualified
import Data.Coerce (coerce)
import Data.Sum
import Data.Typeable (cast, typeOf)
import Data.Void
import JobSchemas.SmartGraphqlCsvGenerate (SmartGraphqlCsvGenerate)
import Kronor.Logger (RequestId)
import Kronor.Logger qualified
import RIO
import RIO.Map qualified as Map
import SmartCsvRunner.Job.SmartCsvEnv (SmartCsvEnv)
import SmartCsvRunner.Job.Type (
    JobPayloadAnnotation (..),
    JobProcessorF (..),
    produceJobTypes,
 )
import SmartCsvRunner.JobHandlers.Email qualified as Email
import SmartCsvRunner.JobHandlers.SmartGenerateCsv ()


data JobItem = JobItem Meta SmartCsvJobType


instance FromJSON JobItem where
    parseJSON = Aeson.withObject "JobItem" $ \o -> do
        meta <- do
            m <- o .:? "meta"
            case m of
                Just rawMeta -> case Map.lookup "requestId" rawMeta of
                    Just reqId ->
                        case reqId ^? _String of
                            Just rid -> return $ Meta (coerce rid) rawMeta
                            Nothing -> fail "expecting requestId to be a string in the Meta map."
                    Nothing -> fail "expecting a required 'requestId' key in the Meta map."
                Nothing -> fail "expecting a required 'meta' key in the job"
        job <- parseJSON (Aeson.Object o)
        return (JobItem meta job)


data Meta = Meta RequestId (Map Text Aeson.Value)


type SmartCsvJobType = SmartCsvJobType' Void


type SmartCsvJobType' =
    Sum
        [ Const SmartGraphqlCsvGenerate
        , Const Email.SendEmail
        ]


instance Aeson.FromJSON SmartCsvJobType where
    parseJSON = produceJobTypes


instance JobProcessorF SmartCsvEnv SmartCsvJobType' where
    processJobF = apply @(JobProcessorF SmartCsvEnv) processJobF
    closeJobF = apply @(JobProcessorF SmartCsvEnv) closeJobF
    expireJobF = apply @(JobProcessorF SmartCsvEnv) expireJobF


withAddedContextFromAnnotations ::
    Kronor.Logger.LoggerCapabilities env m =>
    [AnnException.Annotation] ->
    m b ->
    m b
withAddedContextFromAnnotations annotations action = do
    let context =
            mconcat $
                annotations
                    <&> \(AnnException.Annotation annotation) ->
                        case show (typeOf annotation) of
                            "JobPayloadAnnotation" -> maybe mempty payloadAnnotationToMap (cast annotation :: Maybe JobPayloadAnnotation)
                            _ -> mempty
    Kronor.Logger.withAddedContextMap context action


withContextFromExceptionAnnotations ::
    Kronor.Logger.LoggerCapabilities env m =>
    AnnotatedException e ->
    m b ->
    m b
withContextFromExceptionAnnotations (AnnotatedException annotations _) =
    withAddedContextFromAnnotations annotations


payloadAnnotationToMap :: JobPayloadAnnotation -> Map Text Aeson.Value
payloadAnnotationToMap payloadAnnotation =
    case Aeson.toJSON payloadAnnotation of
        Aeson.Object o ->
            o
                & Aeson.KeyMap.toList
                & fmap (Data.Bifunctor.first Aeson.Key.toText)
                & Map.fromList
        _ -> mempty
