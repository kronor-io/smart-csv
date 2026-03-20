{-# LANGUAGE UndecidableInstances #-}

module SmartCsvRunner.Job.Type where

import Control.Applicative
import Control.Exception.Annotated.UnliftIO qualified as AnnException
import Data.Aeson qualified
import Data.Aeson.Types qualified
import Data.Annotation qualified as AnnException
import Data.Kind
import Data.String.Interpolate (i, iii)
import Data.Sum
import Data.Sum.Populate
import GHC.Generics
import Kronor.Logger qualified
import Kronor.Tracer (Attribute, HasTraceTags (getTraceTags), addTopSpanTags)
import Kronor.Tracer qualified
import RIO hiding (Map)
import RIO.Text qualified as Text
import SmartCsvRunner.Job (Job, JobEnv (..))


instance (Generic x, Data.Aeson.GFromJSON Data.Aeson.Zero (Rep x)) => Producible Parser (Const x) where
    produce =
        fmap
            Const
            ( Parser
                ( Data.Aeson.genericParseJSON
                    Data.Aeson.defaultOptions
                        { Data.Aeson.Types.tagSingleConstructors = True
                        , Data.Aeson.Types.allNullaryToStringTag = False
                        }
                )
            )


newtype Parser a = Parser {runParser :: Data.Aeson.Types.Value -> Data.Aeson.Types.Parser a}
    deriving stock (Functor)


instance Applicative Parser where
    Parser p1 <*> Parser p2 = Parser $ \v -> p1 v <*> p2 v
    pure = Parser . const . pure


instance Alternative Parser where
    empty = Parser $ const empty
    Parser p1 <|> Parser p2 = Parser $ \v -> p1 v <|> p2 v


produceJobTypes :: Populate Parser fs => Data.Aeson.Value -> Data.Aeson.Types.Parser (Sum fs r)
produceJobTypes v = runParser populate v


subJobEnv :: (env -> jobEnv) -> Job jobEnv a -> Job env a
subJobEnv f jje = do
    jenv <- ask
    let env = jenv{jobEnv = f . jobEnv $ jenv}
    runRIO env jje


class JobProcessorF env f where
    processJobCallMeF :: f a -> Job env ()
    default processJobCallMeF :: ConstrName f => DeriveTraceTags f => f a -> Job env ()
    processJobCallMeF input =
        withTracingContext (jobName input) (deriveTraceTags input) do
            processJobF input


    processJobF :: f a -> Job env ()
    closeJobF :: f a -> Job env ()


    expireJobF :: f a -> Job env ()
    expireJobF = closeJobF


class JobProcessor env a where
    processJob :: a -> Job env ()
    closeJob :: a -> Job env ()


    expireJob :: a -> Job env ()
    expireJob = closeJob


instance
    ( JobProcessor env a
    , Generic a
    , ConstrName (Rep a)
    , DeriveTraceTags (Rep a)
    ) =>
    JobProcessorF env (Const a)
    where
    processJobF (Const a) = processJob a
    closeJobF (Const a) = closeJob a
    expireJobF (Const a) = expireJob a


withTracingContext ::
    Text ->
    [(Text, Attribute)] ->
    Job env a ->
    Job env a
withTracingContext name tags action = do
    AnnException.checkpoint (AnnException.toAnnotation (JobPayloadAnnotation context)) do
        Kronor.Logger.withAddedContextMap context do
            Kronor.Tracer.addTopSpanTags tags
            logDebug [iii|Start processing job '#{name}'|]
            a <- action
            logDebug [iii|Finished processing job '#{name}'|]
            return a
  where
    context = Kronor.Tracer.convertToLogAttributes tags


class DeriveTraceTags (f :: Type -> Type) where
    deriveTraceTags :: f x -> [(Text, Kronor.Tracer.Attribute)]


instance (Generic a, DeriveTraceTags (Rep a)) => DeriveTraceTags (Const a) where
    deriveTraceTags (Const a) = deriveTraceTags (from a)


instance DeriveTraceTags f => DeriveTraceTags (M1 i c f) where
    deriveTraceTags (M1 x) = deriveTraceTags x


instance HasTraceTags input => DeriveTraceTags (K1 i input) where
    deriveTraceTags (K1 input) = getTraceTags input


instance DeriveTraceTags U1 where
    deriveTraceTags U1 = []


instance Apply DeriveTraceTags fs => DeriveTraceTags (Sum fs) where
    deriveTraceTags = apply @DeriveTraceTags deriveTraceTags


data JobPayloadAnnotation where
    JobPayloadAnnotation :: (Data.Aeson.ToJSON a, Typeable a) => a -> JobPayloadAnnotation


instance Show JobPayloadAnnotation where
    show (JobPayloadAnnotation a) = [i|#{Data.Aeson.encode a})|]


instance Data.Aeson.ToJSON JobPayloadAnnotation where
    toJSON (JobPayloadAnnotation a) = Data.Aeson.toJSON a


data JobIdAnnotation where
    JobIdAnnotation :: Int64 -> JobIdAnnotation


instance Data.Aeson.ToJSON JobIdAnnotation where
    toJSON (JobIdAnnotation jobId) = Data.Aeson.toJSON jobId


class ConstrName (f :: Type -> Type) where
    getConstrName :: f x -> Text


instance (Generic a, ConstrName (Rep a)) => ConstrName (Const a) where
    getConstrName (Const a) = getConstrName (from a)


instance ConstrName f => ConstrName (D1 c f) where
    getConstrName (M1 x) = getConstrName x


instance Constructor c => ConstrName (C1 c f) where
    getConstrName x = Text.pack $ conName x


instance Apply ConstrName fs => ConstrName (Sum fs) where
    getConstrName = apply @ConstrName getConstrName


jobName :: ConstrName f => f a -> Text
jobName = getConstrName
