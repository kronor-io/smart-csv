{-# OPTIONS_GHC -Wno-orphans #-}

module SmartCsvRunner.MultipartUpload (ReportGenerationRow (..), ProcessingError (..), multiPartUploadFromPagination) where

import Amazonka qualified as Aws
import Amazonka.S3 qualified as Aws
import Amazonka.S3 qualified as Aws.S3
import Amazonka.S3.Lens qualified as Aws.S3
import Control.Lens.Operators ((#))
import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.Binary.Builder qualified
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified
import Data.Csv qualified
import Data.Csv qualified as Csv
import Data.Csv.Builder qualified
import Data.List.NonEmpty qualified as NonEmpty
import Kronor.Tracer qualified
import OpenTelemetry.Context (Context)
import OpenTelemetry.Context.ThreadLocal (adjustContext, getContext)
import RIO
import RIO.Orphans ()
import RIO.Time
import RIO.Vector qualified as Vector
import RIO.Vector.Partial qualified as Vector
import SmartCsvRunner.AWS (HasAwsEnv (..))
import SmartCsvRunner.Job (Job)
import SmartCsvRunner.ReportLink (ReportLinkStatus (..))
import Streamly.Data.Fold qualified
import Streamly.Data.Stream.Prelude qualified as Streamly
import Streamly.Data.Unfold qualified
import Streamly.Internal.Data.Fold (foldtM')
import Streamly.Internal.Data.Fold qualified as Streamly.Data.Fold
import Streamly.Internal.Data.Unfold qualified as Streamly.Data.Unfold


data ProcessingError = EmptySet | InternalError Text | AwsError Aws.Error
    deriving stock (Show)


data ReportGenerationRow cursor = ReportGenerationRow
    { reportPath :: Text
    , status :: ReportLinkStatus
    , bucketName :: Text
    , lastPaginationKey :: cursor
    , uploadId :: Maybe Text
    , lastUploadedPart :: Maybe Int32
    , partEntity :: ByteString
    , startDate :: UTCTime
    , endDate :: UTCTime
    , count :: Int
    }


multiPartUploadFromPagination ::
    forall a cursor env result.
    Csv.ToNamedRecord a =>
    -- generation progress tracker
    ReportGenerationRow (Maybe cursor) ->
    -- how to fetch transactions using the pagination cursor
    (Maybe cursor -> Job env (Vector a)) ->
    -- how to get the pagination cursor
    (a -> cursor) ->
    -- header line for the CSV
    Vector Csv.Name ->
    -- fetch part entities
    (Job env (Vector ByteString)) ->
    -- update progress in the table to retry if failed
    (ReportGenerationRow cursor -> Job env ()) ->
    -- success handler
    (ReportGenerationRow cursor -> Job env result) ->
    Job env (Either ProcessingError result)
multiPartUploadFromPagination
    oldReportGenerationRow
    fetchPage
    getCursor
    header
    fetchPartEntities
    updateProgress
    onSuccess =
        do
            eMultiUpload <- runExceptT do
                (rgr, uploadId) <- ExceptT $ createUpload oldReportGenerationRow
                (rgr', uploadId') <- ExceptT $ uploadParts rgr fetchPage getCursor header uploadId updateProgress
                ExceptT $ completeUpload fetchPartEntities onSuccess rgr' uploadId'

            case eMultiUpload of
                Left err -> pure $ Left err
                ok -> pure ok


uploadParts ::
    forall a cursor env.
    Csv.ToNamedRecord a =>
    -- generation progress tracker
    ReportGenerationRow (Maybe cursor) ->
    -- how to fetch transactions using the pagination cursor
    (Maybe cursor -> Job env (Vector a)) ->
    -- how to get the pagination cursor
    (a -> cursor) ->
    -- header line for the CSV
    Vector Csv.Name ->
    -- Upload Id
    Text ->
    -- update progress in the table to retry if failed
    (ReportGenerationRow cursor -> Job env ()) ->
    Job env (Either ProcessingError (ReportGenerationRow cursor, Text))
uploadParts
    rgr
    fetchPage
    getCursor
    header
    uploadId
    updateProgress =
        do
            mRgr <- handleTransaction <&> fmap (second ((,uploadId)))

            case mRgr of
                Nothing -> pure $ Left EmptySet
                Just (Left err) -> pure $ Left err
                Just uploadState -> pure uploadState
      where
        fetchTransactions ::
            Context ->
            Maybe cursor ->
            Job env (Maybe ((Vector a, cursor), Maybe cursor))
        fetchTransactions telemetryContext mCursor = do
            adjustContext (const telemetryContext)
            transactions <- fetchPage mCursor
            if Vector.null transactions
                then return Nothing
                else do
                    let cursor = getCursor (Vector.last transactions)
                    return (Just ((transactions, cursor), Just cursor))

        handleTransaction :: Job env (Maybe (Either ProcessingError (ReportGenerationRow cursor)))
        handleTransaction =
            let baseOffset :: Int32 = (fromMaybe 0 rgr.lastUploadedPart) + 1
             in do
                    telemetryContext <- getContext
                    Streamly.Data.Unfold.unfoldrM (fetchTransactions telemetryContext)
                        -- a single chunk may contain multiple pages
                        -- Invariant: A page is never split across a chunk
                        & Streamly.Data.Unfold.foldMany
                            ( Streamly.Data.Fold.teeWithFst
                                (,)
                                (writeAndChunkBoundary (Data.Csv.encodeByNameWith Data.Csv.defaultEncodeOptions{Csv.encIncludeHeader = False} header . Vector.toList . fst) 5_242_880)
                                (Streamly.Data.Fold.foldl' (\acc v -> acc `seq` (acc + (Vector.length (fst v)))) 0)
                            )
                        & Streamly.Data.Unfold.takeWhile (isJust . fst)
                        & fmap
                            ( \(mChunk, nRows) ->
                                case mChunk of
                                    Just chunk -> (chunk, nRows)
                                    Nothing -> error "unreachable: chunk is guarded by takeWhile (isJust . fst)"
                            )
                        & Streamly.Data.Unfold.lmap fst
                        & Streamly.Data.Unfold.zipWith
                            (,)
                            (Streamly.Data.Unfold.lmap snd (Streamly.Data.Unfold.enumerateFrom))
                        & flip Streamly.unfold (rgr.lastPaginationKey, baseOffset)
                        -- we assume that generation is deterministic with given pagination cursor, so we assume
                        -- a new pagination cursor will always start a new chunk and will be paired with the
                        -- next part number
                        & Streamly.parMapM
                            (Streamly.maxThreads 3 . Streamly.ordered True)
                            ( \(partNumber, ((lbs, cursorObject), nRows)) -> do
                                adjustContext (const telemetryContext)
                                eresp <-
                                    uploadPart
                                        (Aws._BucketName # rgr.bucketName)
                                        (Aws._ObjectKey # rgr.reportPath)
                                        partNumber
                                        ( Aws.toBody
                                            ( if partNumber == 1
                                                then
                                                    Data.Binary.Builder.toLazyByteString (Data.Csv.Builder.encodeHeader header)
                                                        <> lbs
                                                else lbs
                                            )
                                        )
                                pure
                                    ( ( \etag ->
                                            rgr
                                                { lastUploadedPart = Just partNumber
                                                , lastPaginationKey = snd cursorObject
                                                , partEntity = etag
                                                , count = nRows
                                                }
                                      )
                                        <$> eresp
                                    )
                            )
                        & Streamly.fold
                            ( Streamly.Data.Fold.takeEndBy
                                isLeft
                                do
                                    Streamly.Data.Fold.teeWith
                                        do (\_ x -> x)
                                        do
                                            Streamly.Data.Fold.foldlM'
                                                ( \_ eErr ->
                                                    case eErr of
                                                        Right res -> updateProgress res
                                                        _ -> pure ()
                                                )
                                                (pure ())
                                        do Streamly.Data.Fold.latest
                            )

        uploadPart ::
            Aws.BucketName ->
            Aws.ObjectKey ->
            Int32 ->
            Aws.RequestBody ->
            Job env (Either ProcessingError ByteString)
        uploadPart bn objk partNumber rb = do
            let up =
                    Aws.S3.newUploadPart
                        bn
                        objk
                        (fromIntegral @Int32 partNumber)
                        uploadId
                        rb

            awsEnv <- asks (^. getAwsEnvL)
            eRes <- Kronor.Tracer.withClientTrace "S3 UploadPart" do
                Aws.runResourceT (Aws.sendEither awsEnv up)
            case eRes of
                Left err -> pure $ Left $ AwsError err
                Right upr -> case upr ^. Aws.S3.uploadPartResponse_eTag of
                    Nothing -> pure $ Left $ InternalError "Could not find Etag in part upload"
                    Just et -> pure $ Right (et ^. Aws.S3._ETag)


data CurrentEncodedState a = NothingEncoded | CurrentEncodedState Builder.Builder Int64 a


writeAndChunkBoundary :: forall m a. MonadIO m => (a -> LByteString) -> Int64 -> Streamly.Data.Fold.Fold m a (Maybe (LByteString, a))
writeAndChunkBoundary encoder byteSize = foldtM' step initial extract
  where
    initial = pure (Streamly.Data.Fold.Partial NothingEncoded)
    step NothingEncoded a = do
        let newBuf = encoder a
        pure $ Streamly.Data.Fold.Partial ((CurrentEncodedState (Builder.lazyByteString newBuf) (Data.ByteString.Lazy.length newBuf) a))
    step (CurrentEncodedState encodedBuffer currentSize _) a = pure do
        let newBuf = encoder a
            updatedBuffer = encodedBuffer <> Builder.lazyByteString newBuf
            newSize = currentSize + Data.ByteString.Lazy.length newBuf
        if newSize >= byteSize
            then Streamly.Data.Fold.Done (Just (Builder.toLazyByteString updatedBuffer, a))
            else Streamly.Data.Fold.Partial (CurrentEncodedState updatedBuffer newSize a)
    extract NothingEncoded = pure Nothing
    extract (CurrentEncodedState updatedBuffer _ a) = pure (Just (Builder.toLazyByteString updatedBuffer, a))


completeUpload :: (Job env (Vector ByteString)) -> (ReportGenerationRow cursor -> Job env a) -> ReportGenerationRow cursor -> Text -> Job env (Either ProcessingError a)
completeUpload fetchPartEntities onSuccess rgr uploadId = do
    etags :: Vector ByteString <- fetchPartEntities
    let partEtags = (\(pn, et) -> Aws.newCompletedPart pn (Aws._ETag # et)) <$> zip [1 ..] (Vector.toList etags)
        completedPartUpload = Just $ Aws.newCompletedMultipartUpload & Aws.S3.completedMultipartUpload_parts .~ (Just (NonEmpty.fromList partEtags))

    eUploadErr <-
        completeUpload'
            (Aws._BucketName # rgr.bucketName)
            (Aws._ObjectKey # rgr.reportPath)
            completedPartUpload

    case eUploadErr of
        Left uploadErr -> pure (Left $ AwsError uploadErr)
        Right _ -> Right <$> onSuccess rgr
  where
    completeUpload' :: Aws.BucketName -> Aws.ObjectKey -> Maybe Aws.CompletedMultipartUpload -> Job env (Either Aws.Error ())
    completeUpload' bn objk completedmultipart = do
        let compu =
                Aws.S3.newCompleteMultipartUpload
                    bn
                    objk
                    uploadId
                    & Aws.S3.completeMultipartUpload_multipartUpload
                    .~ completedmultipart
        awsEnv <- asks (^. getAwsEnvL)
        (() <$)
            <$> Kronor.Tracer.withClientTrace "S3 CompleteMultipartUpload" do
                Aws.runResourceT (Aws.sendEither awsEnv compu)


createUpload :: ReportGenerationRow (Maybe cursor) -> Job env (Either ProcessingError (ReportGenerationRow (Maybe cursor), Text))
createUpload rgr = do
    case rgr.uploadId of
        Just uid -> pure $ Right (rgr{uploadId = Just uid}, uid)
        Nothing -> do
            let cmpu =
                    Aws.newCreateMultipartUpload
                        (Aws._BucketName # rgr.bucketName)
                        (Aws._ObjectKey # rgr.reportPath)

            awsEnv <- asks (^. getAwsEnvL)
            eCreateMultiPartResponse <-
                Kronor.Tracer.withClientTrace "S3 CreateMultipartUpload" do
                    Aws.runResourceT $ Aws.sendEither awsEnv cmpu

            bitraverse
                do
                    \err -> do
                        pure (AwsError err)
                do
                    \cmpur -> do
                        let uploadId = cmpur ^. Aws.S3.createMultipartUploadResponse_uploadId
                        pure
                            ( rgr
                                { uploadId = Just uploadId
                                , lastPaginationKey = Nothing
                                , lastUploadedPart = Nothing
                                }
                            , uploadId
                            )
                do eCreateMultiPartResponse
