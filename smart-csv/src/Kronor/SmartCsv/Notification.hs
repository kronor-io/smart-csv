module Kronor.SmartCsv.Notification (
    CompletionEmail (..),
    EnqueueMeta (..),
    defaultEnqueueMeta,
    mkCompletionEmail,
) where

import RIO


data CompletionEmail
    = CompletionEmail
    { subject :: Text
    , htmlBody :: Text
    }
    deriving stock (Eq, Show)


data EnqueueMeta
    = EnqueueMeta
    { tag :: Text
    , caller :: Text
    , requestId :: Text
    , priority :: Int32
    }
    deriving stock (Eq, Show)


defaultEnqueueMeta :: EnqueueMeta
defaultEnqueueMeta =
    EnqueueMeta
        { tag = "SendEmail"
        , caller = "worker_sendCsvDoneEmail"
        , requestId = "portal_sendEmailNoReply"
        , priority = 5000
        }


-- | The email for a finished export; @fileKind@ names the file ("CSV", "Excel").
mkCompletionEmail :: Text -> Maybe Text -> CompletionEmail
mkCompletionEmail fileKind mUrl =
    case mUrl of
        Just url ->
            CompletionEmail
                { subject = "Your " <> fileKind <> " file is ready for download"
                , htmlBody =
                    mconcat
                        [ "<html>\n"
                        , "  <body>\n"
                        , "    <p>Your requested " <> fileKind <> " file is ready for download:</p>\n"
                        , "    <a href=\""
                        , url
                        , "\">Download " <> fileKind <> "</a>\n"
                        , "  </body>\n"
                        , "</html>\n"
                        ]
                }
        Nothing ->
            CompletionEmail
                { subject = "Your " <> fileKind <> " file contained no data"
                , htmlBody =
                    mconcat
                        [ "<html>\n"
                        , "  <body>\n"
                        , "    <p>Your requested " <> fileKind <> " file was not produced because it contained no data.</p>\n"
                        , "  </body>\n"
                        , "</html>\n"
                        ]
                }
