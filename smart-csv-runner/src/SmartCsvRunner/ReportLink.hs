module SmartCsvRunner.ReportLink (
    ReportLinkStatus (..),
    statusToText,
) where

import RIO


data ReportLinkStatus
    = INITIALIZED
    | DONE
    | ERROR


statusToText :: ReportLinkStatus -> Text
statusToText INITIALIZED = "INITIALIZED"
statusToText DONE = "DONE"
statusToText ERROR = "ERROR"
