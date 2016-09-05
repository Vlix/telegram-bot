module Web.Telegram.Instances.Static where


import           Control.Applicative        ((<|>))
import           Data.Aeson
import           Data.Aeson.Types           (typeMismatch)
import           Data.Maybe                 (isJust)
import qualified Data.HashMap.Strict        as HM

import           Web.Telegram.Types.Static


----------------------
-- ToJSON INSTANCES --
----------------------

instance ToJSON ChatType where
  toJSON Private        = String "private"
  toJSON Group          = String "group"
  toJSON Supergroup     = String "supergroup"
  toJSON Channel        = String "channel"

instance ToJSON ParseMode where
  toJSON Markdown = "Markdown"
  toJSON HTML     = "HTML"

instance ToJSON VideoMIME where
  toJSON TextHTML = String "text/html"
  toJSON VideoMP4 = String "video/mp4"

instance ToJSON DocumentMIME where
  toJSON ApplicationPDF = String "application/pdf"
  toJSON ApplicationZIP = String "application/zip"


------------------------
-- FromJSON INSTANCES --
------------------------

instance FromJSON ChatType where
  parseJSON (String "private")    = pure Private
  parseJSON (String "group")      = pure Group
  parseJSON (String "supergroup") = pure Supergroup
  parseJSON (String "channel")    = pure Channel
  parseJSON (String _)            = fail "Incorrect ChatType"
  parseJSON wat = typeMismatch "ChatType" wat

instance FromJSON ParseMode where
  parseJSON (String "Markdown") = pure Markdown
  parseJSON (String "HTML")     = pure HTML
  parseJSON (String _)          = fail "Failed to parse ParseMode"
  parseJSON wat = typeMismatch "ParseMode" wat

instance FromJSON VideoMIME where
  parseJSON (String "text/html") = pure TextHTML
  parseJSON (String "video/mp4") = pure VideoMP4
  parseJSON (String _) = fail "Wrong VideoMIME String"
  parseJSON wat = typeMismatch "VideoMIME" wat

instance FromJSON DocumentMIME where
  parseJSON (String "application/pdf") = pure ApplicationPDF
  parseJSON (String "application/zip") = pure ApplicationZIP
  parseJSON (String _) = fail "Wrong DocumentMIME String"
  parseJSON wat = typeMismatch "VideoMIME" wat
