module Web.Telegram.Instances.Static where


import           Control.Applicative        ((<|>))
import           Data.Aeson
import           Data.Aeson.Types           (typeMismatch)
import qualified Data.HashMap.Strict        as HM
import           Data.Maybe                 (isJust)
import           Data.Monoid                ((<>))
import           Data.Text                  (unpack)

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

instance ToJSON ChatAction where
  toJSON Typing         = "typing"
  toJSON UploadPhoto    = "upload_photo"
  toJSON RecordVideo    = "record_video"
  toJSON UploadVideo    = "upload_video"
  toJSON RecordAudio    = "record_audio"
  toJSON UploadAudio    = "upload_audio"
  toJSON UploadDocument = "upload_document"
  toJSON FindLocation   = "find_location"


------------------------
-- FromJSON INSTANCES --
------------------------

instance FromJSON ChatType where
  parseJSON (String "private")    = pure Private
  parseJSON (String "group")      = pure Group
  parseJSON (String "supergroup") = pure Supergroup
  parseJSON (String "channel")    = pure Channel
  parseJSON (String wat) = fail $ "Wrong String \"" <> unpack wat <> "\" as ChatType"
  parseJSON wat = typeMismatch "ChatType" wat

instance FromJSON ParseMode where
  parseJSON (String "Markdown") = pure Markdown
  parseJSON (String "HTML")     = pure HTML
  parseJSON (String wat) = fail $ "Wrong String \"" <> unpack wat <> "\" as ParseMode"
  parseJSON wat = typeMismatch "ParseMode" wat

instance FromJSON VideoMIME where
  parseJSON (String "text/html") = pure TextHTML
  parseJSON (String "video/mp4") = pure VideoMP4
  parseJSON (String wat) = fail $ "Wrong String \"" <> unpack wat <> "\" as VideoMIME"
  parseJSON wat = typeMismatch "VideoMIME" wat

instance FromJSON DocumentMIME where
  parseJSON (String "application/pdf") = pure ApplicationPDF
  parseJSON (String "application/zip") = pure ApplicationZIP
  parseJSON (String wat) = fail $ "Wrong String \"" <> unpack wat <> "\" as DocumentMIME"
  parseJSON wat = typeMismatch "VideoMIME" wat

instance FromJSON ChatAction where
  parseJSON (String "typing")          = pure Typing
  parseJSON (String "upload_photo")    = pure UploadPhoto
  parseJSON (String "record_video")    = pure RecordVideo
  parseJSON (String "upload_video")    = pure UploadVideo
  parseJSON (String "record_audio")    = pure RecordAudio
  parseJSON (String "upload_audio")    = pure UploadAudio
  parseJSON (String "upload_document") = pure UploadDocument
  parseJSON (String "find_location")   = pure FindLocation
  parseJSON (String wat) = fail $ "Wrong String \"" <> unpack wat <> "\" as ChatAction"
  parseJSON wat = typeMismatch "ChatAction" wat
