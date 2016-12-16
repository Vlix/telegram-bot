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
  toJSON Markdown = String "Markdown"
  toJSON HTML     = String "HTML"

instance ToJSON VideoMIME where
  toJSON TextHTML = String "text/html"
  toJSON VideoMP4 = String "video/mp4"

instance ToJSON DocumentMIME where
  toJSON ApplicationPDF = String "application/pdf"
  toJSON ApplicationZIP = String "application/zip"

instance ToJSON ChatAction where
  toJSON Typing         = String "typing"
  toJSON UploadPhoto    = String "upload_photo"
  toJSON RecordVideo    = String "record_video"
  toJSON UploadVideo    = String "upload_video"
  toJSON RecordAudio    = String "record_audio"
  toJSON UploadAudio    = String "upload_audio"
  toJSON UploadDocument = String "upload_document"
  toJSON FindLocation   = String "find_location"

instance ToJSON ChatMemberStatus where
  toJSON Creator       = String "creator"
  toJSON Administrator = String "administrator" 
  toJSON Member        = String "member"
  toJSON MemberLeft    = String "left"
  toJSON MemberKicked  = String "kicked"

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

instance FromJSON ChatMemberStatus where
  parseJSON (String "creator")        = pure Creator
  parseJSON (String "administrator" ) = pure Administrator
  parseJSON (String "member")         = pure Member
  parseJSON (String "left")           = pure MemberLeft
  parseJSON (String "kicked")         = pure MemberKicked
  parseJSON (String wat) = fail $ "Wrong String \"" <> unpack wat <> "\" as ChatAction"
  parseJSON wat = typeMismatch "ChatMemberStatus" wat
