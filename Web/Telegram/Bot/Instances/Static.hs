{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Telegram.Bot.Instances.Static where


import           Data.Aeson
import           Data.Monoid                ((<>))
import           Data.Text                  (unpack)

import           Web.Telegram.Bot.Types.Static


----------------------
-- ToJSON INSTANCES --
----------------------

instance ToJSON ChatType where
  toJSON typ = String t
   where
    t = case typ of
      Private    -> "private"
      Group      -> "group"
      Supergroup -> "supergroup"
      Channel    -> "channel"

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
  toJSON typ = String t
   where
    t = case typ of
      Typing         -> "typing"
      UploadPhoto    -> "upload_photo"
      RecordVideo    -> "record_video"
      UploadVideo    -> "upload_video"
      RecordAudio    -> "record_audio"
      UploadAudio    -> "upload_audio"
      UploadDocument -> "upload_document"
      FindLocation   -> "find_location"

instance ToJSON ChatMemberStatus where
  toJSON typ = String t
   where
    t = case typ of
      Creator       -> "creator"
      Administrator -> "administrator" 
      Member        -> "member"
      MemberLeft    -> "left"
      MemberKicked  -> "kicked"

instance ToJSON UpdateType where
  toJSON typ = String t
   where
    t = case typ of
      MESSAGE            -> "message"
      EDITEDMESSAGE      -> "edited_message"
      CHANNELPOST        -> "channel_post"
      EDITEDCHANNELPOST  -> "edited_channel_post"
      INLINEQUERY        -> "inline_query"
      CHOSENINLINERESULT -> "chosen_inline_result"
      CALLBACKQUERY      -> "callback_query"

------------------------
-- FromJSON INSTANCES --
------------------------

instance FromJSON ChatType where
  parseJSON = withText "ChatType" $ \s ->
    case s of
      "private"    -> pure Private
      "group"      -> pure Group
      "supergroup" -> pure Supergroup
      "channel"    -> pure Channel
      wat          -> fail $ "Wrong String \"" <> unpack wat <> "\" as ChatType"

instance FromJSON ParseMode where
  parseJSON = withText "ParseMode" $ \s ->
    case s of
      "Markdown" -> pure Markdown
      "HTML"     -> pure HTML
      wat        -> fail $ "Wrong String \"" <> unpack wat <> "\" as ParseMode"

instance FromJSON VideoMIME where
  parseJSON = withText "VideoMIME" $ \s ->
    case s of
      "text/html" -> pure TextHTML
      "video/mp4" -> pure VideoMP4
      wat         -> fail $ "Wrong String \"" <> unpack wat <> "\" as VideoMIME"

instance FromJSON DocumentMIME where
  parseJSON = withText "DocumentMIME" $ \s ->
    case s of
      "application/pdf" -> pure ApplicationPDF
      "application/zip" -> pure ApplicationZIP
      wat               -> fail $ "Wrong String \"" <> unpack wat <> "\" as DocumentMIME"

instance FromJSON ChatAction where
  parseJSON = withText "ChatAction" $ \s ->
    case s of
      "typing"          -> pure Typing
      "upload_photo"    -> pure UploadPhoto
      "record_video"    -> pure RecordVideo
      "upload_video"    -> pure UploadVideo
      "record_audio"    -> pure RecordAudio
      "upload_audio"    -> pure UploadAudio
      "upload_document" -> pure UploadDocument
      "find_location"   -> pure FindLocation
      wat               -> fail $ "Wrong String \"" <> unpack wat <> "\" as ChatAction"

instance FromJSON ChatMemberStatus where
  parseJSON = withText "ChatMemberStatus" $ \s ->
    case s of
      "creator"       -> pure Creator
      "administrator" -> pure Administrator
      "member"        -> pure Member
      "left"          -> pure MemberLeft
      "kicked"        -> pure MemberKicked
      wat             -> fail $ "Wrong String \"" <> unpack wat <> "\" as ChatAction"

instance FromJSON UpdateType where
  parseJSON = withText "UpdateType" $ \s ->
    case s of
      "message"              -> pure MESSAGE
      "edited_message"       -> pure EDITEDMESSAGE
      "channel_post"         -> pure CHANNELPOST
      "edited_channel_post"  -> pure EDITEDCHANNELPOST
      "inline_query"         -> pure INLINEQUERY
      "chosen_inline_result" -> pure CHOSENINLINERESULT
      "callback_query"       -> pure CALLBACKQUERY
      wat                    -> fail $ "Wrong String \"" <> unpack wat <> "\" as UpdateType"
