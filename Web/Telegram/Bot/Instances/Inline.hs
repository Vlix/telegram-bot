{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Telegram.Bot.Instances.Inline where


import           Control.Applicative        ((<|>))
import           Data.Aeson
import           Data.Text                  (unpack)
import qualified Data.Aeson.KeyMap          as KM

import           Web.Telegram.Bot.Types.Inline
import           Web.Telegram.Bot.Types.Static
import           Web.Telegram.Bot.Instances.Basic()
import           Web.Telegram.Bot.Instances.Static()

----------------------
-- ToJSON INSTANCES --
----------------------

instance ToJSON InlineQuery where
  toJSON InlineQuery{..} =
    object' [ "id"       .=! query_id
            , "from"     .=! query_from
            , "query"    .=! query_query
            , "offset"   .=! query_offset
            , "location" .=!! query_location
            ]

instance ToJSON ChosenInlineResult where
  toJSON ChosenInlineResult{..} =
    object' [ "result_id"         .=! chosen_result_id
            , "from"              .=! chosen_from
            , "query"             .=! chosen_query
            , "inline_message_id" .=!! chosen_inline_message_id
            , "location"          .=!! chosen_location
            ]

instance ToJSON InputMessageContent where
  toJSON InputTextMessageContent{..} =
    object' [ "message_text" .=! input_message_text
            , "parse_mode"   .=!! parse_mode
            , mBool "disable_web_page_preview" False disable_web_page_preview
            ]
  toJSON InputVenueMessageContent{..} =
    object' [ "latitude"      .=! latitude
            , "longitude"     .=! longitude
            , "title"         .=! title
            , "address"       .=! address
            , "foursquare_id" .=!! foursquare_id
            ]
  toJSON InputLocationMessageContent{..} =
    object [ "latitude"  .= latitude
           , "longitude" .= longitude
           ]
  toJSON InputContactMessageContent{..} =
    object' [ "phone_number" .=! phone_number
            , "first_name"   .=! first_name
            , "last_name"    .=!! last_name
            ]

instance ToJSON InlineKeyboardMarkup where
  toJSON (IKM buttons) =
    object [ "inline_keyboard" .= buttons ]

instance ToJSON InlineQueryResult where
  toJSON InlineQueryResultArticle{..} =
    object' [ "type"                  .=! String "article"
            , "id"                    .=! iqr_id
            , "title"                 .=! iqr_title
            , "input_message_content" .=! iqr_article_input_message_content
            , "reply_markup"          .=!! iqr_reply_markup
            , "url"                   .=!! iqr_url
            , mBool "hide_url" False iqr_hide_url
            , "description"           .=!! iqr_description
            , "thumb_url"             .=!! iqr_thumb_url
            , "thumb_width"           .=!! iqr_thumb_width
            , "thumb_height"          .=!! iqr_thumb_height
            ]
  toJSON InlineQueryResultPhoto{..} =
    object' [ "type"                  .=! String "photo"
            , "id"                    .=! iqr_id
            , "photo_url"             .=! iqr_photo_url
            , "thumb_url"             .=! iqr_photo_thumb_url
            , "photo_width"           .=!! iqr_photo_width
            , "photo_height"          .=!! iqr_photo_height
            , "title"                 .=!! iqr_photo_title
            , "description"           .=!! iqr_description
            , "caption"               .=!! iqr_caption
            , "input_message_content" .=!! iqr_reply_markup
            , "reply_markup"          .=!! iqr_input_message_content
            ]
  toJSON InlineQueryResultGif{..} =
    object' [ "type"                  .=! String "gif"
            , "id"                    .=! iqr_id
            , "gif_url"               .=! iqr_gif_url
            , "gif_width"             .=!! iqr_gif_width
            , "gif_height"            .=!! iqr_gif_height
            , "thumb_url"             .=! iqr_gif_thumb_url
            , "title"                 .=!! iqr_gif_title
            , "caption"               .=!! iqr_caption
            , "input_message_content" .=!! iqr_reply_markup
            , "reply_markup"          .=!! iqr_input_message_content
            ]
  toJSON InlineQueryResultMpeg4Gif{..} =
    object' [ "type"                  .=! String "mpeg4_gif"
            , "id"                    .=! iqr_id
            , "mpeg4_url"             .=! iqr_mpeg4_url
            , "mpeg4_width"           .=!! iqr_mpeg4_width
            , "mpeg4_height"          .=!! iqr_mpeg4_height
            , "thumb_url"             .=! iqr_mpeg4_thumb_url
            , "title"                 .=!! iqr_mpeg4_title
            , "caption"               .=!! iqr_caption
            , "input_message_content" .=!! iqr_reply_markup
            , "reply_markup"          .=!! iqr_input_message_content
            ]
  toJSON InlineQueryResultVideo{..} =
    object' [ "type"                  .=! String "video"
            , "id"                    .=! iqr_id
            , "video_url"             .=! iqr_video_url
            , "mime_type"             .=! iqr_video_mime_type
            , "thumb_url"             .=! iqr_video_thumb_url
            , "title"                 .=! iqr_title
            , "caption"               .=!! iqr_caption
            , "video_width"           .=!! iqr_video_width
            , "video_height"          .=!! iqr_video_height
            , "video_duration"        .=!! iqr_video_duration
            , "description"           .=!! iqr_description
            , "reply_markup"          .=!! iqr_reply_markup
            , "input_message_content" .=!! iqr_input_message_content
            ]
  toJSON InlineQueryResultAudio{..} =
    object' [ "type"                  .=! String "audio"
            , "id"                    .=! iqr_id
            , "audio_url"             .=! iqr_audio_url
            , "title"                 .=! iqr_title
            , "caption"               .=!! iqr_caption
            , "performer"             .=!! iqr_performer
            , "audio_duration"        .=!! iqr_audio_duration
            , "reply_markup"          .=!! iqr_reply_markup
            , "input_message_content" .=!! iqr_input_message_content
            ]
  toJSON InlineQueryResultVoice{..} =
    object' [ "type"                  .=! String "voice"
            , "id"                    .=! iqr_id
            , "voice_url"             .=! iqr_voice_url
            , "title"                 .=! iqr_title
            , "caption"               .=!! iqr_caption
            , "voice_duration"        .=!! iqr_voice_duration
            , "reply_markup"          .=!! iqr_reply_markup
            , "input_message_content" .=!! iqr_input_message_content
            ]
  toJSON InlineQueryResultDocument{..} =
    object' [ "type"                  .=! String "document"
            , "id"                    .=! iqr_id
            , "title"                 .=! iqr_title
            , "caption"               .=!! iqr_caption
            , "document_url"          .=! iqr_document_url
            , "mime_type"             .=! iqr_document_mime_type
            , "description"           .=!! iqr_description
            , "reply_markup"          .=!! iqr_reply_markup
            , "input_message_content" .=!! iqr_input_message_content
            , "thumb_url"             .=!! iqr_thumb_url
            , "thumb_width"           .=!! iqr_thumb_width
            , "thumb_height"          .=!! iqr_thumb_height
            ]
  toJSON InlineQueryResultLocation{..} =
    object' [ "type"                  .=! String "location"
            , "id"                    .=! iqr_id
            , "latitude"              .=! iqr_latitude
            , "longitude"             .=! iqr_longitude
            , "title"                 .=! iqr_title
            , "reply_markup"          .=!! iqr_reply_markup
            , "input_message_content" .=!! iqr_input_message_content
            , "thumb_url"             .=!! iqr_thumb_url
            , "thumb_width"           .=!! iqr_thumb_width
            , "thumb_height"          .=!! iqr_thumb_height
            ]
  toJSON InlineQueryResultVenue{..} =
    object' [ "type"                  .=! String "venue"
            , "id"                    .=! iqr_id
            , "latitude"              .=! iqr_latitude
            , "longitude"             .=! iqr_longitude
            , "title"                 .=! iqr_title
            , "address"               .=! iqr_address
            , "foursquare_id"         .=!! iqr_foursquare_id
            , "reply_markup"          .=!! iqr_reply_markup
            , "input_message_content" .=!! iqr_input_message_content
            , "thumb_url"             .=!! iqr_thumb_url
            , "thumb_width"           .=!! iqr_thumb_width
            , "thumb_height"          .=!! iqr_thumb_height
            ]
  toJSON InlineQueryResultContact{..} =
    object' [ "type"                  .=! String "venue"
            , "id"                    .=! iqr_id
            , "phone_number"          .=! iqr_phone_number
            , "first_name"            .=! iqr_first_name
            , "last_name"             .=!! iqr_last_name
            , "reply_markup"          .=!! iqr_reply_markup
            , "input_message_content" .=!! iqr_input_message_content
            , "thumb_url"             .=!! iqr_thumb_url
            , "thumb_width"           .=!! iqr_thumb_width
            , "thumb_height"          .=!! iqr_thumb_height
            ]
  toJSON (InlineQueryResultGame ident gsn reply_markup) =
    object' [ "type"            .=! String "game"
            , "id"              .=! ident
            , "game_short_name" .=! gsn
            , "reply_markup"    .=!! reply_markup
            ]
  toJSON InlineQueryResultCachedPhoto{..} =
    object' [ "type"                  .=! String "photo"
            , "id"                    .=! iqrc_id
            , "photo_file_id"         .=! iqrc_photo_file_id
            , "title"                 .=!! iqrc_photo_title
            , "description"           .=!! iqrc_description
            , "caption"               .=!! iqrc_caption
            , "input_message_content" .=!! iqrc_reply_markup
            , "reply_markup"          .=!! iqrc_input_message_content
            ]
  toJSON InlineQueryResultCachedGif{..} =
    object' [ "type"                  .=! String "gif"
            , "id"                    .=! iqrc_id
            , "gif_file_id"           .=! iqrc_gif_file_id
            , "title"                 .=!! iqrc_gif_title
            , "caption"               .=!! iqrc_caption
            , "input_message_content" .=!! iqrc_reply_markup
            , "reply_markup"          .=!! iqrc_input_message_content
            ]
  toJSON InlineQueryResultCachedMpeg4Gif{..} =
    object' [ "type"                  .=! String "mpeg4_gif"
            , "id"                    .=! iqrc_id
            , "mpeg4_file_id"         .=! iqrc_mpeg4_file_id
            , "title"                 .=!! iqrc_mpeg4_title
            , "caption"               .=!! iqrc_caption
            , "input_message_content" .=!! iqrc_reply_markup
            , "reply_markup"          .=!! iqrc_input_message_content
            ]
  toJSON InlineQueryResultCachedSticker{..} =
    object' [ "type"                  .=! String "sticker"
            , "id"                    .=! iqrc_id
            , "sticker_file_id"       .=! iqrc_sticker_file_id
            , "reply_markup"          .=!! iqrc_reply_markup
            , "input_message_content" .=!! iqrc_input_message_content
            ]
  toJSON InlineQueryResultCachedDocument{..} =
    object' [ "type"                  .=! String "document"
            , "id"                    .=! iqrc_id
            , "title"                 .=! iqrc_title
            , "document_file_id"      .=! iqrc_document_file_id
            , "caption"               .=!! iqrc_description
            , "description"           .=!! iqrc_caption
            , "reply_markup"          .=!! iqrc_reply_markup
            , "input_message_content" .=!! iqrc_input_message_content
            ]
  toJSON InlineQueryResultCachedVideo{..} =
    object' [ "type"                  .=! String "video"
            , "id"                    .=! iqrc_id
            , "video_file_id"         .=! iqrc_video_file_id
            , "title"                 .=! iqrc_title
            , "description"           .=!! iqrc_description
            , "caption"               .=!! iqrc_caption
            , "reply_markup"          .=!! iqrc_reply_markup
            , "input_message_content" .=!! iqrc_input_message_content
            ]
  toJSON InlineQueryResultCachedVoice{..} =
    object' [ "type"                  .=! String "voice"
            , "id"                    .=! iqrc_id
            , "voice_file_id"         .=! iqrc_voice_file_id
            , "title"                 .=! iqrc_title
            , "caption"               .=!! iqrc_caption
            , "reply_markup"          .=!! iqrc_reply_markup
            , "input_message_content" .=!! iqrc_input_message_content
            ]
  toJSON InlineQueryResultCachedAudio{..} =
    object' [ "type"                  .=! String "audio"
            , "id"                    .=! iqrc_id
            , "audio_file_id"         .=! iqrc_audio_file_id
            , "caption"               .=!! iqrc_caption
            , "reply_markup"          .=!! iqrc_reply_markup
            , "input_message_content" .=!! iqrc_input_message_content
            ]

------------------------
-- FromJSON INSTANCES --
------------------------

instance FromJSON InlineQuery where
  parseJSON = withObject "InlineQuery" $ \o ->
    InlineQuery <$> o .: "id"
                <*> o .: "from"
                <*> o .: "query"
                <*> o .: "offset"
                <*> o .:? "location"

instance FromJSON ChosenInlineResult where
  parseJSON = withObject "ChosenInlineResult" $ \o ->
    ChosenInlineResult <$> o .: "result_id"
                       <*> o .: "from"
                       <*> o .: "query"
                       <*> o .:? "inline_message_id"
                       <*> o .:? "location"

instance FromJSON InputMessageContent where
  parseJSON = withObject "InputMessageContent" $ \o ->
    InputTextMessageContent <$> o .: "message_text"
                            <*> o .:? "parse_mode"
                            <*> o .:? "disable_web_page_preview" .!= False
    <|> InputVenueMessageContent <$> o .: "latitude"
                                 <*> o .: "longitude"
                                 <*> o .: "title"
                                 <*> o .: "address"
                                 <*> o .:? "foursquare_id"
    <|> InputLocationMessageContent <$> o .: "latitude"
                                    <*> o .: "longitude"
    <|> InputContactMessageContent <$> o .: "phone_number"
                                   <*> o .: "first_name"
                                   <*> o .:? "last_name"

instance FromJSON InlineKeyboardMarkup where
  parseJSON = withObject "InlineKeyboardMarkup" $ \o ->
    IKM <$> o .: "inline_keyboard"

instance FromJSON InlineQueryResult where
  parseJSON = withObject "InlineQueryResult" $ \o ->
    case "type" `KM.lookup` o of
      Nothing -> fail "No [type] argument in InlineQueryResult object"
      Just val -> go o val
   where
    go o = withText "InlineQueryResult(type)" $ \s ->
      case s of
        "article" ->
          InlineQueryResultArticle <$> o .: "id"
                                   <*> o .: "title"
                                   <*> o .: "input_message_content"
                                   <*> o .:? "reply_markup"
                                   <*> o .:? "url"
                                   <*> o .:? "hide_url" .!= False
                                   <*> o .:? "description"
                                   <*> o .:? "thumb_url"
                                   <*> o .:? "thumb_width"
                                   <*> o .:? "thumb_height"
        "photo" ->
          InlineQueryResultPhoto <$> o .: "id"
                                 <*> o .: "photo_url"
                                 <*> o .: "thumb_url"
                                 <*> o .:? "photo_width"
                                 <*> o .:? "photo_height"
                                 <*> o .:? "title"
                                 <*> o .:? "description"
                                 <*> o .:? "caption"
                                 <*> o .:? "reply_markup"
                                 <*> o .:? "input_message_content"
          <|> InlineQueryResultCachedPhoto <$> o .: "id"
                                           <*> o .: "photo_file_id"
                                           <*> o .:? "title"
                                           <*> o .:? "description"
                                           <*> o .:? "caption"
                                           <*> o .:? "reply_markup"
                                           <*> o .:? "input_message_content"
        "gif" ->
          InlineQueryResultGif <$> o .: "id"
                               <*> o .: "gif_url"
                               <*> o .:? "gif_width"
                               <*> o .:? "gif_height"
                               <*> o .: "thumb_url"
                               <*> o .:? "title"
                               <*> o .:? "caption"
                               <*> o .:? "reply_markup"
                               <*> o .:? "input_message_content"
          <|> InlineQueryResultCachedGif <$> o .: "id"
                                         <*> o .: "gif_file_id"
                                         <*> o .:? "title"
                                         <*> o .:? "caption"
                                         <*> o .:? "reply_markup"
                                         <*> o .:? "input_message_content"
        "mpeg4_gif" ->
          InlineQueryResultMpeg4Gif <$> o .: "id"
                                    <*> o .: "mpeg4_url"
                                    <*> o .:? "mpeg4_width"
                                    <*> o .:? "mpeg4_height"
                                    <*> o .: "thumb_url"
                                    <*> o .:? "title"
                                    <*> o .:? "caption"
                                    <*> o .:? "reply_markup"
                                    <*> o .:? "input_message_content"
          <|> InlineQueryResultCachedMpeg4Gif <$> o .: "id"
                                              <*> o .: "mpeg4_file_id"
                                              <*> o .:? "title"
                                              <*> o .:? "caption"
                                              <*> o .:? "reply_markup"
                                              <*> o .:? "input_message_content"
        "video" ->
          InlineQueryResultVideo <$> o .: "id"
                                 <*> o .: "video_url"
                                 <*> o .: "mime_type"
                                 <*> o .: "thumb_url"
                                 <*> o .: "title"
                                 <*> o .:? "caption"
                                 <*> o .:? "video_width"
                                 <*> o .:? "video_height"
                                 <*> o .:? "video_duration"
                                 <*> o .:? "description"
                                 <*> o .:? "reply_markup"
                                 <*> o .:? "input_message_content"
          <|> InlineQueryResultCachedVideo <$> o .: "id"
                                           <*> o .: "video_file_id"
                                           <*> o .: "title"
                                           <*> o .:? "description"
                                           <*> o .:? "caption"
                                           <*> o .:? "reply_markup"
                                           <*> o .:? "input_message_content"
        "audio" ->
          InlineQueryResultAudio <$> o .: "id"
                                 <*> o .: "audio_url"
                                 <*> o .: "title"
                                 <*> o .:? "caption"
                                 <*> o .:? "performer"
                                 <*> o .:? "audio_duration"
                                 <*> o .:? "reply_markup"
                                 <*> o .:? "input_message_content"
          <|> InlineQueryResultCachedAudio <$> o .: "id"
                                           <*> o .: "audio_file_id"
                                           <*> o .:? "caption"
                                           <*> o .:? "reply_markup"
                                           <*> o .:? "input_message_content"
        "voice" ->
          InlineQueryResultVoice <$> o .: "id"
                                 <*> o .: "voice_url"
                                 <*> o .: "title"
                                 <*> o .:? "caption"
                                 <*> o .:? "voice_duration"
                                 <*> o .:? "reply_markup"
                                 <*> o .:? "input_message_content"
          <|> InlineQueryResultCachedVoice <$> o .: "id"
                                           <*> o .: "voice_file_id"
                                           <*> o .: "title"
                                           <*> o .:? "caption"
                                           <*> o .:? "reply_markup"
                                           <*> o .:? "input_message_content"
        "document" ->
          InlineQueryResultDocument <$> o .: "id"
                                    <*> o .: "title"
                                    <*> o .:? "caption"
                                    <*> o .: "document_url"
                                    <*> o .: "mime_type"
                                    <*> o .:? "description"
                                    <*> o .:? "reply_markup"
                                    <*> o .:? "input_message_content"
                                    <*> o .:? "thumb_url"
                                    <*> o .:? "thumb_width"
                                    <*> o .:? "thumb_height"
          <|> InlineQueryResultCachedDocument <$> o .: "id"
                                              <*> o .: "title"
                                              <*> o .: "document_file_id"
                                              <*> o .:? "description"
                                              <*> o .:? "caption"
                                              <*> o .:? "reply_markup"
                                              <*> o .:? "input_message_content"
        "location" ->
          InlineQueryResultLocation <$> o .: "id"
                                    <*> o .: "latitude"
                                    <*> o .: "longitude"
                                    <*> o .: "title"
                                    <*> o .:? "reply_markup"
                                    <*> o .:? "input_message_content"
                                    <*> o .:? "thumb_url"
                                    <*> o .:? "thumb_width"
                                    <*> o .:? "thumb_height"
        "venue" ->
          InlineQueryResultVenue <$> o .: "id"
                                 <*> o .: "latitude"
                                 <*> o .: "longitude"
                                 <*> o .: "title"
                                 <*> o .: "address"
                                 <*> o .:? "foursquare_id"
                                 <*> o .:? "reply_markup"
                                 <*> o .:? "input_message_content"
                                 <*> o .:? "thumb_url"
                                 <*> o .:? "thumb_width"
                                 <*> o .:? "thumb_height"
        "contact" ->
          InlineQueryResultContact <$> o .: "id"
                                   <*> o .: "phone_number"
                                   <*> o .: "first_name"
                                   <*> o .:? "last_name"
                                   <*> o .:? "reply_markup"
                                   <*> o .:? "input_message_content"
                                   <*> o .:? "thumb_url"
                                   <*> o .:? "thumb_width"
                                   <*> o .:? "thumb_height"
        "game" ->
          InlineQueryResultGame <$> o .: "id"
                                <*> o .: "game_short_name"
                                <*> o .:? "reply_markup"
        "sticker" ->
          InlineQueryResultCachedSticker <$> o .: "id"
                                         <*> o .: "sticker_file_id"
                                         <*> o .:? "reply_markup"
                                         <*> o .:? "input_message_content"
        wat -> fail $ "Wrong String \"" <> unpack wat <> "\" in InlineQueryResult's [type] argument"
