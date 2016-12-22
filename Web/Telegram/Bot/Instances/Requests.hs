{-# LANGUAGE RecordWildCards #-}

module Web.Telegram.Bot.Instances.Requests where


import           Control.Applicative        ((<|>))
import           Data.Aeson
import           Data.Aeson.Types           (typeMismatch,Pair)
import           Data.Text                  (Text)

import           Web.Telegram.Bot.Types.Requests
import           Web.Telegram.Bot.Types.Static
import           Web.Telegram.Bot.Instances.Inline


----------------------
-- ToJSON INSTANCES --
----------------------

instance ToJSON SendMessageRequest where
  toJSON SendMessageRequest{..} =
    object' [ "chat_id"                  .=! message_chat_id
            , "text"                     .=! message_text
            , mBool "disable_web_page_preview" False message_disable_web_page_preview
            , mBool "disable_notification"     False message_disable_notification
            , "parse_mode"               .=!! message_parse_mode
            , "reply_markup"             .=!! message_reply_markup
            ]

instance ToJSON ForwardMessageRequest where
  toJSON ForwardMessageRequest{..} =
    object' [ "chat_id"              .=! forward_chat_id
            , "from_chat_id"         .=! forward_from_chat_id
            , mBool "disable_notification" False forward_disable_notification
            , "message_id"           .=! forward_message_id
            ]

instance ToJSON SendPhotoRequest where
  toJSON SendPhotoRequest{..} =
    object' [ "chat_id"              .=! req_photo_chat_id
            , "photo"                .=! req_photo_photo
            , mBool "disable_notification" False req_photo_disable_notification
            , "caption"              .=!! req_photo_caption
            , "reply_to_message_id"  .=!! req_photo_reply_to_message_id
            , "reply_markup"         .=!! req_photo_reply_markup
            ]

instance ToJSON SendAudioRequest where
  toJSON SendAudioRequest{..} =
    object' [ "chat_id"              .=! req_audio_chat_id
            , "audio"                .=! req_audio_audio
            , mBool "disable_notification" False req_audio_disable_notification
            , "caption"              .=!! req_audio_caption
            , "duration"             .=!! req_audio_duration
            , "performer"            .=!! req_audio_performer
            , "title"                .=!! req_audio_title
            , "reply_to_message_id"  .=!! req_audio_reply_to_message_id
            , "reply_markup"         .=!! req_audio_reply_markup
            ]

instance ToJSON SendStickerRequest where
  toJSON SendStickerRequest{..} =
    object' [ "chat_id"              .=! req_sticker_chat_id
            , "sticker"              .=! req_sticker_sticker
            , mBool "disable_notification" False req_sticker_disable_notification
            , "reply_to_message_id"  .=!! req_sticker_reply_to_message_id
            , "reply_markup"         .=!! req_sticker_reply_markup
            ]

instance ToJSON SendDocumentRequest where
  toJSON SendDocumentRequest{..} =
    object' [ "chat_id"              .=! req_doc_chat_id
            , "document"             .=! req_doc_document
            , mBool "disable_notification" False req_doc_disable_notification
            , "caption"              .=!! req_doc_caption
            , "reply_to_message_id"  .=!! req_doc_reply_to_message_id
            , "reply_markup"         .=!! req_doc_reply_markup
            ]

instance ToJSON SendVideoRequest where
  toJSON SendVideoRequest{..} =
    object' [ "chat_id"              .=! req_video_chat_id
            , "video"                .=! req_video_video
            , mBool "disable_notification" False req_video_disable_notification
            , "duration"             .=!! req_video_duration
            , "width"                .=!! req_video_width
            , "height"               .=!! req_video_height
            , "caption"              .=!! req_video_caption
            , "reply_to_message_id"  .=!! req_video_reply_to_message_id
            , "reply_markup"         .=!! req_video_reply_markup
            ]

instance ToJSON SendVoiceRequest where
  toJSON SendVoiceRequest{..} =
    object' [ "chat_id"              .=! req_voice_chat_id
            , "voice"                .=! req_voice_voice
            , mBool "disable_notification" False req_voice_disable_notification
            , "caption"              .=!! req_voice_caption
            , "duration"             .=!! req_voice_duration
            , "reply_to_message_id"  .=!! req_voice_reply_to_message_id
            , "reply_markup"         .=!! req_voice_reply_markup
            ]


instance ToJSON SendLocationRequest where
  toJSON SendLocationRequest{..} =
    object' [ "chat_id"              .=! req_location_chat_id
            , "latitude"             .=! req_location_latitude
            , "longitude"            .=! req_location_longitude
            , mBool "disable_notification" False req_location_disable_notification
            , "reply_to_message_id"  .=!! req_location_reply_to_message_id
            , "reply_markup"         .=!! req_location_reply_markup
            ]


instance ToJSON SendVenueRequest where
  toJSON SendVenueRequest{..} =
    object' [ "chat_id"              .=! req_venue_chat_id
            , "latitude"             .=! req_venue_latitude
            , "longitude"            .=! req_venue_longitude
            , "title"                .=! req_venue_title
            , "address"              .=! req_venue_address
            , mBool "disable_notification" False req_venue_disable_notification
            , "foursquare_id"        .=!! req_venue_foursquare_id
            , "reply_to_message_id"  .=!! req_venue_reply_to_message_id
            , "reply_markup"         .=!! req_venue_reply_markup
            ]

instance ToJSON SendContactRequest where
  toJSON SendContactRequest{..} =
    object' [ "chat_id"              .=! req_contact_chat_id
            , "phone_number"         .=! req_contact_phone_number
            , "first_name"           .=! req_contact_first_name
            , "last_name"            .=! req_contact_last_name
            , mBool "disable_notification" False req_contact_disable_notification
            , "reply_to_message_id"  .=!! req_contact_reply_to_message_id
            , "reply_markup"         .=!! req_contact_reply_markup
            ]

instance ToJSON SendGameRequest where
  toJSON SendGameRequest{..} =
    object' [ "chat_id"             .=! req_game_chat_id
            , "game_short_name"     .=! req_game_game_short_name
            , mBool "disable_notification" False req_game_disable_notification
            , "reply_to_message_id" .=! req_game_reply_to_message_id
            , "reply_markup"        .=! req_game_reply_markup
            ]

instance ToJSON SetGameScoreRequest where
  toJSON SetGameScoreRequest{..} =
    object' [ "user_id"              .=! req_score_user_id
            , "score"                .=! req_score_score
            , "chat_id"              .=! req_score_chat_id
            , "message_id"           .=! req_score_message_id
            , mBool "force"                False req_score_force
            , mBool "disable_edit_message" False req_score_disable_edit_message
            ]
  toJSON SetGameScoreInlineRequest{..} =
    object' [ "user_id"              .=! req_score_user_id
            , "score"                .=! req_score_score
            , "inline_message_id"    .=! req_score_inline_message_id
            , mBool "force"                False req_score_force
            , mBool "disable_edit_message" False req_score_disable_edit_message
            ]

instance ToJSON GetGameHighScoresRequest where
  toJSON (GetGameHighScoresRequest user_id chat_id message_id) =
    object [ "user_id"    .= user_id
           , "chat_id"    .= chat_id
           , "message_id" .= message_id
           ]
  toJSON (GetGameHighScoresInlineRequest user_id inline_message_id) =
    object [ "user_id"           .= user_id
           , "inline_message_id" .= inline_message_id
           ]

instance ToJSON SendChatActionRequest where
  toJSON (SendChatActionRequest chat_id action) =
    object [ "chat_id" .= chat_id
           , "action"  .= action
           ]

instance ToJSON AnswerInlineQueryRequest where
  toJSON AnswerInlineQueryRequest{..} =
    object' [ "inline_query_id"     .=! query_inline_query_id
            , "results"             .=! query_results
            , mBool "is_personal" False query_is_personal
            , "cache_time"          .=!! query_cache_time
            , "next_offset"         .=!! query_next_offset
            , "switch_pm_text"      .=!! query_switch_pm_text
            , "switch_pm_parameter" .=!! query_switch_pm_parameter
            ]

instance ToJSON UserProfilePhotosRequest where
  toJSON (UserProfilePhotosRequest user_id offset limit) =
    object' [ "user_id" .=! user_id
            , "offset"  .=!! offset
            , "limit"   .=!! limit
            ]

instance ToJSON FileRequest where
  toJSON (FileRequest file_id) =
    object [ "file_id" .= file_id ]

instance ToJSON KickChatMemberRequest where
  toJSON (KickChatMemberRequest chat_id user_id) =
    object [ "chat_id" .= chat_id
           , "user_id" .= user_id
           ]

instance ToJSON LeaveChatRequest where
  toJSON (LeaveChatRequest chat_id) =
    object [ "chat_id" .= chat_id ]

instance ToJSON UnbanChatMemberRequest where
  toJSON (UnbanChatMemberRequest chat_id user_id) =
    object [ "chat_id" .= chat_id
           , "user_id" .= user_id
           ]

instance ToJSON GetChatRequest where
  toJSON (GetChatRequest chat_id) =
    object [ "chat_id" .= chat_id ]

instance ToJSON GetChatAdministratorsRequest where
  toJSON (GetChatAdministratorsRequest chat_id) =
    object [ "chat_id" .= chat_id ]

instance ToJSON GetChatMembersCountRequest where
  toJSON (GetChatMembersCountRequest chat_id) =
    object [ "chat_id" .= chat_id ]

instance ToJSON GetChatMemberRequest where
  toJSON (GetChatMemberRequest chat_id user_id) =
    object [ "chat_id" .= chat_id
           , "user_id" .= user_id
           ]

instance ToJSON AnswerCallbackQueryRequest where
  toJSON (AnswerCallbackQueryRequest callback_query_id text show_alert url cache_time) =
    object' [ "callback_query_id" .=! callback_query_id
            , "text"              .=!! text
            , mBool "show_alert" False show_alert
            , "url"               .=!! url
            , if cache_time == 0
                then Nothing
                else Just $ "cache_time" .= cache_time
            ]

instance ToJSON UpdatesRequest where
  toJSON (UpdatesRequest offset limit timeout allowed_updates) =
    object' [ "offset" .=!! offset
            , check "limit" (\x -> x < 100 && x > 0) limit
            , check "timeout" (<= 0 ) timeout
            , "allowed_updates" .=!! allowed_updates
            ]
   where check :: ToJSON a => Text -> (a -> Bool) -> Maybe a -> Maybe Pair
         check _ _ Nothing  = Nothing
         check t f (Just a) | f a = Just $ t .= a
                            | otherwise = Nothing

instance ToJSON WebhookRequest where
  toJSON (WebhookRequest url max_conns allowed_updates) =
    object' [ "url"    .=! url
            , check "max_connections" (\x -> x > 1 || x < 100 || x /= 40) max_conns
            , "allowed_updates" .=!! allowed_updates
            ]
   where check :: ToJSON a => Text -> (a -> Bool) -> Maybe a -> Maybe Pair
         check _ _ Nothing  = Nothing
         check t f (Just a) | f a = Just $ t .= a
                            | otherwise = Nothing

------------------------
-- FromJSON INSTANCES --
------------------------

instance FromJSON SendMessageRequest where
  parseJSON (Object o) =
    SendMessageRequest <$> o .: "chat_id"
                       <*> o .: "text"
                       <*> o .:? "disable_web_page_preview" .!= False
                       <*> o .:? "disable_notification"     .!= False
                       <*> o .:? "parse_mode"
                       <*> o .:? "reply_to_message_id"
                       <*> o .:? "reply_markup"
  parseJSON wat = typeMismatch "SendMessageRequest" wat

instance FromJSON ForwardMessageRequest where
  parseJSON (Object o) =
    ForwardMessageRequest <$> o .: "chat_id"
                          <*> o .: "from_chat_id"
                          <*> o .:? "disable_notification" .!= False
                          <*> o .: "message_id"
  parseJSON wat = typeMismatch "ForwardMessageRequest" wat

instance FromJSON SendPhotoRequest where
  parseJSON (Object o) =
    SendPhotoRequest <$> o .: "chat_id"
                     <*> o .: "photo"
                     <*> o .:? "disable_notification" .!= False
                     <*> o .:? "caption"
                     <*> o .:? "reply_to_message_id"
                     <*> o .:? "reply_markup"
  parseJSON wat = typeMismatch "SendPhotoRequest" wat

instance FromJSON SendAudioRequest where
  parseJSON (Object o) =
    SendAudioRequest <$> o .: "chat_id"
                     <*> o .: "audio"
                     <*> o .:? "disable_notification" .!= False
                     <*> o .:? "caption"
                     <*> o .:? "duration"
                     <*> o .:? "performer"
                     <*> o .:? "title"
                     <*> o .:? "reply_to_message_id"
                     <*> o .:? "reply_markup"
  parseJSON wat = typeMismatch "SendAudioRequest" wat

instance FromJSON SendStickerRequest where
  parseJSON (Object o) =
    SendStickerRequest <$> o .: "chat_id"
                       <*> o .: "sticker"
                       <*> o .:? "disable_notification" .!= False
                       <*> o .:? "reply_to_message_id"
                       <*> o .:? "reply_markup"
  parseJSON wat = typeMismatch "SendStickerRequest" wat

instance FromJSON SendDocumentRequest where
  parseJSON (Object o) =
    SendDocumentRequest <$> o .: "chat_id"
                        <*> o .: "document"
                        <*> o .:? "disable_notification" .!= False
                        <*> o .:? "caption"
                        <*> o .:? "reply_to_message_id"
                        <*> o .:? "reply_markup"
  parseJSON wat = typeMismatch "SendDocumentRequest" wat

instance FromJSON SendVideoRequest where
  parseJSON (Object o) =
    SendVideoRequest <$> o .: "chat_id"
                     <*> o .: "video"
                     <*> o .:? "disable_notification" .!= False
                     <*> o .:? "duration"
                     <*> o .:? "width"
                     <*> o .:? "height"
                     <*> o .:? "caption"
                     <*> o .:? "reply_to_message_id"
                     <*> o .:? "reply_markup"
  parseJSON wat = typeMismatch "SendVideoRequest" wat

instance FromJSON SendVoiceRequest where
  parseJSON (Object o) =
    SendVoiceRequest <$> o .: "chat_id"
                     <*> o .: "voice"
                     <*> o .:? "disable_notification" .!= False
                     <*> o .:? "caption"
                     <*> o .:? "duration"
                     <*> o .:? "reply_to_message_id"
                     <*> o .:? "reply_markup"
  parseJSON wat = typeMismatch "SendVoiceRequest" wat

instance FromJSON SendLocationRequest where
  parseJSON (Object o) =
    SendLocationRequest <$> o .: "chat_id"
                        <*> o .: "latitude"
                        <*> o .: "longitude"
                        <*> o .:? "disable_notification" .!= False
                        <*> o .:? "reply_to_message_id"
                        <*> o .:? "reply_markup"
  parseJSON wat = typeMismatch "SendLocationRequest" wat

instance FromJSON SendVenueRequest where
  parseJSON (Object o) =
    SendVenueRequest <$> o .: "chat_id"
                     <*> o .: "latitude"
                     <*> o .: "longitude"
                     <*> o .: "title"
                     <*> o .: "address"
                     <*> o .:? "disable_notification" .!= False
                     <*> o .:? "foursquare_id"
                     <*> o .:? "reply_to_message_id"
                     <*> o .:? "reply_markup"
  parseJSON wat = typeMismatch "SendVenueRequest" wat

instance FromJSON SendContactRequest where
  parseJSON (Object o) =
    SendContactRequest <$> o .: "chat_id"
                       <*> o .: "phone_number"
                       <*> o .: "first_name"
                       <*> o .:? "last_name"
                       <*> o .:? "disable_notification" .!= False
                       <*> o .:? "reply_to_message_id"
                       <*> o .:? "reply_markup"
  parseJSON wat = typeMismatch "SendContactRequest" wat

instance FromJSON SendGameRequest where
  parseJSON (Object o) =
    SendGameRequest <$> o .: "chat_id"
                    <*> o .: "game_short_name"
                    <*> o .:? "disable_notification" .!= False
                    <*> o .:? "reply_to_message_id"
                    <*> o .:? "reply_markup"
  parseJSON wat = typeMismatch "SendGameRequest" wat

instance FromJSON SetGameScoreRequest where
  parseJSON (Object o) =
    SetGameScoreRequest <$> o .: "user_id"
                 <*> o .: "score"
                 <*> o .: "chat_id"
                 <*> o .: "message_id"
                 <*> o .:? "force" .!= False
                 <*> o .:? "disable_edit_message" .!= False
    <|> SetGameScoreInlineRequest <$> o .: "chat_id"
                           <*> o .: "score"
                           <*> o .: "inline_message_id"
                           <*> o .:? "force" .!= False
                           <*> o .:? "disable_edit_message" .!= False
  parseJSON wat = typeMismatch "SetGameScoreRequest" wat

instance FromJSON GetGameHighScoresRequest where
  parseJSON (Object o) =
    GetGameHighScoresRequest <$> o .: "user_id"
                             <*> o .: "chat_id"
                             <*> o .: "message_id"
    <|> GetGameHighScoresInlineRequest <$> o .: "user_id"
                                       <*> o .: "inline_message_id"
  parseJSON wat = typeMismatch "GetGameHighScores" wat

instance FromJSON SendChatActionRequest where
  parseJSON (Object o) = SendChatActionRequest <$> o .: "chat_id"
                                                 <*> o .: "action"
  parseJSON wat = typeMismatch "SendChatActionRequest" wat

instance FromJSON AnswerInlineQueryRequest where
  parseJSON (Object o) =
    AnswerInlineQueryRequest <$> o .: "chat_id"
                             <*> o .: "results"
                             <*> o .:? "is_personal" .!= False
                             <*> o .:? "cache_time"
                             <*> o .:? "next_offset"
                             <*> o .:? "switch_pm_text"
                             <*> o .:? "switch_pm_parameter"
  parseJSON wat = typeMismatch "AnswerInlineQueryRequest" wat

instance FromJSON UserProfilePhotosRequest where
  parseJSON (Object o) =
    UserProfilePhotosRequest <$> o .: "user_id"
                             <*> o .:? "offset"
                             <*> o .:? "limit"
  parseJSON wat = typeMismatch "UserProfilePhotosRequest" wat

instance FromJSON FileRequest where
  parseJSON (Object o) = FileRequest <$> o .: "file_id"
  parseJSON wat = typeMismatch "FileRequest" wat

instance FromJSON KickChatMemberRequest where
  parseJSON (Object o) = KickChatMemberRequest <$> o .: "chat_id"
                                               <*> o .: "user_id"
  parseJSON wat = typeMismatch "KickChatMemberRequest" wat

instance FromJSON LeaveChatRequest where
  parseJSON (Object o) = LeaveChatRequest <$> o .: "chat_id"
  parseJSON wat = typeMismatch "LeaveChatRequest" wat

instance FromJSON UnbanChatMemberRequest where
  parseJSON (Object o) = UnbanChatMemberRequest <$> o .: "chat_id"
                                                <*> o .: "user_id"
  parseJSON wat = typeMismatch "UnbanChatMemberRequest" wat

instance FromJSON GetChatRequest where
  parseJSON (Object o) = GetChatRequest <$> o .: "chat_id"
  parseJSON wat = typeMismatch "GetChatRequest" wat

instance FromJSON GetChatAdministratorsRequest where
  parseJSON (Object o) = GetChatAdministratorsRequest <$> o .: "chat_id"
  parseJSON wat = typeMismatch "GetChatAdministratorsRequest" wat

instance FromJSON GetChatMembersCountRequest where
  parseJSON (Object o) = GetChatMembersCountRequest <$> o .: "chat_id"
  parseJSON wat = typeMismatch "GetChatMembersCountRequest" wat

instance FromJSON GetChatMemberRequest where
  parseJSON (Object o) = GetChatMemberRequest <$> o .: "chat_id"
                                              <*> o .: "user_id"
  parseJSON wat = typeMismatch "GetChatMemberRequest" wat

instance FromJSON AnswerCallbackQueryRequest where
  parseJSON (Object o) =
    AnswerCallbackQueryRequest <$> o .: "callback_query_id"
                               <*> o .:? "text"
                               <*> o .:? "show_alert" .!= False
                               <*> o .:? "url"
                               <*> o .:? "cache_time" .!= 0
  parseJSON wat = typeMismatch "AnswerCallbackQueryRequest" wat

instance FromJSON UpdatesRequest where
  parseJSON (Object o) =
    UpdatesRequest <$> o .:? "offset"
                   <*> o .:? "limit"
                   <*> o .:? "timeout"
                   <*> o .:? "allowed_updates"
  parseJSON wat = typeMismatch "UpdatesRequest" wat

instance FromJSON WebhookRequest where
  parseJSON (Object o) =
    WebhookRequest <$> o .: "url"
                   <*> o .:? "max_conns"
                   <*> o .:? "allowed_updates"
  parseJSON wat = typeMismatch "WebhookRequest" wat
