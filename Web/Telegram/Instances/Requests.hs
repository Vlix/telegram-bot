module Web.Telegram.Instances.Requests where


import           Data.Aeson
import           Data.Aeson.Types           (typeMismatch)

import           Web.Telegram.Types.Requests
import           Web.Telegram.Instances.Inline


----------------------
-- ToJSON INSTANCES --
----------------------

instance ToJSON SendMessageRequest where
    toJSON (SendMessageRequest chat_id              text
                               parse_mode           disable_web_page_preview
                               disable_notification reply_to_message_id
                               reply_markup) = object [ "chat_id"                  .= chat_id
                                                      , "text"                     .= text
                                                      , "parse_mode"               .= parse_mode
                                                      , "disable_web_page_preview" .= disable_web_page_preview
                                                      , "disable_notification"     .= disable_notification
                                                      , "reply_to_message_id"      .= reply_to_message_id
                                                      , "reply_markup"             .= reply_markup
                                                      ]

instance ToJSON ForwardMessageRequest where
    toJSON (ForwardMessageRequest chat_id              from_chat_id
                                  disable_notification message_id) = object [ "chat_id"              .= chat_id
                                                                            , "from_chat_id"         .= from_chat_id
                                                                            , "disable_notification" .= disable_notification
                                                                            , "message_id"           .= message_id
                                                                            ]

instance ToJSON SendPhotoRequest where
  toJSON (SendPhotoRequest chat_id             photo      caption     disable_notification
                           reply_to_message_id reply_markup) = object [ "chat_id"              .= chat_id
                                                                      , "photo"                .= photo
                                                                      , "caption"              .= caption
                                                                      , "disable_notification" .= disable_notification
                                                                      , "reply_to_message_id"  .= reply_to_message_id
                                                                      , "reply_markup"         .= reply_markup
                                                                      ]

instance ToJSON SendAudioRequest where
  toJSON (SendAudioRequest chat_id audio                duration            performer
                           title   disable_notification reply_to_message_id reply_markup) = object [ "chat_id"              .= chat_id
                                                                                                   , "audio"                .= audio
                                                                                                   , "duration"             .= duration
                                                                                                   , "performer"            .= performer
                                                                                                   , "title"                .= title
                                                                                                   , "disable_notification" .= disable_notification
                                                                                                   , "reply_to_message_id"  .= reply_to_message_id
                                                                                                   , "reply_markup"         .= reply_markup
                                                                                                   ]

instance ToJSON SendStickerRequest where
  toJSON (SendStickerRequest chat_id             sticker     disable_notification
                             reply_to_message_id reply_markup) = object [ "chat_id"              .= chat_id
                                                                        , "sticker"              .= sticker
                                                                        , "disable_notification" .= disable_notification
                                                                        , "reply_to_message_id"  .= reply_to_message_id
                                                                        , "reply_markup"         .= reply_markup
                                                                        ]

instance ToJSON SendDocumentRequest where
  toJSON (SendDocumentRequest chat_id             document     caption    disable_notification
                              reply_to_message_id reply_markup) = object [ "chat_id"              .= chat_id
                                                                         , "document"             .= document
                                                                         , "caption"              .= caption
                                                                         , "disable_notification" .= disable_notification
                                                                         , "reply_to_message_id"  .= reply_to_message_id
                                                                         , "reply_markup"         .= reply_markup
                                                                         ]

instance ToJSON SendVideoRequest where
  toJSON (SendVideoRequest chat_id video   duration             width 
                           height  caption disable_notification reply_to_message_id
                           reply_markup) = object [ "chat_id"              .= chat_id
                                                  , "video"                .= video
                                                  , "duration"             .= duration
                                                  , "width"                .= width
                                                  , "height"               .= height
                                                  , "caption"              .= caption
                                                  , "disable_notification" .= disable_notification
                                                  , "reply_to_message_id"  .= reply_to_message_id
                                                  , "reply_markup"         .= reply_markup
                                                  ]

instance ToJSON SendVoiceRequest where
  toJSON (SendVoiceRequest chat_id             voice     duration    disable_notification
                           reply_to_message_id reply_markup) = object [ "chat_id"              .= chat_id
                                                                      , "voice"                .= voice
                                                                      , "duration"             .= duration
                                                                      , "disable_notification" .= disable_notification
                                                                      , "reply_to_message_id"  .= reply_to_message_id
                                                                      , "reply_markup"         .= reply_markup
                                                                      ]


instance ToJSON SendLocationRequest where
  toJSON (SendLocationRequest chat_id             latitude     longitude    disable_notification
                              reply_to_message_id reply_markup) = object [ "chat_id"              .= chat_id
                                                                         , "latitude"             .= latitude
                                                                         , "longitude"            .= longitude
                                                                         , "disable_notification" .= disable_notification
                                                                         , "reply_to_message_id"  .= reply_to_message_id
                                                                         , "reply_markup"         .= reply_markup
                                                                         ]


instance ToJSON SendVenueRequest where
  toJSON (SendVenueRequest chat_id latitude      longitude            title 
                           address foursquare_id disable_notification reply_to_message_id
                           reply_markup) = object [ "chat_id"              .= chat_id
                                                  , "latitude"             .= latitude
                                                  , "longitude"            .= longitude
                                                  , "title"                .= title
                                                  , "address"              .= address
                                                  , "foursquare_id"        .= foursquare_id
                                                  , "disable_notification" .= disable_notification
                                                  , "reply_to_message_id"  .= reply_to_message_id
                                                  , "reply_markup"         .= reply_markup
                                                  ]

instance ToJSON SendContactRequest where
  toJSON (SendContactRequest chat_id              phone_number        first_name  last_name 
                             disable_notification reply_to_message_id reply_markup) = object [ "chat_id"              .= chat_id
                                                                                             , "phone_number"         .= phone_number
                                                                                             , "first_name"           .= first_name
                                                                                             , "last_name"            .= last_name
                                                                                             , "disable_notification" .= disable_notification
                                                                                             , "reply_to_message_id"  .= reply_to_message_id
                                                                                             , "reply_markup"         .= reply_markup
                                                                                             ]

instance ToJSON SendChatActionRequest where
  toJSON (SendChatActionRequest chat_id action) = object [ "chat_id" .= chat_id
                                                         , "action"  .= action
                                                         ]

instance ToJSON AnswerInlineQueryRequest where
  toJSON (AnswerInlineQueryRequest inline_query_id results        cache_time     is_personal 
                                   next_offset     switch_pm_text switch_pm_parameter) = object [ "inline_query_id"     .= inline_query_id
                                                                                                , "results"             .= results
                                                                                                , "cache_time"          .= cache_time
                                                                                                , "is_personal"         .= is_personal
                                                                                                , "next_offset"         .= next_offset
                                                                                                , "switch_pm_text"      .= switch_pm_text
                                                                                                , "switch_pm_parameter" .= switch_pm_parameter
                                                                                                ]

instance ToJSON UserProfilePhotosRequest where
  toJSON (UserProfilePhotosRequest user_id offset limit) = object [ "user_id" .= user_id
                                                                  , "offset"  .= offset
                                                                  , "limit"   .= limit
                                                                  ]

instance ToJSON FileRequest where
  toJSON (FileRequest file_id) = object [ "file_id" .= file_id ]

instance ToJSON KickChatMemberRequest where
  toJSON (KickChatMemberRequest chat_id user_id) = object [ "chat_id" .= chat_id
                                                          , "user_id" .= user_id
                                                          ]

instance ToJSON LeaveChatRequest where
  toJSON (LeaveChatRequest chat_id) = object [ "chat_id" .= chat_id ]

instance ToJSON UnbanChatMemberRequest where
  toJSON (UnbanChatMemberRequest chat_id user_id) = object [ "chat_id" .= chat_id
                                                           , "user_id" .= user_id
                                                           ]

instance ToJSON GetChatRequest where
  toJSON (GetChatRequest chat_id) = object [ "chat_id" .= chat_id ]

instance ToJSON GetChatAdministratorsRequest where
  toJSON (GetChatAdministratorsRequest chat_id) = object [ "chat_id" .= chat_id ]

instance ToJSON GetChatMembersCountRequest where
  toJSON (GetChatMembersCountRequest chat_id) = object [ "chat_id" .= chat_id ]

instance ToJSON GetChatMemberRequest where
  toJSON (GetChatMemberRequest chat_id user_id) = object [ "chat_id" .= chat_id
                                                         , "user_id" .= user_id
                                                         ]

instance ToJSON AnswerCallbackQueryRequest where
  toJSON (AnswerCallbackQueryRequest callback_query_id text show_alert) = object [ "callback_query_id" .= callback_query_id
                                                                                 , "text"              .= text
                                                                                 , "show_alert"        .= show_alert
                                                                                 ]

------------------------
-- FromJSON INSTANCES --
------------------------

instance FromJSON SendMessageRequest where
    parseJSON (Object o) = SendMessageRequest <$> o .: "chat_id"
                                              <*> o .: "text"
                                              <*> o .:? "parse_mode"
                                              <*> o .:? "disable_web_page_preview"
                                              <*> o .:? "disable_notification"
                                              <*> o .:? "reply_to_message_id"
                                              <*> o .:? "reply_markup"
    parseJSON wat = typeMismatch "SendMessageRequest" wat

instance FromJSON ForwardMessageRequest where
    parseJSON (Object o) = ForwardMessageRequest <$> o .: "chat_id"
                                                 <*> o .: "from_chat_id" 
                                                 <*> o .:? "disable_notification" 
                                                 <*> o .: "message_id"
    parseJSON wat = typeMismatch "ForwardMessageRequest" wat

instance FromJSON SendPhotoRequest where
    parseJSON (Object o) = SendPhotoRequest <$> o .: "chat_id"
                                            <*> o .: "photo"
                                            <*> o .:? "caption"
                                            <*> o .:? "disable_notification"
                                            <*> o .:? "reply_to_message_id"
                                            <*> o .:? "reply_markup"
    parseJSON wat = typeMismatch "SendPhotoRequest" wat

instance FromJSON SendAudioRequest where
    parseJSON (Object o) = SendAudioRequest <$> o .: "chat_id"
                                            <*> o .: "audio"
                                            <*> o .:? "duration"
                                            <*> o .:? "performer"
                                            <*> o .:? "title"
                                            <*> o .:? "disable_notification"
                                            <*> o .:? "reply_to_message_id"
                                            <*> o .:? "reply_markup"
    parseJSON wat = typeMismatch "SendAudioRequest" wat

instance FromJSON SendStickerRequest where
    parseJSON (Object o) = SendStickerRequest <$> o .: "chat_id"
                                              <*> o .: "sticker"
                                              <*> o .:? "disable_notification"
                                              <*> o .:? "reply_to_message_id"
                                              <*> o .:? "reply_markup"
    parseJSON wat = typeMismatch "SendStickerRequest" wat

instance FromJSON SendDocumentRequest where
    parseJSON (Object o) = SendDocumentRequest <$> o .: "chat_id"
                                               <*> o .: "document"
                                               <*> o .:? "caption"
                                               <*> o .:? "disable_notification"
                                               <*> o .:? "reply_to_message_id"
                                               <*> o .:? "reply_markup"
    parseJSON wat = typeMismatch "SendDocumentRequest" wat

instance FromJSON SendVideoRequest where
    parseJSON (Object o) = SendVideoRequest <$> o .: "chat_id"
                                            <*> o .: "video"
                                            <*> o .:? "duration"
                                            <*> o .:? "width"
                                            <*> o .:? "height"
                                            <*> o .:? "caption"
                                            <*> o .:? "disable_notification"
                                            <*> o .:? "reply_to_message_id"
                                            <*> o .:? "reply_markup"
    parseJSON wat = typeMismatch "SendVideoRequest" wat

instance FromJSON SendVoiceRequest where
    parseJSON (Object o) = SendVoiceRequest <$> o .: "chat_id"
                                            <*> o .: "voice"
                                            <*> o .:? "duration"
                                            <*> o .:? "disable_notification"
                                            <*> o .:? "reply_to_message_id"
                                            <*> o .:? "reply_markup"
    parseJSON wat = typeMismatch "SendVoiceRequest" wat

instance FromJSON SendLocationRequest where
    parseJSON (Object o) = SendLocationRequest <$> o .: "chat_id"
                                               <*> o .: "latitude"
                                               <*> o .: "longitude"
                                               <*> o .:? "disable_notification"
                                               <*> o .:? "reply_to_message_id"
                                               <*> o .:? "reply_markup"
    parseJSON wat = typeMismatch "SendLocationRequest" wat

instance FromJSON SendVenueRequest where
    parseJSON (Object o) = SendVenueRequest <$> o .: "chat_id"
                                            <*> o .: "latitude"
                                            <*> o .: "longitude"
                                            <*> o .: "title"
                                            <*> o .: "address"
                                            <*> o .:? "foursquare_id"
                                            <*> o .:? "disable_notification"
                                            <*> o .:? "reply_to_message_id"
                                            <*> o .:? "reply_markup"
    parseJSON wat = typeMismatch "SendVenueRequest" wat

instance FromJSON SendContactRequest where
    parseJSON (Object o) = SendContactRequest <$> o .: "chat_id"
                                              <*> o .: "phone_number"
                                              <*> o .: "first_name"
                                              <*> o .:? "last_name"
                                              <*> o .:? "disable_notification"
                                              <*> o .:? "reply_to_message_id"
                                              <*> o .:? "reply_markup"
    parseJSON wat = typeMismatch "SendContactRequest" wat

instance FromJSON SendChatActionRequest where
    parseJSON (Object o) = SendChatActionRequest <$> o .: "chat_id"
                                                 <*> o .: "action"
    parseJSON wat = typeMismatch "SendChatActionRequest" wat

instance FromJSON AnswerInlineQueryRequest where
    parseJSON (Object o) = AnswerInlineQueryRequest <$> o .: "chat_id"
                                                    <*> o .: "results"
                                                    <*> o .:? "cache_time"
                                                    <*> o .:? "is_personal"
                                                    <*> o .:? "next_offset"
                                                    <*> o .:? "switch_pm_text"
                                                    <*> o .:? "switch_pm_parameter"
    parseJSON wat = typeMismatch "AnswerInlineQueryRequest" wat

instance FromJSON UserProfilePhotosRequest where
  parseJSON (Object o) = UserProfilePhotosRequest <$> o .: "user_id"
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
  parseJSON (Object o) = AnswerCallbackQueryRequest <$> o .: "callback_query_id"
                                                    <*> o .:? "text"
                                                    <*> o .:? "show_alert"
  parseJSON wat = typeMismatch "AnswerCallbackQueryRequest" wat
