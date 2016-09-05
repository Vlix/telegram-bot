module Web.Telegram.Instances.Inline where


import           Control.Applicative        ((<|>))
import           Data.Aeson
import           Data.Aeson.Types           (typeMismatch)
import           Data.Text                  (unpack)
import           Data.Monoid                ((<>))
import qualified Data.HashMap.Strict        as HM

import           Web.Telegram.Types.Basic
import           Web.Telegram.Types.Inline
import           Web.Telegram.Instances.Basic

----------------------
-- ToJSON INSTANCES --
----------------------

instance ToJSON InlineQuery where
    toJSON (InlineQuery ident from query offset location) = object [ "id"       .= ident
                                                                   , "from"     .= from
                                                                   , "query"    .= query
                                                                   , "offset"   .= offset
                                                                   , "location" .= location
                                                                   ]

instance ToJSON ChosenInlineResult where
    toJSON (ChosenInlineResult result_id from query inline location) = object [ "result_id"         .= result_id
                                                                              , "from"              .= from
                                                                              , "query"             .= query
                                                                              , "inline_message_id" .= inline
                                                                              , "location"          .= location
                                                                              ]

instance ToJSON InputMessageContent where
    toJSON (InputTextMessageContent message_text parse_mode disable_web_page_preview) = object [ "message_text"             .= message_text
                                                                                               , "parse_mode"               .= parse_mode
                                                                                               , "disable_web_page_preview" .= disable_web_page_preview
                                                                                               ]
    toJSON (InputVenueMessageContent latitude longitude title address
                                     foursquare_id) = object [ "latitude"      .= latitude
                                                             , "longitude"     .= longitude
                                                             , "title"         .= title
                                                             , "address"       .= address
                                                             , "foursquare_id" .= foursquare_id
                                                             ]
    toJSON (InputLocationMessageContent latitude longitude) = object [ "latitude"  .= latitude
                                                                     , "longitude" .= longitude
                                                                     ]
    toJSON (InputContactMessageContent phone_number first_name last_name) = object [ "phone_number" .= phone_number
                                                                                   , "first_name"   .= first_name
                                                                                   , "last_name"    .= last_name
                                                                                   ]

instance ToJSON InlineKeyboardMarkup where
    toJSON (IKM buttons) = object [ "inline_keyboard" .= buttons ]

instance ToJSON InlineQueryResult where
    toJSON (InlineQueryResultArticle ident title    input_message_content reply_markup
                                     url   hide_url description           thumb_url
                                     thumb_width    thumb_height) = object [ "type"                  .= String "article"
                                                                           , "id"                    .= ident
                                                                           , "title"                 .= title
                                                                           , "input_message_content" .= input_message_content
                                                                           , "reply_markup"          .= reply_markup
                                                                           , "url"                   .= url
                                                                           , "hide_url"              .= hide_url
                                                                           , "description"           .= description
                                                                           , "thumb_url"             .= thumb_url
                                                                           , "thumb_width"           .= thumb_width
                                                                           , "thumb_height"          .= thumb_height
                                                                           ]
    toJSON (InlineQueryResultPhoto ident        photo_url thumb_url   photo_width 
                                   photo_height title     description caption
                                   reply_markup input_message_content) = object [ "type"                  .= String "photo"
                                                                                , "id"                    .= ident
                                                                                , "photo_url"             .= photo_url
                                                                                , "thumb_url"             .= thumb_url
                                                                                , "photo_width"           .= photo_width
                                                                                , "photo_height"          .= photo_height
                                                                                , "title"                 .= title
                                                                                , "description"           .= description
                                                                                , "caption"               .= caption
                                                                                , "input_message_content" .= input_message_content
                                                                                , "reply_markup"          .= reply_markup
                                                                                ]
    toJSON (InlineQueryResultGif ident     gif_url gif_width gif_height
                                 thumb_url title   caption   reply_markup
                                 input_message_content) = object [ "type"                  .= String "gif"
                                                                 , "id"                    .= ident
                                                                 , "gif_url"               .= gif_url
                                                                 , "gif_width"             .= gif_width
                                                                 , "gif_height"            .= gif_height
                                                                 , "thumb_url"             .= thumb_url
                                                                 , "title"                 .= title
                                                                 , "caption"               .= caption
                                                                 , "input_message_content" .= input_message_content
                                                                 , "reply_markup"          .= reply_markup
                                                                 ]
    toJSON (InlineQueryResultMpeg4Gif ident     mpeg4_url mpeg4_width mpeg4_height
                                      thumb_url title     caption     reply_markup
                                      input_message_content) = object [ "type"                  .= String "mpeg4_gif"
                                                                      , "id"                    .= ident
                                                                      , "mpeg4_url"             .= mpeg4_url
                                                                      , "mpeg4_width"           .= mpeg4_width
                                                                      , "mpeg4_height"          .= mpeg4_height
                                                                      , "thumb_url"             .= thumb_url
                                                                      , "title"                 .= title
                                                                      , "caption"               .= caption
                                                                      , "input_message_content" .= input_message_content
                                                                      , "reply_markup"          .= reply_markup
                                                                      ]
    toJSON (InlineQueryResultVideo ident          video_url   mime_type    thumb_url
                                   title          caption     video_width  video_height
                                   video_duration description reply_markup input_message_content) = object [ "type"                  .= String "video"
                                                                                                           , "id"                    .= ident
                                                                                                           , "video_url"             .= video_url
                                                                                                           , "mime_type"             .= mime_type
                                                                                                           , "thumb_url"             .= thumb_url
                                                                                                           , "title"                 .= title
                                                                                                           , "caption"               .= caption
                                                                                                           , "video_width"           .= video_width
                                                                                                           , "video_height"          .= video_height
                                                                                                           , "video_duration"        .= video_duration
                                                                                                           , "description"           .= description
                                                                                                           , "reply_markup"          .= reply_markup
                                                                                                           , "input_message_content" .= input_message_content
                                                                                                           ]
    toJSON (InlineQueryResultAudio ident          audio_url    title    performer
                                   audio_duration reply_markup input_message_content) = object [ "type"                  .= String "audio"
                                                                                               , "id"                    .= ident
                                                                                               , "audio_url"             .= audio_url
                                                                                               , "title"                 .= title
                                                                                               , "performer"             .= performer
                                                                                               , "audio_duration"        .= audio_duration
                                                                                               , "reply_markup"          .= reply_markup
                                                                                               , "input_message_content" .= input_message_content
                                                                                               ]
    toJSON (InlineQueryResultVoice ident        voice_url    title   voice_duration
                                   reply_markup input_message_content) = object [ "type"                  .= String "voice"
                                                                                , "id"                    .= ident
                                                                                , "voice_url"             .= voice_url
                                                                                , "title"                 .= title
                                                                                , "voice_duration"        .= voice_duration
                                                                                , "reply_markup"          .= reply_markup
                                                                                , "input_message_content" .= input_message_content
                                                                                ]
    toJSON (InlineQueryResultDocument ident     title       caption      document_url
                                      mime_type description reply_markup input_message_content
                                      thumb_url thumb_width thumb_height ) = object [ "type"                  .= String "document"
                                                                                    , "id"                    .= ident
                                                                                    , "title"                 .= title
                                                                                    , "caption"               .= caption
                                                                                    , "document_url"          .= document_url
                                                                                    , "mime_type"             .= mime_type
                                                                                    , "description"           .= description
                                                                                    , "reply_markup"          .= reply_markup
                                                                                    , "input_message_content" .= input_message_content
                                                                                    , "thumb_url"             .= thumb_url
                                                                                    , "thumb_width"           .= thumb_width
                                                                                    , "thumb_height"          .= thumb_height
                                                                                    ]
    toJSON (InlineQueryResultLocation ident        latitude              longitude title
                                      reply_markup input_message_content thumb_url thumb_width
                                      thumb_height) = object [ "type"                  .= String "location"
                                                             , "id"                    .= ident
                                                             , "latitude"              .= latitude
                                                             , "longitude"             .= longitude
                                                             , "title"                 .= title
                                                             , "reply_markup"          .= reply_markup
                                                             , "input_message_content" .= input_message_content
                                                             , "thumb_url"             .= thumb_url
                                                             , "thumb_width"           .= thumb_width
                                                             , "thumb_height"          .= thumb_height
                                                             ]
    toJSON (InlineQueryResultVenue ident     latitude      longitude    title
                                   address   foursquare_id reply_markup input_message_content
                                   thumb_url thumb_width thumb_height) = object [ "type"                  .= String "venue"
                                                                                , "id"                    .= ident
                                                                                , "latitude"              .= latitude
                                                                                , "longitude"             .= longitude
                                                                                , "title"                 .= title
                                                                                , "address"               .= address
                                                                                , "foursquare_id"         .= foursquare_id
                                                                                , "reply_markup"          .= reply_markup
                                                                                , "input_message_content" .= input_message_content
                                                                                , "thumb_url"             .= thumb_url
                                                                                , "thumb_width"           .= thumb_width
                                                                                , "thumb_height"          .= thumb_height
                                                                                ]
    toJSON (InlineQueryResultContact ident        phone_number          first_name last_name
                                     reply_markup input_message_content thumb_url  thumb_width
                                     thumb_height) = object [ "type"                  .= String "venue"
                                                            , "id"                    .= ident
                                                            , "phone_number"          .= phone_number
                                                            , "first_name"            .= first_name
                                                            , "last_name"             .= last_name
                                                            , "reply_markup"          .= reply_markup
                                                            , "input_message_content" .= input_message_content
                                                            , "thumb_url"             .= thumb_url
                                                            , "thumb_width"           .= thumb_width
                                                            , "thumb_height"          .= thumb_height
                                                            ]
    toJSON (InlineQueryResultCachedPhoto ident   photo_file_id title     description
                                         caption reply_markup  input_message_content) = object [ "type"                  .= String "photo"
                                                                                               , "id"                    .= ident
                                                                                               , "photo_file_id"         .= photo_file_id
                                                                                               , "title"                 .= title
                                                                                               , "description"           .= description
                                                                                               , "caption"               .= caption
                                                                                               , "input_message_content" .= input_message_content
                                                                                               , "reply_markup"          .= reply_markup
                                                                                               ]
    toJSON (InlineQueryResultCachedGif ident        gif_file_id  title  caption
                                       reply_markup input_message_content) = object [ "type"                  .= String "gif"
                                                                                    , "id"                    .= ident
                                                                                    , "gif_file_id"           .= gif_file_id
                                                                                    , "title"                 .= title
                                                                                    , "caption"               .= caption
                                                                                    , "input_message_content" .= input_message_content
                                                                                    , "reply_markup"          .= reply_markup
                                                                                    ]
    toJSON (InlineQueryResultCachedMpeg4Gif ident        mpeg4_file_id  title  caption
                                            reply_markup input_message_content) = object [ "type"                  .= String "mpeg4_gif"
                                                                                         , "id"                    .= ident
                                                                                         , "mpeg4_file_id"         .= mpeg4_file_id
                                                                                         , "title"                 .= title
                                                                                         , "caption"               .= caption
                                                                                         , "input_message_content" .= input_message_content
                                                                                         , "reply_markup"          .= reply_markup
                                                                                         ]
    toJSON (InlineQueryResultCachedSticker ident        sticker_file_id 
                                           reply_markup input_message_content) = object [ "type"                  .= String "sticker"
                                                                                        , "id"                    .= ident
                                                                                        , "sticker_file_id"       .= sticker_file_id
                                                                                        , "reply_markup"          .= reply_markup
                                                                                        , "input_message_content" .= input_message_content
                                                                                        ]
    toJSON (InlineQueryResultCachedDocument ident   title        document_file_id   description
                                            caption reply_markup input_message_content) = object [ "type"                  .= String "document"
                                                                                                 , "id"                    .= ident
                                                                                                 , "title"                 .= title
                                                                                                 , "document_file_id"      .= document_file_id
                                                                                                 , "caption"               .= caption
                                                                                                 , "description"           .= description
                                                                                                 , "reply_markup"          .= reply_markup
                                                                                                 , "input_message_content" .= input_message_content
                                                                                                 ]
    toJSON (InlineQueryResultCachedVideo ident   video_file_id title      description
                                         caption reply_markup  input_message_content) = object [ "type"                  .= String "video"
                                                                                               , "id"                    .= ident
                                                                                               , "video_file_id"         .= video_file_id
                                                                                               , "title"                 .= title
                                                                                               , "description"           .= description
                                                                                               , "caption"               .= caption
                                                                                               , "reply_markup"          .= reply_markup
                                                                                               , "input_message_content" .= input_message_content
                                                                                               ]
    toJSON (InlineQueryResultCachedVoice ident voice_file_id title reply_markup
                                         input_message_content) = object [ "type"                  .= String "voice"
                                                                         , "id"                    .= ident
                                                                         , "voice_file_id"         .= voice_file_id
                                                                         , "title"                 .= title
                                                                         , "reply_markup"          .= reply_markup
                                                                         , "input_message_content" .= input_message_content
                                                                         ]
    toJSON (InlineQueryResultCachedAudio ident audio_file_id reply_markup
                                         input_message_content) = object [ "type"                  .= String "audio"
                                                                         , "id"                    .= ident
                                                                         , "audio_file_id"         .= audio_file_id
                                                                         , "reply_markup"          .= reply_markup
                                                                         , "input_message_content" .= input_message_content
                                                                         ]

------------------------
-- FromJSON INSTANCES --
------------------------

instance FromJSON InlineQuery where
    parseJSON (Object o) = InlineQuery <$> o .: "id"
                                       <*> o .: "from"
                                       <*> o .: "query"
                                       <*> o .: "offset"
                                       <*> o .:? "location"
    parseJSON wat = typeMismatch "InlineQuery" wat

instance FromJSON ChosenInlineResult where
    parseJSON (Object o) = ChosenInlineResult <$> o .: "result_id"
                                              <*> o .: "from"
                                              <*> o .: "query"
                                              <*> o .:? "inline_message_id"
                                              <*> o .:? "location"
    parseJSON wat = typeMismatch "ChosenInlineResult" wat

instance FromJSON InputMessageContent where
    parseJSON (Object o) = InputTextMessageContent <$> o .: "message_text"
                                                   <*> o .:? "parse_mode"
                                                   <*> o .:? "disable_web_page_preview"
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
    parseJSON wat = typeMismatch "InputMessageContent" wat

instance FromJSON InlineKeyboardMarkup where
    parseJSON (Object o) = IKM <$> o .: "inline_keyboard"
    parseJSON wat = typeMismatch "InlineKeyboardMarkup" wat

instance FromJSON InlineQueryResult where
    parseJSON (Object o) = case "type" `HM.lookup` o of
        Just (String "article") -> InlineQueryResultArticle <$> o .: "id"
                                                            <*> o .: "title"
                                                            <*> o .: "input_message_content"
                                                            <*> o .:? "reply_markup"
                                                            <*> o .:? "url"
                                                            <*> o .:? "hide_url"
                                                            <*> o .:? "description"
                                                            <*> o .:? "thumb_url"
                                                            <*> o .:? "thumb_width"
                                                            <*> o .:? "thumb_height"
        Just (String "photo") -> InlineQueryResultPhoto <$> o .: "id"
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
        Just (String "gif") -> InlineQueryResultGif <$> o .: "id"
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
        Just (String "mpeg4_gif") -> InlineQueryResultMpeg4Gif <$> o .: "id"
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
        Just (String "video") -> InlineQueryResultVideo <$> o .: "id"
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
        Just (String "audio") -> InlineQueryResultAudio <$> o .: "id"
                                                        <*> o .: "audio_url"
                                                        <*> o .: "title"
                                                        <*> o .:? "performer"
                                                        <*> o .:? "audio_duration"
                                                        <*> o .:? "reply_markup"
                                                        <*> o .:? "input_message_content"
                             <|> InlineQueryResultCachedAudio <$> o .: "id"
                                                              <*> o .: "audio_file_id"
                                                              <*> o .:? "reply_markup"
                                                              <*> o .:? "input_message_content"
        Just (String "voice") -> InlineQueryResultVoice <$> o .: "id"
                                                        <*> o .: "voice_url"
                                                        <*> o .: "title"
                                                        <*> o .:? "voice_duration"
                                                        <*> o .:? "reply_markup"
                                                        <*> o .:? "input_message_content"
                             <|> InlineQueryResultCachedVoice <$> o .: "id"
                                                              <*> o .: "voice_file_id"
                                                              <*> o .: "title"
                                                              <*> o .:? "reply_markup"
                                                              <*> o .:? "input_message_content"
        Just (String "document") -> InlineQueryResultDocument <$> o .: "id"
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
        Just (String "location") -> InlineQueryResultLocation <$> o .: "id"
                                                              <*> o .: "latitude"
                                                              <*> o .: "longitude"
                                                              <*> o .: "title"
                                                              <*> o .:? "reply_markup"
                                                              <*> o .:? "input_message_content"
                                                              <*> o .:? "thumb_url"
                                                              <*> o .:? "thumb_width"
                                                              <*> o .:? "thumb_height"
        Just (String "venue") -> InlineQueryResultVenue <$> o .: "id"
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
        Just (String "contact") -> InlineQueryResultContact <$> o .: "id"
                                                            <*> o .: "phone_number"
                                                            <*> o .: "first_name"
                                                            <*> o .:? "last_name"
                                                            <*> o .:? "reply_markup"
                                                            <*> o .:? "input_message_content"
                                                            <*> o .:? "thumb_url"
                                                            <*> o .:? "thumb_width"
                                                            <*> o .:? "thumb_height"
        Just (String "sticker") -> InlineQueryResultCachedSticker <$> o .: "id"
                                                                  <*> o .: "sticker_file_id"
                                                                  <*> o .:? "reply_markup"
                                                                  <*> o .:? "input_message_content"
        Just (String wat) -> fail $ "Wrong String \"" <> unpack wat <> "\" in InlineQueryResult's [type] argument"
        Just _ -> fail "Wrong type in InlineQueryResult's [type] argument"
        Nothing -> fail "No [type] argument in InlineQueryResult object"
    parseJSON wat = typeMismatch "InlineQueryResult" wat
