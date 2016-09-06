module Web.Telegram.Instances.UpdateRequests where


import           Control.Applicative        ((<|>))
import           Data.Aeson
import           Data.Aeson.Types           (typeMismatch)

import           Web.Telegram.Types.UpdateRequests
import           Web.Telegram.Instances.Inline
import           Web.Telegram.Instances.Static

----------------------
-- ToJSON INSTANCES --
----------------------

instance ToJSON EditMessageTextRequest where
  toJSON (EditMessageTextRequest chat_id                  message_id
                                 text                     parse_mode
                                 disable_web_page_preview reply_markup) = object [ "chat_id"                  .= chat_id
                                                                                 , "message_id"               .= message_id
                                                                                 , "text"                     .= text
                                                                                 , "parse_mode"               .= parse_mode
                                                                                 , "disable_web_page_preview" .= disable_web_page_preview
                                                                                 , "reply_markup"             .= reply_markup
                                                                                 ]
  toJSON (EditInlineTextRequest inline_message_id text
                                parse_mode        disable_web_page_preview
                                reply_markup) = object [ "inline_message_id"        .= inline_message_id
                                                       , "text"                     .= text
                                                       , "parse_mode"               .= parse_mode
                                                       , "disable_web_page_preview" .= disable_web_page_preview
                                                       , "reply_markup"             .= reply_markup
                                                       ]

instance ToJSON EditMessageCaptionRequest where
  toJSON (EditMessageCaptionRequest chat_id message_id caption reply_markup) = object [ "chat_id"      .= chat_id
                                                                                      , "message_id"   .= message_id
                                                                                      , "caption"      .= caption
                                                                                      , "reply_markup" .= reply_markup
                                                                                      ]
  toJSON (EditInlineCaptionRequest inline_message_id caption reply_markup) = object [ "inline_message_id" .= inline_message_id
                                                                                    , "caption"           .= caption
                                                                                    , "reply_markup"      .= reply_markup
                                                                                    ]

instance ToJSON EditMessageReplyMarkupRequest where
  toJSON (EditMessageReplyMarkupRequest chat_id message_id reply_markup) = object [ "chat_id"      .= chat_id
                                                                                  , "message_id"   .= message_id
                                                                                  , "reply_markup" .= reply_markup
                                                                                  ]
  toJSON (EditInlineReplyMarkupRequest inline_message_id reply_markup) = object [ "inline_message_id" .= inline_message_id
                                                                                , "reply_markup"      .= reply_markup
                                                                                ]

------------------------
-- FromJSON INSTANCES --
------------------------

instance FromJSON EditMessageTextRequest where
    parseJSON (Object o) = EditMessageTextRequest <$> o .: "chat_id"
                                                  <*> o .: "message_id"
                                                  <*> o .: "text"
                                                  <*> o .:? "parse_mode"
                                                  <*> o .:? "disable_web_page_preview"
                                                  <*> o .:? "reply_markup"
                       <|> EditInlineTextRequest <$> o .: "inline_message_id"
                                                 <*> o .: "text"
                                                 <*> o .:? "parse_mode"
                                                 <*> o .:? "disable_web_page_preview"
                                                 <*> o .:? "reply_markup"
    parseJSON wat = typeMismatch "EditMessageTextRequest" wat

instance FromJSON EditMessageCaptionRequest where
  parseJSON (Object o) = EditMessageCaptionRequest <$> o .: "chat_id"
                                                   <*> o .: "message_id"
                                                   <*> o .: "caption"
                                                   <*> o .:? "reply_markup"
                     <|> EditInlineCaptionRequest <$> o .: "inline_message_id"
                                                  <*> o .: "caption"
                                                  <*> o .:? "reply_markup"
  parseJSON wat = typeMismatch "EditMessageCaptionRequest" wat

instance FromJSON EditMessageReplyMarkupRequest where
  parseJSON (Object o) = EditMessageReplyMarkupRequest <$> o .: "chat_id"
                                                       <*> o .: "message_id"
                                                       <*> o .: "reply_markup"
                     <|> EditInlineReplyMarkupRequest <$> o .: "inline_message_id"
                                                      <*> o .: "reply_markup"
  parseJSON wat = typeMismatch "EditMessageReplyMarkupRequest" wat
