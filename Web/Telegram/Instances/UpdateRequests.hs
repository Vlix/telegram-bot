{-# LANGUAGE RecordWildCards #-}

module Web.Telegram.Instances.UpdateRequests where


import           Control.Applicative        ((<|>))
import           Data.Aeson
import           Data.Aeson.Types           (typeMismatch)

import           Web.Telegram.Types.UpdateRequests
import           Web.Telegram.Types.Static
import           Web.Telegram.Instances.Inline
import           Web.Telegram.Instances.Static

----------------------
-- ToJSON INSTANCES --
----------------------

instance ToJSON EditMessageTextRequest where
  toJSON EditMessageTextRequest{..} =
    object' [ "chat_id"                  .=! edit_text_chat_id
            , "message_id"               .=! edit_text_message_id
            , "text"                     .=! edit_text_text
            , mBool "disable_web_page_preview" False edit_text_disable_web_page_preview
            , "parse_mode"               .=! edit_text_parse_mode
            , "reply_markup"             .=! edit_text_reply_markup
            ]
  toJSON EditInlineTextRequest{..} =
    object' [ "inline_message_id"        .=! edit_text_inline_message_id
            , "text"                     .=! edit_text_text
            , mBool "disable_web_page_preview" False edit_text_disable_web_page_preview
            , "parse_mode"               .=! edit_text_parse_mode
            , "reply_markup"             .=! edit_text_reply_markup
            ]

instance ToJSON EditMessageCaptionRequest where
  toJSON EditMessageCaptionRequest{..} =
    object' [ "chat_id"      .=! edit_caption_chat_id
            , "message_id"   .=! edit_caption_message_id
            , "caption"      .=! edit_caption_caption
            , "reply_markup" .=! edit_caption_reply_markup
            ]
  toJSON EditInlineCaptionRequest{..} =
    object' [ "inline_message_id" .=! edit_caption_inline_message_id
            , "caption"           .=! edit_caption_caption
            , "reply_markup"      .=! edit_caption_reply_markup
            ]

instance ToJSON EditMessageReplyMarkupRequest where
  toJSON (EditMessageReplyMarkupRequest chat_id message_id reply_markup) =
    object' [ "chat_id"      .=! chat_id
            , "message_id"   .=! message_id
            , "reply_markup" .=!! reply_markup
            ]
  toJSON (EditInlineReplyMarkupRequest inline_message_id reply_markup) =
    object' [ "inline_message_id" .=! inline_message_id
            , "reply_markup"      .=!! reply_markup
            ]

------------------------
-- FromJSON INSTANCES --
------------------------

instance FromJSON EditMessageTextRequest where
  parseJSON (Object o) =
    EditMessageTextRequest <$> o .: "chat_id"
                           <*> o .: "message_id"
                           <*> o .: "text"
                           <*> o .:? "disable_web_page_preview" .!= False
                           <*> o .:? "parse_mode"
                           <*> o .:? "reply_markup"
    <|> EditInlineTextRequest <$> o .: "inline_message_id"
                              <*> o .: "text"
                              <*> o .:? "disable_web_page_preview" .!= False
                              <*> o .:? "parse_mode"
                              <*> o .:? "reply_markup"
  parseJSON wat = typeMismatch "EditMessageTextRequest" wat

instance FromJSON EditMessageCaptionRequest where
  parseJSON (Object o) =
    EditMessageCaptionRequest <$> o .: "chat_id"
                              <*> o .: "message_id"
                              <*> o .: "caption"
                              <*> o .:? "reply_markup"
    <|> EditInlineCaptionRequest <$> o .: "inline_message_id"
                                 <*> o .: "caption"
                                 <*> o .:? "reply_markup"
  parseJSON wat = typeMismatch "EditMessageCaptionRequest" wat

instance FromJSON EditMessageReplyMarkupRequest where
  parseJSON (Object o) =
    EditMessageReplyMarkupRequest <$> o .: "chat_id"
                                  <*> o .: "message_id"
                                  <*> o .:? "reply_markup"
    <|> EditInlineReplyMarkupRequest <$> o .: "inline_message_id"
                                     <*> o .:? "reply_markup"
  parseJSON wat = typeMismatch "EditMessageReplyMarkupRequest" wat
