{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Telegram.Bot.Instances.UpdateRequests where


import           Control.Applicative        ((<|>))
import           Data.Aeson

import           Web.Telegram.Bot.Types.UpdateRequests
import           Web.Telegram.Bot.Types.Static
import           Web.Telegram.Bot.Instances.Inline()
import           Web.Telegram.Bot.Instances.Static()

----------------------
-- ToJSON INSTANCES --
----------------------

instance ToJSON EditMessageTextRequest where
  toJSON editMsgReq = object' $ extra ++ basis
   where
    basis = [ "text"                     .=! edit_text_text editMsgReq
            , mBool "disable_web_page_preview" False $ edit_text_disable_web_page_preview editMsgReq
            , "parse_mode"               .=! edit_text_parse_mode editMsgReq
            , "reply_markup"             .=! edit_text_reply_markup editMsgReq
            ]
    extra = case editMsgReq of
      EditMessageTextRequest{..} ->
        [ "chat_id"    .=! edit_text_chat_id
        , "message_id" .=! edit_text_message_id ]
      EditInlineTextRequest{..} ->
        [ "inline_message_id" .=! edit_text_inline_message_id ]

instance ToJSON EditMessageCaptionRequest where
  toJSON editCaptReq = object' $ extra ++ basis
   where
    basis = [ "caption"      .=! edit_caption_caption editCaptReq
            , "reply_markup" .=! edit_caption_reply_markup editCaptReq ]
    extra = case editCaptReq of
      EditMessageCaptionRequest{..} ->
        [ "chat_id"    .=! edit_caption_chat_id
        , "message_id" .=! edit_caption_message_id ]
      EditInlineCaptionRequest{..} ->
        [ "inline_message_id" .=! edit_caption_inline_message_id ]


instance ToJSON EditMessageReplyMarkupRequest where
  toJSON editMarkupReq = object' $ extra ++ [ "reply_markup" .=!! edit_reply_markup_reply_markup editMarkupReq ]
   where
    extra = case editMarkupReq of
      EditMessageReplyMarkupRequest{..} ->
        [ "chat_id"    .=! edit_reply_markup_chat_id
        , "message_id" .=! edit_reply_markup_message_id ]
      EditInlineReplyMarkupRequest{..} ->
        [ "inline_message_id" .=! edit_reply_markup_inline_message_id ]

------------------------
-- FromJSON INSTANCES --
------------------------

instance FromJSON EditMessageTextRequest where
  parseJSON = withObject "EditMessageTextRequest" $ \o ->
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

instance FromJSON EditMessageCaptionRequest where
  parseJSON = withObject "EditMessageCaptionRequest" $ \o ->
    EditMessageCaptionRequest <$> o .: "chat_id"
                              <*> o .: "message_id"
                              <*> o .: "caption"
                              <*> o .:? "reply_markup"
    <|> EditInlineCaptionRequest <$> o .: "inline_message_id"
                                 <*> o .: "caption"
                                 <*> o .:? "reply_markup"

instance FromJSON EditMessageReplyMarkupRequest where
  parseJSON = withObject "EditMessageReplyMarkupRequest" $ \o ->
    EditMessageReplyMarkupRequest <$> o .: "chat_id"
                                  <*> o .: "message_id"
                                  <*> o .:? "reply_markup"
    <|> EditInlineReplyMarkupRequest <$> o .: "inline_message_id"
                                     <*> o .:? "reply_markup"
