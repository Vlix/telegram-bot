-- | This module contains data objects which represents requests to update messages to Telegram Bot API
module Web.Telegram.Types.UpdateRequests where

import           Data.Text                  (Text (..))
import           Web.Telegram.Types.Inline
import           Web.Telegram.Types.Static


-- | This object represents request for 'editMessageText'
-- | Use this method to edit text messages sent by the bot or via the bot (for inline bots). On success, if edited message is sent by the bot, the edited Message is returned, otherwise True is returned.
data EditMessageTextRequest =
  EditMessageTextRequest
  { edit_text_chat_id                  :: Text                       -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , edit_text_message_id               :: Int                        -- ^ Unique identifier of the sent message
  , edit_text_text                     :: Text                       -- ^ New text of the message
  , edit_text_parse_mode               :: Maybe ParseMode            -- ^ Send Markdown or HTML, if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , edit_text_disable_web_page_preview :: Maybe Bool                 -- ^ Disables link previews for links in this message
  , edit_text_reply_markup             :: Maybe InlineKeyboardMarkup -- ^ A JSON-serialized object for an inline keyboard.
  }
  | EditInlineTextRequest
  { edit_text_inline_message_id        :: Int                        -- ^ Identifier of the inline message
  , edit_text_text                     :: Text                       -- ^ New text of the message
  , edit_text_parse_mode               :: Maybe ParseMode            -- ^ Send Markdown or HTML, if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , edit_text_disable_web_page_preview :: Maybe Bool                 -- ^ Disables link previews for links in this message
  , edit_text_reply_markup             :: Maybe InlineKeyboardMarkup -- ^ A JSON-serialized object for an inline keyboard.
  } deriving (Show)

-- | This object represents request for 'editMessageCaption'
-- | Use this method to edit captions of messages sent by the bot or via the bot (for inline bots). On success, if edited message is sent by the bot, the edited Message is returned, otherwise True is returned.
data EditMessageCaptionRequest =
  EditMessageCaptionRequest
  { edit_caption_chat_id      :: Text                       -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , edit_caption_message_id   :: Int                        -- ^ Unique identifier of the sent message
  , edit_caption_caption      :: Text                       -- ^ New caption of the message
  , edit_caption_reply_markup :: Maybe InlineKeyboardMarkup -- ^ A JSON-serialized object for an inline keyboard.
  }
  | EditInlineCaptionRequest
  { edit_caption_inline_message_id :: Text                       -- ^ Identifier of the inline message
  , edit_caption_caption           :: Text                       -- ^ New caption of the message
  , edit_caption_reply_markup      :: Maybe InlineKeyboardMarkup -- ^ A JSON-serialized object for an inline keyboard.
  } deriving (Show)

-- | This object represents request for 'editMessageReplyMarkup'
-- | Use this method to edit only the reply markup of messages sent by the bot or via the bot (for inline bots). On success, if edited message is sent by the bot, the edited Message is returned, otherwise True is returned. 
data EditMessageReplyMarkupRequest =
  EditMessageReplyMarkupRequest
  { edit_reply_markup_chat_id      :: Text                       -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , edit_reply_markup_message_id   :: Int                        -- ^ Unique identifier of the sent message
  , edit_reply_markup_reply_markup :: Maybe InlineKeyboardMarkup -- ^ A JSON-serialized object for an inline keyboard.
  }
  | EditInlineReplyMarkupRequest
  { edit_reply_markup_inline_message_id :: Text                       -- ^ Identifier of the inline message
  , edit_reply_markup_reply_markup      :: Maybe InlineKeyboardMarkup -- ^  A JSON-serialized object for an inline keyboard.
  } deriving (Show)
