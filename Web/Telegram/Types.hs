{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

-- | This module contains objects which represent data of Telegram Bot API responses
module Web.Telegram.Types
    ( module Web.Telegram.Types.Basic
    , module Web.Telegram.Types.Static
    , module Web.Telegram.Types.Inline
    , module Web.Telegram.Instances.Basic
    , module Web.Telegram.Instances.Static
    , module Web.Telegram.Instances.Inline
    -- * Types
    , Update                (..)
    ) where

import           Control.Applicative        ((<|>))
import           Data.Aeson
import           Data.Text                  (Text (..))
import           Data.Aeson.Types           (typeMismatch)

import           Web.Telegram.Types.Basic
import           Web.Telegram.Types.Static
import           Web.Telegram.Types.Inline
import           Web.Telegram.Instances.Basic
import           Web.Telegram.Instances.Static
import           Web.Telegram.Instances.Inline


-- | This object represents an incoming update.
data Update =
  MessageUpdate
  { update_id :: Int     -- ^ The update's unique identifier. Update identifiers start from a certain positive number and increase sequentially. This ID becomes especially handy if you’re using 'setWebhooks', since it allows you to ignore repeated updates or to restore the correct update sequence, should they get out of order.
  , message   :: Message -- ^ New incoming message of any kind — text, photo, sticker, etc.
  }
  | InlineQueryUpdate
  { update_id    :: Int
  , inline_query :: InlineQuery -- ^ New incoming inline query
  }
  | ChosenInlineUpdate
  { update_id            :: Int
  , chosen_inline_result :: ChosenInlineResult -- ^ The result of a inline query that was chosen by a user and sent to their chat partner
  }
  | CallbackUpdate
  { update_id      :: Int
  , callback_query :: CallbackQuery -- ^ New incoming callback query
  } deriving (Show)

{-
data InlineQueryResult =
  -- | Represents a link to an article or web page.
  InlineQueryResultArticle
  { iq_res_id                              :: Text -- ^ Unique identifier for this result, 1-64 Bytes
  , iq_res_title                           :: Maybe Text -- ^ Title of the result
  , iq_res_message_text                    :: Maybe Text -- ^ Text of the message to be sent
  , iq_res_parse_mode                      :: Maybe ParseMode -- Send 'Markdown', if you want Telegram apps to show bold, italic and inline URLs in your bot's message.
  , iq_res_disable_web_page_preview        :: Maybe Bool -- ^ Disables link previews for links in the sent message
  , iq_res_url                             :: Maybe Text -- ^ URL of the result
  , iq_res_hide_url                        :: Maybe Bool -- ^ Pass True, if you don't want the URL to be shown in the message
  , iq_res_description                     :: Maybe Text -- ^ Short description of the result
  , iq_res_thumb_url                       :: Maybe Text -- ^ Url of the thumbnail for the result
  , iq_res_thumb_width                     :: Maybe Int -- ^ Thumbnail width
  , iq_res_thumb_height                    :: Maybe Int -- ^ Thumbnail height
  }
  -- | Represents a link to a photo. By default, this photo will be sent by the user with optional caption. Alternatively, you can provide message_text to send it instead of photo.
  | InlineQueryResultPhoto
  { iq_res_id                              :: Text -- ^ Unique identifier for this result, 1-64 bytes
  , iq_res_photo_url                       :: Text -- ^ A valid URL of the photo. Photo must be in jpeg format. Photo size must not exceed 5MB
  , iq_res_photo_width                     :: Maybe Int -- ^ Optional. Width of the photo
  , iq_res_photo_height                    :: Maybe Int -- ^ Optional. Height of the photo
  , iq_res_thumb_url                       :: Maybe Text -- ^ URL of the thumbnail for the photo
  , iq_res_title                           :: Maybe Text -- ^ Title for the result
  , iq_res_description                     :: Maybe Text -- ^ Short description of the result
  , iq_res_caption                         :: Maybe Text -- ^ Caption of the photo to be sent, 0-200 characters
  , iq_res_message_text                    :: Maybe Text -- ^ Text of a message to be sent instead of the photo, 1-512 characters
  , iq_res_parse_mode                      :: Maybe ParseMode -- ^ Send 'Markdown', if you want Telegram apps to show bold, italic and inline URLs in your bot's message.
  , iq_res_disable_web_page_preview        :: Maybe Bool -- ^ Disables link previews for links in the sent message
  }
  -- | Represents a link to an animated GIF file. By default, this animated GIF file will be sent by the user with optional caption. Alternatively, you can provide message_text to send it instead of the animation.
  | InlineQueryResultGif
  { iq_res_id                              :: Text -- ^ Unique identifier for this result, 1-64 bytes
  , iq_res_gif_url                         :: Text -- ^ A valid URL for the GIF file. File size must not exceed 1MB
  , iq_res_gif_width                       :: Maybe Int -- ^ Width of the GIF
  , iq_res_gif_height                      :: Maybe Int -- ^ Height of the GIF
  , iq_res_thumb_url                       :: Maybe Text -- ^ URL of the static thumbnail for the result (jpeg or gif)
  , iq_res_title                           :: Maybe Text -- ^ Title for the result
  , iq_res_caption                         :: Maybe Text -- ^ Caption of the GIF file to be sent, 0-200 characters
  , iq_res_message_text                    :: Maybe Text -- ^ Text of a message to be sent instead of the animation, 1-512 characters
  , iq_res_parse_mode                      :: Maybe ParseMode -- ^ Send 'Markdown', if you want Telegram apps to show bold, italic and inline URLs in your bot's message.
  , iq_res_disable_web_page_preview        :: Maybe Bool -- ^ Disables link previews for links in the sent message
  }
  -- | Represents a link to a video animation (H.264/MPEG-4 AVC video without sound). By default, this animated MPEG-4 file will be sent by the user with optional caption. Alternatively, you can provide message_text to send it instead of the animation.
  | InlineQueryResultMpeg4Gif
  { iq_res_id                              :: Text -- ^ Unique identifier for this result, 1-64 bytes
  , iq_res_mpeg4_url                       :: Text -- ^ A valid URL for the MP4 file. File size must not exceed 1MB
  , iq_res_mpeg4_width                     :: Maybe Int -- ^ Video width
  , iq_res_mpeg4_height                    :: Maybe Int -- ^ Video height
  , iq_res_thumb_url                       :: Maybe Text -- ^ URL of the static thumbnail (jpeg or gif) for the result
  , iq_res_title                           :: Maybe Text -- ^ Title for the result
  , iq_res_caption                         :: Maybe Text -- ^ Caption of the MPEG-4 file to be sent, 0-200 characters
  , iq_res_message_text                    :: Maybe Text -- ^ Text of a message to be sent instead of the animation, 1-512 characters
  , iq_res_parse_mode                      :: Maybe ParseMode -- ^ Send 'Markdown', if you want Telegram apps to show bold, italic and inline URLs in your bot's message.
  , iq_res_disable_web_page_preview        :: Maybe Bool -- ^ Disables link previews for links in the sent message
  }
  -- | Represents link to a page containing an embedded video player or a video file.
  | InlineQueryResultVideo
  { iq_res_id                              :: Text -- ^ Unique identifier for this result, 1-64 bytes
  , iq_res_video_url                       :: Text -- ^ A valid URL for the embedded video player or video file
  , iq_res_mime_type                       :: Text -- ^ Mime type of the content of video url, “text/html” or “video/mp4”
  , iq_res_message_text                    :: Maybe Text -- ^ Text of the message to be sent with the video, 1-512 characters
  , iq_res_parse_mode                      :: Maybe ParseMode -- ^ Send 'Markdown', if you want Telegram apps to show bold, italic and inline URLs in your bot's message.
  , iq_res_disable_web_page_preview        :: Maybe Bool -- ^ Disables link previews for links in the sent message
  , iq_res_video_width                     :: Maybe Int -- ^ Video width
  , iq_res_video_height                    :: Maybe Int -- ^ Video height
  , iq_res_video_duration                  :: Maybe Int -- ^ Video duration in seconds
  , iq_res_thumb_url                       :: Maybe Text -- ^ URL of the thumbnail (jpeg only) for the video
  , iq_res_title                           :: Maybe Text -- ^ Title for the result
  , iq_res_description                     :: Maybe Text -- ^ Short description of the result
  } deriving (Show, Read, Generic)

tagModifier "InlineQueryResultMpeg4Gif" = "mpeg4_gif"
tagModifier x = ((drop 17) . (fmap (Char.toLower))) x
-}


--------------------
-- JSON INSTANCES --
--------------------


instance ToJSON Update where
    toJSON (MessageUpdate update_id message) = object [ "update_id" .= update_id
                                                      , "message"   .= message
                                                      ]
    toJSON (InlineQueryUpdate update_id inline_query) = object [ "update_id"    .= update_id
                                                               , "inline_query" .= inline_query
                                                               ]
    toJSON (ChosenInlineUpdate update_id chosen_inline_result) = object [ "update_id" .= update_id
                                                                        , "chosen_inline_result" .= chosen_inline_result
                                                                        ]
    toJSON (CallbackUpdate update_id callback_query) = object [ "update_id" .= update_id
                                                              , "callback_query" .= callback_query
                                                              ]

instance FromJSON Update where
  parseJSON (Object o) = MessageUpdate <$> o .: "update_id"
                                       <*> o .: "message"
                     <|> InlineQueryUpdate <$> o .: "update_id"
                                           <*> o .: "inline_query"
                     <|> ChosenInlineUpdate <$> o .: "update_id"
                                            <*> o .: "chosen_inline_result"
                     <|> CallbackUpdate <$> o .: "update_id"
                                        <*> o .: "callback_query"
  parseJSON wat = typeMismatch "Update" wat