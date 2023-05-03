{-# LANGUAGE DerivingStrategies #-}
module Web.Telegram.Bot.Types.Inline where


import           Data.Text                  (Text)

import           Web.Telegram.Bot.Types.Basic
import           Web.Telegram.Bot.Types.Static

-- | This object represents an incoming inline query. When the user sends an empty query, your bot could return some default or trending results.
data InlineQuery = InlineQuery
  { query_id        :: Text           -- ^ Unique identifier for this query
  , query_from      :: User           -- ^ Sender
  , query_query     :: Text           -- ^ Text of the query (up to 512 characters)
  , query_offset    :: Text           -- ^ Offset of the results to be returned, can be controlled by the bot
  , query_location  :: Maybe Location -- ^ Sender location, only for bots that request user location
  } deriving stock (Eq, Show)

-- | This object represents a result of an inline query that was chosen by the user and sent to their chat partner.
data ChosenInlineResult = ChosenInlineResult
  { chosen_result_id         :: Text           -- ^ Unique identifier for this query
  , chosen_from              :: User           -- ^ Sender
  , chosen_query             :: Text           -- ^ Text of the query
  , chosen_inline_message_id :: Maybe Text     -- ^ Identifier of the sent inline message. Available only if there is an inline keyboard attached to the message. Will be also received in callback queries and can be used to edit the message.
  , chosen_location          :: Maybe Location -- ^ Sender location, only for bots that request user location
  } deriving stock (Eq, Show)

-- because everything here uses only the InlineKeyboardMarkup we can't just use ReplyKeyboard
newtype InlineKeyboardMarkup =
  IKM { ikm_reply_inline_keyboard :: [[InlineKeyboardButton]] } -- ^ Array of button rows, each represented by an Array of InlineKeyboardButton objects
    deriving stock (Eq, Show)

-- | This object represents the content of a message to be sent as a result of an inline query.
data InputMessageContent =
  -- | Represents the content of a text message to be sent as the result of an inline query.
  InputTextMessageContent
  { input_message_text       :: Text            -- ^ Text of the message to be sent, 1-4096 characters
  , parse_mode               :: Maybe ParseMode -- ^ Send Markdown or HTML, if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , disable_web_page_preview :: Bool      -- ^ Disables link previews for links in the sent message
  }
  -- | Represents the content of a venue message to be sent as the result of an inline query.
  | InputVenueMessageContent
  { latitude      :: Double     -- ^ Latitude of the venue in degrees
  , longitude     :: Double     -- ^ Longitude of the venue in degrees
  , title         :: Text       -- ^ Name of the venue
  , address       :: Text       -- ^ Address of the venue
  , foursquare_id :: Maybe Text -- ^ Foursquare identifier of the venue, if known
  }
  -- | Represents the content of a location message to be sent as the result of an inline query.
  | InputLocationMessageContent
  { latitude  :: Double -- ^ Latitude of the location in degrees
  , longitude :: Double -- ^ Longitude of the location in degrees
  }
  -- | Represents the content of a contact message to be sent as the result of an inline query.
  | InputContactMessageContent
  { phone_number :: Text       -- ^ Contact's phone number
  , first_name   :: Text       -- ^ Contact's first name
  , last_name    :: Maybe Text -- ^ Contact's last name
  } deriving stock (Eq, Show)

data InlineQueryResult =
  -- | Represents a link to an article or web page.
  InlineQueryResultArticle
  { iqr_id                            :: Text                       -- ^ Unique identifier for this result, 1-64 Bytes
  , iqr_title                         :: Text                       -- ^ Title of the result
  , iqr_article_input_message_content :: InputMessageContent        -- ^ Content of the message to be sent
  , iqr_reply_markup                  :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iqr_url                           :: Maybe Text                 -- ^ URL of the result
  , iqr_hide_url                      :: Bool                       -- ^ Pass True, if you don't want the URL to be shown in the message
  , iqr_description                   :: Maybe Text                 -- ^ Short description of the result
  , iqr_thumb_url                     :: Maybe Text                 -- ^ Url of the thumbnail for the result
  , iqr_thumb_width                   :: Maybe Int                  -- ^ Thumbnail width
  , iqr_thumb_height                  :: Maybe Int                  -- ^ Thumbnail height
  }
  -- | Represents a link to a photo. By default, this photo will be sent by the user with optional caption.
  -- | Alternatively, you can use input_message_content to send a message with the specified content instead of the photo.
  | InlineQueryResultPhoto
  { iqr_id                    :: Text                       -- ^ Unique identifier for this result, 1-64 bytes
  , iqr_photo_url             :: Text                       -- ^ A valid URL of the photo. Photo must be in jpeg format. Photo size must not exceed 5MB
  , iqr_photo_thumb_url       :: Text                       -- ^ URL of the thumbnail for the photo
  , iqr_photo_width           :: Maybe Int                  -- ^ Width of the photo
  , iqr_photo_height          :: Maybe Int                  -- ^ Height of the photo
  , iqr_photo_title           :: Maybe Text                 -- ^ Title for the result
  , iqr_description           :: Maybe Text                 -- ^ Short description of the result
  , iqr_caption               :: Maybe Text                 -- ^ Caption of the photo to be sent, 0-200 characters
  , iqr_reply_markup          :: Maybe InlineKeyboardMarkup -- ^ Text of a message to be sent instead of the photo, 1-512 characters
  , iqr_input_message_content :: Maybe InputMessageContent  -- ^ Text of a message to be sent instead of the photo, 1-512 characters
  }
  -- | Represents a link to an animated GIF file. By default, this animated GIF file will be sent by the user with optional caption.
  -- | Alternatively, you can use input_message_content to send a message with the specified content instead of the animation.
  | InlineQueryResultGif
  { iqr_id                    :: Text                       -- ^ Unique identifier for this result, 1-64 bytes
  , iqr_gif_url               :: Text                       -- ^ A valid URL for the GIF file. File size must not exceed 1MB
  , iqr_gif_width             :: Maybe Int                  -- ^ Width of the GIF
  , iqr_gif_height            :: Maybe Int                  -- ^ Height of the GIF
  , iqr_gif_thumb_url         :: Text                       -- ^ URL of the static thumbnail for the result (jpeg or gif)
  , iqr_gif_title             :: Maybe Text                 -- ^ Title for the result
  , iqr_caption               :: Maybe Text                 -- ^ Caption of the GIF file to be sent, 0-200 characters
  , iqr_reply_markup          :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iqr_input_message_content :: Maybe InputMessageContent  -- ^ Content of the message to be sent instead of the GIF animation
  }
  -- | Represents a link to a video animation (H.264/MPEG-4 AVC video without sound). By default, this animated MPEG-4 file will be sent by the user with optional caption.
  -- | Alternatively, you can use input_message_content to send a message with the specified content instead of the animation.
  | InlineQueryResultMpeg4Gif
  { iqr_id                    :: Text                       -- ^ Unique identifier for this result, 1-64 bytes
  , iqr_mpeg4_url             :: Text                       -- ^ A valid URL for the MP4 file. File size must not exceed 1MB
  , iqr_mpeg4_width           :: Maybe Int                  -- ^ Video width
  , iqr_mpeg4_height          :: Maybe Int                  -- ^ Video height
  , iqr_mpeg4_thumb_url       :: Text                       -- ^ URL of the static thumbnail (jpeg or gif) for the result
  , iqr_mpeg4_title           :: Maybe Text                 -- ^ Title for the result
  , iqr_caption               :: Maybe Text                 -- ^ Caption of the MPEG-4 file to be sent, 0-200 characters
  , iqr_reply_markup          :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iqr_input_message_content :: Maybe InputMessageContent  -- ^ Content of the message to be sent instead of the video animation
  }
  -- | Represents a link to a page containing an embedded video player or a video file. By default, this video file will be sent by the user with an optional caption.
  -- | Alternatively, you can use input_message_content to send a message with the specified content instead of the video.
  | InlineQueryResultVideo
  { iqr_id                    :: Text                       -- ^ Unique identifier for this result, 1-64 bytes
  , iqr_video_url             :: Text                       -- ^ A valid URL for the embedded video player or video file
  , iqr_video_mime_type       :: VideoMIME                  -- ^ Mime type of the content of video url, “text/html” or “video/mp4”
  , iqr_video_thumb_url       :: Text                       -- ^ URL of the thumbnail (jpeg only) for the video
  , iqr_title                 :: Text                       -- ^ Title for the result
  , iqr_caption               :: Maybe Text                 -- ^ Caption of the video to be sent, 0-200 characters
  , iqr_video_width           :: Maybe Int                  -- ^ Video width
  , iqr_video_height          :: Maybe Int                  -- ^ Video height
  , iqr_video_duration        :: Maybe Int                  -- ^ Video duration in seconds
  , iqr_description           :: Maybe Text                 -- ^ Short description of the result
  , iqr_reply_markup          :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iqr_input_message_content :: Maybe InputMessageContent  -- ^ Content of the message to be sent instead of the video animation
  }
  -- | Represents a link to an mp3 audio file. By default, this audio file will be sent by the user.
  -- | Alternatively, you can use input_message_content to send a message with the specified content instead of the audio.
  | InlineQueryResultAudio
  { iqr_id                    :: Text                       -- ^ Unique identifier for this result, 1-64 bytes
  , iqr_audio_url             :: Text                       -- ^ A valid URL for the audio file
  , iqr_title                 :: Text                       -- ^ Title
  , iqr_caption               :: Maybe Text                 -- ^ Caption, 0-200 characters
  , iqr_performer             :: Maybe Text                 -- ^ Performer
  , iqr_audio_duration        :: Maybe Int                  -- ^ Audio duration in seconds
  , iqr_reply_markup          :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iqr_input_message_content :: Maybe InputMessageContent  -- ^ Content of the message to be sent instead of the audio
  }
  -- | Represents a link to a voice recording in an .ogg container encoded with OPUS. By default, this voice recording will be sent by the user.
  -- | Alternatively, you can use input_message_content to send a message with the specified content instead of the the voice message.
  | InlineQueryResultVoice
  { iqr_id                    :: Text                       -- ^ Unique identifier for this result, 1-64 bytes
  , iqr_voice_url             :: Text                       -- ^ A valid URL for the voice recording
  , iqr_title                 :: Text                       -- ^ Recording title
  , iqr_caption               :: Maybe Text                 -- ^ Caption, 0-200 characters
  , iqr_voice_duration        :: Maybe Int                  -- ^ Recording duration in seconds
  , iqr_reply_markup          :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iqr_input_message_content :: Maybe InputMessageContent  -- ^ Content of the message to be sent instead of the audio
  }
  -- | Represents a link to a file. By default, this file will be sent by the user with an optional caption.
  -- | Alternatively, you can use input_message_content to send a message with the specified content instead of the file. Currently, only .PDF and .ZIP files can be sent using this method.
  | InlineQueryResultDocument
  { iqr_id                    :: Text                       -- ^ Unique identifier for this result, 1-64 bytes
  , iqr_title                 :: Text                       -- ^ Title for the result
  , iqr_caption               :: Maybe Text                 -- ^ Caption of the document to be sent, 0-200 characters
  , iqr_document_url          :: Text                       -- ^ A valid URL for the file
  , iqr_document_mime_type    :: DocumentMIME               -- ^ Mime type of the content of the file, either “application/pdf” or “application/zip”
  , iqr_description           :: Maybe Text                 -- ^ Short description of the result
  , iqr_reply_markup          :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iqr_input_message_content :: Maybe InputMessageContent  -- ^ Content of the message to be sent instead of the file
  , iqr_thumb_url             :: Maybe Text                 -- ^ URL of the thumbnail (jpeg only) for the file
  , iqr_thumb_width           :: Maybe Int                  -- ^ Thumbnail width
  , iqr_thumb_height          :: Maybe Int                  -- ^ Thumbnail height
  }
  -- | Represents a location on a map. By default, the location will be sent by the user.
  -- | Alternatively, you can use input_message_content to send a message with the specified content instead of the location.
  | InlineQueryResultLocation
  { iqr_id                    :: Text                       -- ^ Unique identifier for this result, 1-64 bytes
  , iqr_latitude              :: Double                     -- ^ Location latitude in degrees
  , iqr_longitude             :: Double                     -- ^ Location longitude in degrees
  , iqr_title                 :: Text                       -- ^ Location title
  , iqr_reply_markup          :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iqr_input_message_content :: Maybe InputMessageContent  -- ^ Content of the message to be sent instead of the location
  , iqr_thumb_url             :: Maybe Text                 -- ^ Url of the thumbnail for the result
  , iqr_thumb_width           :: Maybe Int                  -- ^ Thumbnail width
  , iqr_thumb_height          :: Maybe Int                  -- ^ Thumbnail height
  }
  -- | Represents a venue. By default, the venue will be sent by the user.
  -- | Alternatively, you can use input_message_content to send a message with the specified content instead of the venue.
  | InlineQueryResultVenue
  { iqr_id                    :: Text                       -- ^ Unique identifier for this result, 1-64 bytes
  , iqr_latitude              :: Double                     -- ^ Latitude of the venue location in degrees
  , iqr_longitude             :: Double                     -- ^ Longitude of the venue location in degrees
  , iqr_title                 :: Text                       -- ^ Title of the venue
  , iqr_address               :: Text                       -- ^ Address of the venue
  , iqr_foursquare_id         :: Maybe Text                 -- ^ Foursquare identifier of the venue if known
  , iqr_reply_markup          :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iqr_input_message_content :: Maybe InputMessageContent  -- ^ Content of the message to be sent instead of the venue
  , iqr_thumb_url             :: Maybe Text                 -- ^ Url of the thumbnail for the result
  , iqr_thumb_width           :: Maybe Int                  -- ^ Thumbnail width
  , iqr_thumb_height          :: Maybe Int                  -- ^ Thumbnail height
  }
  -- | Represents a contact with a phone number. By default, this contact will be sent by the user.
  -- | Alternatively, you can use input_message_content to send a message with the specified content instead of the contact.
  | InlineQueryResultContact
  { iqr_id                    :: Text                       -- ^ Unique identifier for this result, 1-64 bytes
  , iqr_phone_number          :: Text                       -- ^ Contact's phone number
  , iqr_first_name            :: Text                       -- ^ Contact's first name
  , iqr_last_name             :: Maybe Text                 -- ^ Contact's last name
  , iqr_reply_markup          :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iqr_input_message_content :: Maybe InputMessageContent  -- ^ Content of the message to be sent instead of the venue
  , iqr_thumb_url             :: Maybe Text                 -- ^ Url of the thumbnail for the result
  , iqr_thumb_width           :: Maybe Int                  -- ^ Thumbnail width
  , iqr_thumb_height          :: Maybe Int                  -- ^ Thumbnail height
  }
  -- | Represents a link to a photo stored on the Telegram servers. By default, this photo will be sent by the user with an optional caption.
  -- | Alternatively, you can use input_message_content to send a message with the specified content instead of the photo.
  | InlineQueryResultGame
  { iqr_id              :: Text                       -- ^ Unique identifier for this result, 1-64 bytes
  , iqr_game_short_name :: Text                       -- ^ Short name of the game
  , iqr_reply_markup    :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  }
  | InlineQueryResultCachedPhoto
  { iqrc_id                    :: Text                       -- ^ Unique identifier for this result, 1-64 bytes
  , iqrc_photo_file_id         :: Text                       -- ^ A valid file identifier of the photo
  , iqrc_photo_title           :: Maybe Text                 -- ^ Title for the result
  , iqrc_description           :: Maybe Text                 -- ^ Short description of the result
  , iqrc_caption               :: Maybe Text                 -- ^ Caption of the photo to be sent, 0-200 characters
  , iqrc_reply_markup          :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iqrc_input_message_content :: Maybe InputMessageContent  -- ^ Content of the message to be sent instead of the photo
  }
  -- | Represents a link to an animated GIF file stored on the Telegram servers. By default, this animated GIF file will be sent by the user with an optional caption.
  -- | Alternatively, you can use input_message_content to send a message with specified content instead of the animation.
  | InlineQueryResultCachedGif
  { iqrc_id                    :: Text                       -- ^ Unique identifier for this result, 1-64 bytes
  , iqrc_gif_file_id           :: Text                       -- ^ A valid file identifier for the GIF file
  , iqrc_gif_title             :: Maybe Text                 -- ^ Title for the result
  , iqrc_caption               :: Maybe Text                 -- ^ Caption of the GIF file to be sent, 0-200 characters
  , iqrc_reply_markup          :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iqrc_input_message_content :: Maybe InputMessageContent  -- ^ Content of the message to be sent instead of the GIF animation
  }
  -- | Represents a link to a video animation (H.264/MPEG-4 AVC video without sound) stored on the Telegram servers. By default, this animated MPEG-4 file will be sent by the user with an optional caption.
  -- | Alternatively, you can use input_message_content to send a message with the specified content instead of the animation.
  | InlineQueryResultCachedMpeg4Gif
  { iqrc_id                    :: Text                       -- ^ Unique identifier for this result, 1-64 bytes
  , iqrc_mpeg4_file_id         :: Text                       -- ^ A valid file identifier for the MP4 file
  , iqrc_mpeg4_title           :: Maybe Text                 -- ^ Title for the result
  , iqrc_caption               :: Maybe Text                 -- ^ Caption of the MPEG-4 file to be sent, 0-200 characters
  , iqrc_reply_markup          :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iqrc_input_message_content :: Maybe InputMessageContent  -- ^ Content of the message to be sent instead of the video animation
  }
  -- | Represents a link to a sticker stored on the Telegram servers. By default, this sticker will be sent by the user.
  -- | Alternatively, you can use input_message_content to send a message with the specified content instead of the sticker.
  | InlineQueryResultCachedSticker
  { iqrc_id                    :: Text                       -- ^ Unique identifier for this result, 1-64 bytes
  , iqrc_sticker_file_id       :: Text                       -- ^ A valid file identifier for the sticker
  , iqrc_reply_markup          :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iqrc_input_message_content :: Maybe InputMessageContent  -- ^ Content of the message to be sent instead of the video animation
  }
  -- | Represents a link to a file. By default, this file will be sent by the user with an optional caption.
  -- | Alternatively, you can use input_message_content to send a message with the specified content instead of the file. Currently, only .PDF and .ZIP files can be sent using this method.
  | InlineQueryResultCachedDocument
  { iqrc_id                    :: Text                       -- ^ Unique identifier for this result, 1-64 bytes
  , iqrc_title                 :: Text                       -- ^ Title for the result
  , iqrc_document_file_id      :: Text                       -- ^ A valid file identifier for the file
  , iqrc_description           :: Maybe Text                 -- ^ Short description of the result
  , iqrc_caption               :: Maybe Text                 -- ^ Caption of the document to be sent, 0-200 characters
  , iqrc_reply_markup          :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iqrc_input_message_content :: Maybe InputMessageContent  -- ^ Content of the message to be sent instead of the file
  }
  -- | Represents a link to a video file stored on the Telegram servers. By default, this video file will be sent by the user with an optional caption.
  -- | Alternatively, you can use input_message_content to send a message with the specified content instead of the video.
  | InlineQueryResultCachedVideo
  { iqrc_id                    :: Text                       -- ^ Unique identifier for this result, 1-64 bytes
  , iqrc_video_file_id         :: Text                       -- ^ A valid file identifier for the video file
  , iqrc_title                 :: Text                       -- ^ Title for the result
  , iqrc_description           :: Maybe Text                 -- ^ Short description of the result
  , iqrc_caption               :: Maybe Text                 -- ^ Caption of the video to be sent, 0-200 characters
  , iqrc_reply_markup          :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iqrc_input_message_content :: Maybe InputMessageContent  -- ^ Content of the message to be sent instead of the video animation
  }
  -- | Represents a link to a voice message stored on the Telegram servers. By default, this voice message will be sent by the user.
  -- | Alternatively, you can use input_message_content to send a message with the specified content instead of the voice message.
  | InlineQueryResultCachedVoice
  { iqrc_id                    :: Text                       -- ^ Unique identifier for this result, 1-64 bytes
  , iqrc_voice_file_id         :: Text                       -- ^ A valid file identifier for the voice message
  , iqrc_title                 :: Text                       -- ^ Voice message title
  , iqrc_caption               :: Maybe Text                 -- ^ Caption, 0-200 characters
  , iqrc_reply_markup          :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iqrc_input_message_content :: Maybe InputMessageContent  -- ^ Content of the message to be sent instead of the audio
  }
  -- | Represents a link to an mp3 audio file stored on the Telegram servers. By default, this audio file will be sent by the user.
  -- | Alternatively, you can use input_message_content to send a message with the specified content instead of the audio.
  | InlineQueryResultCachedAudio
  { iqrc_id                    :: Text                       -- ^ Unique identifier for this result, 1-64 bytes
  , iqrc_audio_file_id         :: Text                       -- ^ A valid file identifier for the audio file
  , iqrc_caption               :: Maybe Text                 -- ^ Caption, 0-200 characters
  , iqrc_reply_markup          :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iqrc_input_message_content :: Maybe InputMessageContent  -- ^ Content of the message to be sent instead of the audio
  } deriving stock (Eq, Show)
