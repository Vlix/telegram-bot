-- | This module contains data objects which represents requests to Telegram Bot API
module Web.Telegram.Types.Requests where

import           Data.Text                  (Text (..))
import           Web.Telegram.Types.Basic
import           Web.Telegram.Types.Inline
import           Web.Telegram.Types.Static

-- | This object represents request for 'sendMessage'
-- | Use this method to send text messages. On success, the sent Message is returned.
data SendMessageRequest = SendMessageRequest
  { message_chat_id                  :: Text                -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , message_text                     :: Text                -- ^ Text of the message to be sent
  , message_disable_web_page_preview :: Bool                -- ^ Disables link previews for links in this message
  , message_disable_notification     :: Bool                -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , message_parse_mode               :: Maybe ParseMode     -- ^ Send 'Markdown', if you want Telegram apps to show bold, italic and inline URLs in your bot's message
  , message_reply_to_message_id      :: Maybe Int           -- ^ If the message is a reply, ID of the original message
  , message_reply_markup             :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Eq, Show)

-- | This object represents request for 'forwardMessage'
-- | Use this method to forward messages of any kind. On success, the sent Message is returned.
data ForwardMessageRequest = ForwardMessageRequest
  { forward_chat_id              :: Text -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , forward_from_chat_id         :: Text -- ^ Unique identifier for the chat where the original message was sent (or channel username in the format @@channelusername@)
  , forward_disable_notification :: Bool -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , forward_message_id           :: Int  -- ^ Unique message identifier
  } deriving (Eq, Show)

-- | This object represents request for 'sendPhoto'
-- | Use this method to send photos. On success, the sent Message is returned.
data SendPhotoRequest = SendPhotoRequest
  { req_photo_chat_id              :: Text                -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , req_photo_photo                :: Text                -- ^ Photo to send. Pass a file_id as String to resend a photo that is already on the Telegram servers
  , req_photo_disable_notification :: Bool                -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , req_photo_caption              :: Maybe Text          -- ^ Photo caption (may also be used when resending photos by file_id), 0-200 characters.
  , req_photo_reply_to_message_id  :: Maybe Int           -- ^ If the message is a reply, ID of the original message
  , req_photo_reply_markup         :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Eq, Show)

-- | This object represents request for 'sendAudio'
-- | Use this method to send audio files, if you want Telegram clients to display them in the music player. Your audio must be in the .mp3 format. On success, the sent Message is returned.
-- | Bots can currently send audio files of up to 50 MB in size, this limit may be changed in the future. For sending voice messages, use the sendVoice method instead.
data SendAudioRequest = SendAudioRequest
  { req_audio_chat_id              :: Text                -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , req_audio_audio                :: Text                -- ^ Audio file to send. Pass a file_id as String to resend an audio that is already on the Telegram servers.
  , req_audio_disable_notification :: Bool                -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , req_audio_caption              :: Maybe Text          -- ^ Audio caption, 0-200 characters
  , req_audio_duration             :: Maybe Int           -- ^ Duration of the audio in seconds
  , req_audio_performer            :: Maybe Text          -- ^ Performer
  , req_audio_title                :: Maybe Text          -- ^ Track name
  , req_audio_reply_to_message_id  :: Maybe Int           -- ^ If the message is a reply, ID of the original message
  , req_audio_reply_markup         :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Eq, Show)

-- | This object represents request for 'sendSticker'
-- | Use this method to send .webp stickers. On success, the sent Message is returned.
data SendStickerRequest = SendStickerRequest
  { req_sticker_chat_id              :: Text                -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , req_sticker_sticker              :: Text                -- ^ Sticker to send. A file_id as String to resend a sticker that is already on the Telegram servers
  , req_sticker_disable_notification :: Bool                -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , req_sticker_reply_to_message_id  :: Maybe Int           -- ^ If the message is a reply, ID of the original message
  , req_sticker_reply_markup         :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Eq, Show)

-- | This object represents request for 'sendDocument'
-- | Use this method to send general files. On success, the sent Message is returned. Bots can currently send files of any type of up to 50 MB in size, this limit may be changed in the future.
data SendDocumentRequest = SendDocumentRequest
  { req_doc_chat_id              :: Text                -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , req_doc_document             :: Text                -- ^ File to send. A file_id as String to resend a file that is already on the Telegram servers
  , req_doc_disable_notification :: Bool                -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , req_doc_caption              :: Maybe Text          -- ^ Document caption (may also be used when resending documents by file_id), 0-200 characters
  , req_doc_reply_to_message_id  :: Maybe Int           -- ^ If the message is a reply, ID of the original message
  , req_doc_reply_markup         :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Eq, Show)

-- | This object represents request for 'sendVideo'
data SendVideoRequest = SendVideoRequest
  { req_video_chat_id              :: Text                -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , req_video_video                :: Text                -- ^ Video to send. A file_id as String to resend a video that is already on the Telegram servers
  , req_video_disable_notification :: Bool                -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , req_video_duration             :: Maybe Int           -- ^ Duration of sent video in seconds
  , req_video_width                :: Maybe Int           -- ^ Video width
  , req_video_height               :: Maybe Int           -- ^ Video height
  , req_video_caption              :: Maybe Text          -- ^ Video caption, 0-200 characters.
  , req_video_reply_to_message_id  :: Maybe Int           -- ^ If the message is a reply, ID of the original message
  , req_video_reply_markup         :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Eq, Show)

-- | This object represents request for 'sendVoice'
-- | Use this method to send audio files, if you want Telegram clients to display the file as a playable voice message. For this to work, your audio must be in an .ogg file encoded with OPUS (other formats may be sent as Audio or Document).
-- | On success, the sent Message is returned. Bots can currently send voice messages of up to 50 MB in size, this limit may be changed in the future.
data SendVoiceRequest = SendVoiceRequest
  { req_voice_chat_id              :: Text                -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , req_voice_voice                :: Text                -- ^ Audio file to send. A file_id as String to resend an audio that is already on the Telegram servers
  , req_voice_disable_notification :: Bool                -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , req_voice_caption              :: Maybe Text          -- ^ Voice message caption, 0-200 characters
  , req_voice_duration             :: Maybe Int           -- ^ Duration of sent audio in seconds
  , req_voice_reply_to_message_id  :: Maybe Int           -- ^ If the message is a reply, ID of the original message
  , req_voice_reply_markup         :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Eq, Show)

-- | This object represents request for 'sendLocation'
-- | Use this method to send point on the map. On success, the sent Message is returned.
data SendLocationRequest = SendLocationRequest
  { req_location_chat_id              :: Text                -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , req_location_latitude             :: Double              -- ^ Latitude of location
  , req_location_longitude            :: Double              -- ^ Longitude of location
  , req_location_disable_notification :: Bool                -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , req_location_reply_to_message_id  :: Maybe Int           -- ^ If the message is a reply, ID of the original message
  , req_location_reply_markup         :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Eq, Show)

-- | This object represents request for 'sendVenue'
-- | Use this method to send information about a venue. On success, the sent Message is returned.
data SendVenueRequest = SendVenueRequest
  { req_venue_chat_id              :: Text                -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , req_venue_latitude             :: Double              -- ^ Latitude of location
  , req_venue_longitude            :: Double              -- ^ Longitude of location
  , req_venue_title                :: Text                -- ^ Name of the venue
  , req_venue_address              :: Text                -- ^ Address of the venue
  , req_venue_disable_notification :: Bool                -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , req_venue_foursquare_id        :: Maybe Text          -- ^ Address of the venue
  , req_venue_reply_to_message_id  :: Maybe Int           -- ^ If the message is a reply, ID of the original message
  , req_venue_reply_markup         :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Eq, Show)

-- | This object represents request for 'sendContact'
-- | Use this method to send phone contacts. On success, the sent Message is returned.
data SendContactRequest = SendContactRequest
  { req_contact_chat_id              :: Text                -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , req_contact_phone_number         :: Text                -- ^ Contact's phone number
  , req_contact_first_name           :: Text                -- ^ Contact' first name
  , req_contact_last_name            :: Maybe Text          -- ^ Contact' last name
  , req_contact_disable_notification :: Bool                -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , req_contact_reply_to_message_id  :: Maybe Int           -- ^ If the message is a reply, ID of the original message
  , req_contact_reply_markup         :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Eq, Show)

-- |
data SendGameRequest = SendGameRequest
  { req_game_chat_id              :: Text                -- ^ Unique identifier for the target chat
  , req_game_game_short_name      :: Text                -- ^ Short name of the game, serves as the unique identifier for the game. Set up your games via Botfather.
  , req_game_disable_notification :: Bool                -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , req_game_reply_to_message_id  :: Maybe Int           -- ^ If the message is a reply, ID of the original message
  , req_game_reply_markup         :: Maybe ReplyKeyboard -- ^ A JSON-serialized object for an inline keyboard. If empty, one ‘Play game_title’ button will be shown. If not empty, the first button must launch the game.
  } deriving (Eq,Show)

-- | Use this method to set the score of the specified user in a game.
-- | On success, if the message was sent by the bot, returns the edited Message, otherwise returns True.
-- | Returns an error, if the new score is not greater than the user's current score in the chat and force is False.
data SetGameScoreRequest =
  SetGameScoreRequest
  { req_score_user_id              :: Int  -- ^ User identifier
  , req_score_score                :: Int  -- ^ New score, must be non-negative
  , req_score_chat_id              :: Text -- ^ Unique identifier for the target chat
  , req_score_message_id           :: Int  -- ^ Identifier of the sent message
  , req_score_force                :: Bool -- ^ Pass True, if the high score is allowed to decrease. This can be useful when fixing mistakes or banning cheaters
  , req_score_disable_edit_message :: Bool -- ^ Pass True, if the game message should not be automatically edited to include the current scoreboard
  }
  | SetGameScoreInlineRequest
  { req_score_user_id              :: Int  -- ^ User identifier
  , req_score_score                :: Int  -- ^ New score, must be non-negative
  , req_score_inline_message_id    :: Text -- ^ Identifier of the inline message
  , req_score_force                :: Bool -- ^ Pass True, if the high score is allowed to decrease. This can be useful when fixing mistakes or banning cheaters
  , req_score_disable_edit_message :: Bool -- ^ Pass True, if the game message should not be automatically edited to include the current scoreboard
  } deriving (Eq,Show)

-- | Use this method to get data for high score tables. Will return the score of the specified user and several of his neighbors in a game.
-- | On success, returns an Array of GameHighScore objects.
-- This method will currently return scores for the target user, plus two of his closest neighbors on each side.
-- Will also return the top three users if the user and his neighbors are not among them. Please note that this behavior is subject to change.
data GetGameHighScores =
  GetGameHighScores
  { req_highscore_user_id    :: Int  -- ^ Target user id
  , req_highscore_chat_id    :: Text -- ^ Unique identifier for the target chat
  , req_highscore_message_id :: Int  -- ^ Identifier of the sent message
  }
  | GetGameHighScoresInline
  { req_highscore_user_id           :: Int  -- ^ Target user id
  , req_highscore_inline_message_id :: Text -- ^ Identifier of the inline message
  } deriving (Eq,Show)

-- | This object represents request for 'sendChatAction'
-- | Use this method when you need to tell the user that something is happening on the bot's side.
-- | The status is set for 5 seconds or less (when a message arrives from your bot, Telegram clients clear its typing status).
data SendChatActionRequest = SendChatActionRequest
  { action_chat_id :: Text
  , action_action  :: ChatAction
  } deriving (Eq, Show)

-- | This object represents request for 'answerInlineQuery'
-- | Use this method to send answers to an inline query. On success, True is returned. No more than 50 results per query are allowed.
data AnswerInlineQueryRequest = AnswerInlineQueryRequest
  { query_inline_query_id     :: Text       -- ^ Unique identifier for the answered query
  , query_results             :: [InlineQueryResult] -- ^ A JSON-serialized array of results for the inline query
  , query_is_personal         :: Bool       -- ^ Pass True, if results may be cached on the server side only for the user that sent the query. By default, results may be returned to any user who sends the same query
  , query_cache_time          :: Maybe Int  -- ^ The maximum amount of time in seconds that the result of the inline query may be cached on the server. Defaults to 300.
  , query_next_offset         :: Maybe Text -- ^ Pass the offset that a client should send in the next query with the same text to receive more results. Pass an empty string if there are no more results or if you don‘t support pagination. Offset length can’t exceed 64 bytes.
  , query_switch_pm_text      :: Maybe Text -- ^ If passed, clients will display a button with specified text that switches the user to a private chat with the bot and sends the bot a start message with the parameter switch_pm_parameter
  , query_switch_pm_parameter :: Maybe Text -- ^ Parameter for the start message sent to the bot when user presses the switch button
  } deriving (Eq, Show)
-- Example: An inline bot that sends YouTube videos can ask the user to connect the bot to their YouTube account to adapt search results accordingly.
-- To do this, it displays a ‘Connect your YouTube account’ button above the results, or even before showing any.
-- The user presses the button, switches to a private chat with the bot and, in doing so, passes a start parameter that instructs the bot to return an oauth link.
-- Once done, the bot can offer a switch_inline button so that the user can easily return to the chat where they wanted to use the bot's inline capabilities.

-- | This object represents request for 'getUserProfilePhotos'
-- | Use this method to get a list of profile pictures for a user. Returns a UserProfilePhotos object.
data UserProfilePhotosRequest =
  UserProfilePhotosRequest
  { photos_user_id :: Int       -- ^ Unique identifier of the target user
  , photos_offset  :: Maybe Int -- ^ Sequential number of the first photo to be returned. By default, all photos are returned.
  , photos_limit   :: Maybe Int -- ^ Limits the number of photos to be retrieved. Values between 1—100 are accepted. Defaults to 100.
  } deriving (Eq, Show)

-- | This object represents request for 'getFile'
-- | Use this method to get basic info about a file and prepare it for downloading. For the moment, bots can download files of up to 20MB in size.
--   On success, a File object is returned. The file can then be downloaded via the link https://api.telegram.org/file/bot<token>/<file_path>,
--   where <file_path> is taken from the response. It is guaranteed that the link will be valid for at least 1 hour. When the link expires, a new one can be requested by calling getFile again.
newtype FileRequest = FileRequest { filereq_file_id :: Text } -- ^ File identifier to get info about
-- Note: This function may not preserve original file name. Mime type of the file and its name (if available) should be saved when the File object is received.

-- | This object represents request for 'kickChatMember'
-- | Use this method to kick a user from a group or a supergroup. In the case of supergroups, the user will not be able to return to the group on their own using invite links, etc., unless unbanned first.
--   The bot must be an administrator in the group for this to work. Returns True on success.
data KickChatMemberRequest =
  KickChatMemberRequest
  { kick_chat_id :: Text -- ^ Unique identifier for the target group or username of the target supergroup (in the format @supergroupusername)
  , kick_user_id :: Int  -- ^ Unique identifier of the target user
  } deriving (Eq, Show)
-- Note: This will method only work if the ‘All Members Are Admins’ setting is off in the target group. Otherwise members may only be removed by the group's creator or by the member that added them.

-- | This object represents request for 'leaveChat'
-- | Use this method for your bot to leave a group, supergroup or channel. Returns True on success.
newtype LeaveChatRequest = LeaveChatRequest { leave_chat_id :: Text } -- ^ Unique identifier for the target chat or username of the target supergroup or channel (in the format @channelusername)

-- | This object represents request for 'unbanChatMember'
-- | Use this method to unban a previously kicked user in a supergroup. The user will not return to the group automatically, but will be able to join via link, etc. The bot must be an administrator in the group for this to work. Returns True on success.
data UnbanChatMemberRequest =
  UnbanChatMemberRequest
  { unban_chat_id :: Text -- ^ Unique identifier for the target group or username of the target supergroup (in the format @supergroupusername)
  , unban_user_id :: Int  -- ^ Unique identifier of the target user
  } deriving (Eq, Show)

-- | This object represents request for 'getChat'
-- | Use this method to get up to date information about the chat (current name of the user for one-on-one conversations, current username of a user, group or channel, etc.). Returns a Chat object on success.
newtype GetChatRequest = GetChatRequest { getchat_chat_id :: Text } -- ^ Unique identifier for the target chat or username of the target supergroup or channel (in the format @channelusername)
  deriving (Eq, Show)

-- | This object represents request for 'getChatAdministrators'
-- | Use this method to get a list of administrators in a chat. On success, returns an Array of ChatMember objects that contains information about all chat administrators except other bots.
--   If the chat is a group or a supergroup and no administrators were appointed, only the creator will be returned.
newtype GetChatAdministratorsRequest = GetChatAdministratorsRequest { admin_chat_id :: Text } -- ^ Unique identifier for the target chat or username of the target supergroup or channel (in the format @channelusername)
  deriving (Eq, Show)

-- | This object represents request for 'getChatMembersCount'
-- | Use this method to get the number of members in a chat. Returns Int on success.
newtype GetChatMembersCountRequest = GetChatMembersCountRequest { count_chat_id :: Text } -- ^ Unique identifier for the target chat or username of the target supergroup or channel (in the format @channelusername)
  deriving (Eq, Show)

-- | This object represents request for 'getChatMember'
-- | Use this method to get information about a member of a chat. Returns a ChatMember object on success.
data GetChatMemberRequest =
  GetChatMemberRequest
  { member_chat_id :: Text -- ^ Unique identifier for the target chat or username of the target supergroup or channel (in the format @channelusername)
  , member_user_id :: Int  -- ^ Unique identifier of the target user
  } deriving (Eq, Show)

-- | This object represents request for 'answerCallbackQuery'
-- | Use this method to send answers to callback queries sent from inline keyboards. The answer will be displayed to the user as a notification at the top of the chat screen or as an alert. On success, True is returned.
data AnswerCallbackQueryRequest =
  AnswerCallbackQueryRequest
  { callback_callback_query_id :: Text       -- ^ Unique identifier for the query to be answered
  , callback_text              :: Maybe Text -- ^ Text of the notification. If not specified, nothing will be shown to the user, 0-200 characters
  , callback_show_alert        :: Bool       -- ^ If true, an alert will be shown by the client instead of a notification at the top of the chat screen. Defaults to false.
  , callback_url               :: Maybe Text -- ^ URL that will be opened by the user's client. If you have created a Game and accepted the conditions via @Botfather,
                                             -- specify the URL that opens your game – note that this will only work if the query comes from a callback_game button.
                                             -- Otherwise, you may use links like telegram.me/your_bot?start=XXXX that open your bot with a parameter.
  , callback_cache_time        :: Int        -- ^ The maximum amount of time in seconds that the result of the callback query may be cached client-side. Defaults to 0.
  } deriving (Eq, Show)
