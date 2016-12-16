module Web.Telegram.Bot where


import           Data.Text              (Text (..))

import           Web.Telegram.Bot.Types


type UpdateID = Int
--type ID       = Text
--type Query    = Text
--type Offset   = Text

type UserId      = Int
type ChatId      = Text
type MessageId   = Int
type InlineMessageId = Text
type Caption     = Text
type Duration    = Int -- in seconds
type Performer   = Text
type Title       = Text
type Latitude    = Double
type Longitude   = Double
type Address     = Text
type PhoneNumber = Text
type FirstName   = Text
type LastName    = Text


handleUpdates :: UpdateHandler a -> Update -> a
handleUpdates uh (MessageUpdate i m)             = handleMessage           uh i m
handleUpdates uh (EditedMessageUpdate i em)      = handleEditedMessage     uh i em
handleUpdates uh (ChannelPostUpdate i cp)        = handleChannelPost       uh i cp
handleUpdates uh (EditedChannelPostUpdate i ecp) = handleEditedChannelPost uh i ecp
handleUpdates uh (CallbackUpdate i c)            = handleCallbackQuery     uh i c
handleUpdates uh (InlineQueryUpdate i iq)        = handleInlineQuery       uh i iq
handleUpdates uh (ChosenInlineUpdate i cir)      = handleChosenInline      uh i cir

data UpdateHandler a =
    UpdateHandler
    { handleMessage           :: UpdateID -> Message            -> a
    , handleEditedMessage     :: UpdateID -> Message            -> a
    , handleChannelPost       :: UpdateID -> Message            -> a
    , handleEditedChannelPost :: UpdateID -> Message            -> a
    , handleCallbackQuery     :: UpdateID -> CallbackQuery      -> a
    , handleInlineQuery       :: UpdateID -> InlineQuery        -> a
    , handleChosenInline      :: UpdateID -> ChosenInlineResult -> a
    }


-- These are some shortcuts for certain requests, if you want to default any optional arguments

sendMessage :: ChatId -> Text -> SendMessageRequest
sendMessage chatid txt =
  SendMessageRequest chatid txt False False Nothing Nothing Nothing

forwardMessage :: ChatId -> ChatId -> MessageId -> ForwardMessageRequest
forwardMessage chatid fromchatid msgid =
  ForwardMessageRequest chatid fromchatid False msgid

sendPhoto :: ChatId -> Text -> Maybe Caption -> SendPhotoRequest
sendPhoto chatid photoid mcaption =
  SendPhotoRequest chatid photoid False mcaption Nothing Nothing

sendAudio :: ChatId -> Text -> Maybe Caption -> Maybe Duration -> Maybe Performer -> Maybe Title -> SendAudioRequest
sendAudio chatid audioid mcaption mduration mperformer mtitle =
  SendAudioRequest chatid audioid False mcaption mduration mperformer mtitle Nothing Nothing

sendSticker :: ChatId -> Text -> SendStickerRequest
sendSticker chatid stickerid =
  SendStickerRequest chatid stickerid False Nothing Nothing

sendDocument :: ChatId -> Text -> Maybe Caption -> SendDocumentRequest
sendDocument chatid documentid mcaption =
  SendDocumentRequest chatid documentid False mcaption Nothing Nothing

sendVideo :: ChatId -> Text -> Maybe Duration -> Maybe Caption -> SendVideoRequest
sendVideo chatid videoid mduration mcaption =
  SendVideoRequest chatid videoid False mduration Nothing Nothing mcaption Nothing Nothing

sendVoice :: ChatId -> Text -> Maybe Duration -> Maybe Caption -> SendVoiceRequest
sendVoice chatid voiceid mduration mcaption =
  SendVoiceRequest chatid voiceid False mcaption mduration Nothing Nothing

sendLocation :: ChatId -> Latitude -> Longitude -> SendLocationRequest
sendLocation chatid lat lon =
  SendLocationRequest chatid lat lon False Nothing Nothing

sendVenue :: ChatId -> Latitude -> Longitude -> Title -> Address -> SendVenueRequest
sendVenue chatid lat lon title address =
  SendVenueRequest chatid lat lon title address False Nothing Nothing Nothing

sendContact :: ChatId -> PhoneNumber -> FirstName -> Maybe LastName -> SendContactRequest
sendContact chatid phonenumber firstname mlastname =
  SendContactRequest chatid phonenumber firstname mlastname False Nothing Nothing

sendGame :: ChatId -> Text -> SendGameRequest
sendGame chatid gameShortName =
  SendGameRequest chatid gameShortName False Nothing Nothing

setGameScore :: UserId -> Int -> ChatId -> MessageId -> SetGameScoreRequest
setGameScore userid score chatid msgid =
  SetGameScoreRequest userid score chatid msgid False False

setGameScoreInline :: UserId -> Int -> InlineMessageId -> SetGameScoreRequest
setGameScoreInline userid score inlinemsgid =
  SetGameScoreInlineRequest userid score inlinemsgid False False