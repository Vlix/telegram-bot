module Web.Telegram where


import           Data.Text              (Text (..))

import           Web.Telegram.Types


type UpdateID = Int
--type ID       = Text
--type Query    = Text
--type Offset   = Text

type ChatId      = Text
type MessageId   = Int
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
handleUpdates uh (MessageUpdate i m)        = handleMessage            uh i m
handleUpdates uh (CallbackUpdate i c)       = handleCallbackQuery      uh i c
handleUpdates uh (InlineQueryUpdate i iq)   = handleInlineQuery        uh i iq
handleUpdates uh (ChosenInlineUpdate i cir) = handleChosenInlineResult uh i cir

data UpdateHandler a =
    UpdateHandler
    { handleMessage            :: UpdateID -> Message            -> a
    , handleCallbackQuery      :: UpdateID -> CallbackQuery      -> a
    , handleInlineQuery        :: UpdateID -> InlineQuery        -> a
    , handleChosenInlineResult :: UpdateID -> ChosenInlineResult -> a
    }


-- These are some shortcuts for certain requests, if you want to default any optional arguments

sendMessage :: ChatId -> Text -> SendMessageRequest
sendMessage chatid txt = SendMessageRequest chatid txt Nothing Nothing Nothing Nothing Nothing

forwardMessage :: ChatId -> ChatId -> MessageId -> ForwardMessageRequest
forwardMessage chatid fromchatid msgid = ForwardMessageRequest chatid fromchatid Nothing msgid

sendPhoto :: ChatId -> Text -> Maybe Caption -> SendPhotoRequest
sendPhoto chatid photoid mcaption = SendPhotoRequest chatid photoid mcaption Nothing Nothing Nothing

sendAudio :: ChatId -> Text -> Maybe Duration -> Maybe Performer -> Maybe Title -> SendAudioRequest
sendAudio chatid audioid mduration mperformer mtitle = SendAudioRequest chatid audioid mduration mperformer mtitle Nothing Nothing Nothing

sendSticker :: ChatId -> Text -> SendStickerRequest
sendSticker chatid stickerid = SendStickerRequest chatid stickerid Nothing Nothing Nothing

sendDocument :: ChatId -> Text -> Maybe Caption -> SendDocumentRequest
sendDocument chatid documentid mcaption = SendDocumentRequest chatid documentid mcaption Nothing Nothing Nothing

sendVideo :: ChatId -> Text -> Maybe Duration -> Maybe Caption -> SendVideoRequest
sendVideo chatid videoid mduration mcaption = SendVideoRequest chatid videoid mduration Nothing Nothing mcaption Nothing Nothing Nothing

sendVoice :: ChatId -> Text -> Maybe Duration -> SendVoiceRequest
sendVoice chatid voiceid mduration = SendVoiceRequest chatid voiceid mduration Nothing Nothing Nothing

sendLocation :: ChatId -> Latitude -> Longitude -> SendLocationRequest
sendLocation chatid lat lon = SendLocationRequest chatid lat lon Nothing Nothing Nothing

sendVenue :: ChatId -> Latitude -> Longitude -> Title -> Address -> SendVenueRequest
sendVenue chatid lat lon title address = SendVenueRequest chatid lat lon title address Nothing Nothing Nothing Nothing

sendContact :: ChatId -> PhoneNumber -> FirstName -> Maybe LastName -> SendContactRequest
sendContact chatid phonenumber firstname mlastname = SendContactRequest chatid phonenumber firstname mlastname Nothing Nothing Nothing
