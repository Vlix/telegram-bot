-- | This module contains objects which represent data of Telegram Bot API responses
module Web.Telegram.Bot.Types
    ( module Web.Telegram.Bot.Instances
    , module Web.Telegram.Bot.Requests
    , module Web.Telegram.Bot.Responses
    , module Web.Telegram.Bot.Types.Basic
    , module Web.Telegram.Bot.Types.Static
    , module Web.Telegram.Bot.Types.Inline
    -- * Types
    , Update(..)
    ) where

import Control.Applicative ((<|>))
import Data.Aeson

import Web.Telegram.Bot.Instances()
import Web.Telegram.Bot.Requests
import Web.Telegram.Bot.Responses
import Web.Telegram.Bot.Types.Basic
import Web.Telegram.Bot.Types.Static
import Web.Telegram.Bot.Types.Inline


-- | This object represents an incoming update.
data Update =
  MessageUpdate
  { update_id :: Int     -- ^ The update's unique identifier. Update identifiers start from a certain positive number and increase sequentially.
                         -- This ID becomes especially handy if you’re using 'setWebhooks', since it allows you to ignore repeated updates or
                         -- to restore the correct update sequence, should they get out of order.
  , message   :: Message -- ^ New incoming message of any kind — text, photo, sticker, etc.
  }
  | EditedMessageUpdate
  { update_id :: Int     -- ^ Same as MessageUpdate
  , message   :: Message -- ^ New version of a message that is known to the bot and was edited
  }
  | ChannelPostUpdate
  { update_id :: Int     -- ^ Same as MessageUpdate
  , cMessage  :: ChannelMessage -- ^ New incoming channel post of any kind — text, photo, sticker, etc.
  }
  | EditedChannelPostUpdate
  { update_id :: Int     -- ^ Same as MessageUpdate
  , cMessage   :: ChannelMessage -- ^ New version of a channel post that is known to the bot and was edited
  }
  | InlineQueryUpdate
  { update_id    :: Int         -- ^ Same as MessageUpdate
  , inline_query :: InlineQuery -- ^ New incoming inline query
  }
  | ChosenInlineUpdate
  { update_id            :: Int                -- ^ Same as MessageUpdate
  , chosen_inline_result :: ChosenInlineResult -- ^ The result of a inline query that was chosen by a user and sent to their chat partner
  }
  | CallbackUpdate
  { update_id      :: Int           -- ^ Same as MessageUpdate
  , callback_query :: CallbackQuery -- ^ New incoming callback query
  } deriving (Eq, Show)


--------------------
-- JSON INSTANCES --
--------------------


instance ToJSON Update where
  toJSON upd = object [extra, updateId]
   where
    updateId = "update_id" .= update_id upd
    extra = case upd of
      MessageUpdate{}           -> "message"              .= message upd
      EditedMessageUpdate{}     -> "edited_message"       .= message upd
      ChannelPostUpdate{}       -> "channel_post"         .= message upd
      EditedChannelPostUpdate{} -> "edited_channel_post"  .= message upd
      InlineQueryUpdate{}       -> "inline_query"         .= inline_query upd
      ChosenInlineUpdate{}      -> "chosen_inline_result" .= chosen_inline_result upd
      CallbackUpdate{}          -> "callback_query"       .= callback_query upd

instance FromJSON Update where
  parseJSON = withObject "Update" $ \o -> do
    updateId <- o .: "update_id"
    MessageUpdate                 updateId <$> o .: "message"
      <|> EditedMessageUpdate     updateId <$> o .: "edited_message"
      <|> ChannelPostUpdate       updateId <$> o .: "channel_post"
      <|> EditedChannelPostUpdate updateId <$> o .: "edited_channel_post"
      <|> InlineQueryUpdate       updateId <$> o .: "inline_query"
      <|> ChosenInlineUpdate      updateId <$> o .: "chosen_inline_result"
      <|> CallbackUpdate          updateId <$> o .: "callback_query"
