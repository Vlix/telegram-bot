-- | This module contains objects which represent data of Telegram Bot API responses
module Web.Telegram.Types
    ( module Web.Telegram.Requests
    , module Web.Telegram.Responses
    , module Web.Telegram.Types.Basic
    , module Web.Telegram.Types.Static
    , module Web.Telegram.Types.Inline
    , module Web.Telegram.Instances.Basic
    , module Web.Telegram.Instances.Inline
    , module Web.Telegram.Instances.Static
    -- * Types
    , Update                (..)
    ) where

import           Control.Applicative        ((<|>))
import           Data.Aeson
import           Data.Aeson.Types           (typeMismatch)

import           Web.Telegram.Requests
import           Web.Telegram.Responses
import           Web.Telegram.Types.Basic
import           Web.Telegram.Types.Static
import           Web.Telegram.Types.Inline
import           Web.Telegram.Instances.Basic
import           Web.Telegram.Instances.Inline
import           Web.Telegram.Instances.Static


-- | This object represents an incoming update.
data Update =
  MessageUpdate
  { update_id :: Int     -- ^ The update's unique identifier. Update identifiers start from a certain positive number and increase sequentially. This ID becomes especially handy if you’re using 'setWebhooks', since it allows you to ignore repeated updates or to restore the correct update sequence, should they get out of order.
  , message   :: Message -- ^ New incoming message of any kind — text, photo, sticker, etc.
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