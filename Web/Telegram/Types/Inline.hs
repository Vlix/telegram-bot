module Web.Telegram.Types.Inline where


import           Data.Text                  (Text (..))

import           Web.Telegram.Types.Basic

-- | This object represents an incoming inline query. When the user sends an empty query, your bot could return some default or trending results.
data InlineQuery = InlineQuery
  { query_id        :: Text -- ^ Unique identifier for this query
  , query_from      :: User -- ^ Sender
  , query_query     :: Text -- ^ Text of the query
  , query_offset    :: Text -- ^ Offset of the results to be returned, can be controlled by the bot
  } deriving (Show)

-- | This object represents a result of an inline query that was chosen by the user and sent to their chat partner.
data ChosenInlineResult = ChosenInlineResult
  { chosen_result_id :: Text -- ^ Unique identifier for this query
  , chosen_from      :: User -- ^ Sender
  , chosen_query     :: Text -- ^ Text of the query
  } deriving (Show)
