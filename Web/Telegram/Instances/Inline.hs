module Web.Telegram.Instances.Inline where


import           Control.Applicative        ((<|>))
import           Data.Aeson
import           Data.Text                  (Text (..))
import           Data.Aeson.Types           (typeMismatch)
import qualified Data.HashMap.Strict        as HM

import           Web.Telegram.Types.Basic
import           Web.Telegram.Types.Inline
import           Web.Telegram.Instances.Basic

----------------------
-- ToJSON INSTANCES --
----------------------

instance ToJSON InlineQuery where
    toJSON (InlineQuery ident from query offset) = object [ "id"     .= ident
                                                          , "from"   .= from
                                                          , "query"  .= query
                                                          , "offset" .= offset
                                                          ]

instance ToJSON ChosenInlineResult where
    toJSON (ChosenInlineResult result_id from query) = object [ "result_id" .= result_id
                                                              , "from" .= from
                                                              , "query" .= query
                                                              ]


------------------------
-- FromJSON INSTANCES --
------------------------

instance FromJSON InlineQuery where
    parseJSON (Object o) = InlineQuery <$> o .: "id"
                                       <*> o .: "from"
                                       <*> o .: "query"
                                       <*> o .: "offset" 
    parseJSON wat = typeMismatch "InlineQuery" wat

instance FromJSON ChosenInlineResult where
    parseJSON (Object o) = ChosenInlineResult <$> o .: "result_id"
                                              <*> o .: "from"
                                              <*> o .: "query" 
    parseJSON wat = typeMismatch "ChosenInlineResult" wat

