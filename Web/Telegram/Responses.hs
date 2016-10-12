module Web.Telegram.Responses where


import           Control.Applicative    ((<|>))
import           Data.Text              (Text (..))
import           Data.Aeson
import           Data.Aeson.Types       (typeMismatch)
import qualified Data.HashMap.Strict    as HM
import           Data.Monoid            ((<>))

import           Web.Telegram.Types.Basic
import           Web.Telegram.Instances


data Response =
    OK
    { ok_description :: Maybe Text }
    | UserResponse
    { user_result          :: User
    , response_description :: Maybe Text
    }
    | MessageResponse
    { message_result       :: Message
    , response_description :: Maybe Text
    }
    | UserProfilePhotosResponse
    { user_profile_photos_result :: UserProfilePhotos
    , response_description       :: Maybe Text
    }
    | FileResponse
    { file_result          :: File
    , response_description :: Maybe Text
    }
    | ChatResponse
    { chat_result          :: Chat
    , response_description :: Maybe Text
    }
    | ChatMemberResponse
    { chat_member_result   :: [ChatMember]
    , response_description :: Maybe Text
    }
    | IntResponse
    { int_result           :: Int
    , response_description :: Maybe Text
    }
    | ErrorResponse
    { error_description :: Text
    , error_error_code  :: Maybe Int
    } deriving (Eq, Show)

instance ToJSON Response where
    toJSON (OK desc) = object [ "ok"          .= True
                              , "description" .= desc
                              , "result"      .= True
                              ]
    toJSON (UserResponse user desc) = object [ "ok"          .= True
                                             , "description" .= desc
                                             , "result"      .= user
                                             ]
    toJSON (MessageResponse msg desc) = object [ "ok"          .= True
                                               , "description" .= desc
                                               , "result"      .= msg
                                               ]
    toJSON (UserProfilePhotosResponse upr desc) = object [ "ok"          .= True
                                                         , "description" .= desc
                                                         , "result"      .= upr
                                                         ]
    toJSON (FileResponse file desc) = object [ "ok"          .= True
                                             , "description" .= desc
                                             , "result"      .= file
                                             ]
    toJSON (ChatResponse chat desc) = object [ "ok"          .= True
                                             , "description" .= desc
                                             , "result"      .= chat
                                             ]
    toJSON (ChatMemberResponse cmember desc) = object [ "ok"          .= True
                                                      , "description" .= desc
                                                      , "result"      .= cmember
                                                      ]
    toJSON (IntResponse int desc) = object [ "ok"          .= True
                                           , "description" .= desc
                                           , "result"      .= int
                                           ]
    toJSON (ErrorResponse desc code) = object [ "ok"          .= False
                                              , "description" .= desc
                                              , "error_code"  .= code
                                              ]

instance FromJSON Response where
    parseJSON (Object o) = case "ok" `HM.lookup` o of
        Just (Bool False) -> ErrorResponse <$> o .: "description"
                                           <*> o .:? "error_code"
        Just (Bool True) -> OK <$> o .:? "description"
            <|> UserResponse <$> o .: "result"
                             <*> o .:? "description"
            <|> MessageResponse <$> o .: "result"
                                <*> o .:? "description"
            <|> UserProfilePhotosResponse <$> o .: "result"
                                          <*> o .:? "description"
            <|> FileResponse <$> o .: "result"
                             <*> o .:? "description"
            <|> ChatResponse <$> o .: "result"
                             <*> o .:? "description"
            <|> ChatMemberResponse <$> o .: "result"
                                   <*> o .:? "description"
            <|> IntResponse <$> o .: "result"
                            <*> o .:? "description"
        Just wat -> fail $ "Wrong [ok] argument type: " <> show wat
        Nothing -> fail "No [ok] argument in Response object"
    parseJSON wat = typeMismatch "Response" wat
