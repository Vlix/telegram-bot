{-# LANGUAGE DerivingStrategies #-}
module Web.Telegram.Bot.Responses where


import           Data.Text              (Text)
import           Data.Aeson
import qualified Data.Aeson.KeyMap      as KM

import           Web.Telegram.Bot.Types.Basic
import           Web.Telegram.Bot.Types.Static
import           Web.Telegram.Bot.Instances()

-- | The `a` can be any of [User,Message,UserProfilePhotos,File,Chat,[ChatMember],Int,Bool]
data Response a =
  OKResponse
  { ok_result      :: a
  , ok_description :: Maybe Text
  }
  | ErrorResponse
  { error_description :: Text
  , error_error_code  :: Maybe Int
  , error_parameters  :: Maybe ResponseParameters
  } deriving stock (Eq, Show)


instance ToJSON a => ToJSON (Response a) where
  toJSON (OKResponse result desc) =
    object' [ "ok"          .=! True
            , "result"      .=! result
            , "description" .=!! desc
            ]
  toJSON (ErrorResponse desc code parameters) =
    object' [ "ok"          .=! False
            , "description" .=! desc
            , "error_code"  .=!! code
            , "parameters"  .=!! parameters
            ]

instance FromJSON a => FromJSON (Response a) where
  parseJSON = withObject "Response" $ \o ->
    case "ok" `KM.lookup` o of
      Just (Bool False) -> ErrorResponse <$> o .: "description"
                                         <*> o .:? "error_code"
                                         <*> o .:? "parameters"
      Just (Bool True) -> OKResponse <$> o .: "result"
                                     <*> o .:? "description"
      Just wat -> fail $ "Wrong [ok] argument type: " <> show wat
      Nothing -> fail "No [ok] argument in Response object"
