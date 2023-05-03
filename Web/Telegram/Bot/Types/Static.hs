{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Telegram.Bot.Types.Static where


import Data.Aeson
import Data.Aeson.Types     (Pair)
import Data.Maybe           (catMaybes)
import Data.Aeson.KeyMap    as KM

-- | Helper function to avoid `Maybe [a]`s
mEmptyList :: ToJSON a => Key -> [a] -> Maybe Pair
mEmptyList _ [] = Nothing
mEmptyList t l  = Just $ t .= l

-- | Helper function to avoid `Maybe Bool`s
-- (first Bool is what Nothing would default to)
mBool :: Key -> Bool -> Bool -> Maybe Pair
mBool t a b = if a == b then Nothing else Just $ t .= b

object' :: [Maybe Pair] -> Value
object' = Object . KM.fromList . catMaybes

(.=!!) :: ToJSON a => Key -> Maybe a -> Maybe Pair
name .=!! v = (name .=) <$> v

(.=!) :: ToJSON a => Key -> a -> Maybe Pair
name .=! value = Just $ name .= value


instance {-# OVERLAPPING #-} (FromJSON a) => FromJSON (Either Bool a) where
  parseJSON (Bool b) = pure $ Left b
  parseJSON x = Right <$> parseJSON x


-- | Type of chat.
data ChatType = Private
              | Group
              | Supergroup
              | Channel
  deriving stock (Eq, Show)

-- | Parse mode for text message
data ParseMode = Markdown
               | HTML
  deriving stock (Eq, Show)

-- | MIME type for InlineQueryResultVideo
data VideoMIME = TextHTML
               | VideoMP4
  deriving stock (Eq, Show)

-- | MIME type for InlineQueryResultDocument
data DocumentMIME = ApplicationPDF
                  | ApplicationZIP
  deriving stock (Eq, Show)

-- | Type of action to broadcast.
data ChatAction = Typing
                | UploadPhoto
                | RecordVideo
                | UploadVideo
                | RecordAudio
                | UploadAudio
                | UploadDocument
                | FindLocation
  deriving stock (Eq, Show)

-- | Status of ChatMembers
data ChatMemberStatus = Creator
                      | Administrator
                      | Member
                      | MemberLeft
                      | MemberKicked
  deriving stock (Eq, Show)

-- | Types of updates for `getUpdates`
data UpdateType = MESSAGE
                | EDITEDMESSAGE
                | CHANNELPOST
                | EDITEDCHANNELPOST
                | INLINEQUERY
                | CHOSENINLINERESULT
                | CALLBACKQUERY
  deriving stock (Eq, Show)
