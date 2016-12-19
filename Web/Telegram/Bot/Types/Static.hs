module Web.Telegram.Bot.Types.Static where


import Data.Aeson
import Data.Aeson.Types     (Pair)
import Data.Maybe           (catMaybes)
import Data.Text            (Text)
import Data.HashMap.Strict  as HM

-- | Helper function to avoid `Maybe [a]`s
mEmptyList :: ToJSON a => Text -> [a] -> Maybe Pair
mEmptyList _ [] = Nothing
mEmptyList t l  = Just $ t .= l

-- | Helper function to avoid `Maybe Bool`s
-- (first Bool is what Nothing would default to)
mBool :: Text -> Bool -> Bool -> Maybe Pair
mBool t a b = if a == b then Nothing else Just $ t .= b

object' :: [Maybe Pair] -> Value
object' = Object . HM.fromList . catMaybes

(.=!!) :: ToJSON a => Text -> Maybe a -> Maybe Pair
_    .=!! Nothing  = Nothing
name .=!! (Just v) = Just $ name .= v

(.=!) :: ToJSON a => Text -> a -> Maybe Pair
name .=! value = Just $ name .= value


-- | Type of chat.
data ChatType = Private
              | Group
              | Supergroup
              | Channel
  deriving (Eq, Show)

-- | Parse mode for text message
data ParseMode = Markdown
               | HTML
  deriving (Eq, Show)

-- | MIME type for InlineQueryResultVideo
data VideoMIME = TextHTML
               | VideoMP4
  deriving (Eq, Show)

-- | MIME type for InlineQueryResultDocument
data DocumentMIME = ApplicationPDF
                  | ApplicationZIP
  deriving (Eq, Show)

-- | Type of action to broadcast.
data ChatAction = Typing
                | UploadPhoto
                | RecordVideo
                | UploadVideo
                | RecordAudio
                | UploadAudio
                | UploadDocument
                | FindLocation
  deriving (Eq, Show)

-- | Status of ChatMembers
data ChatMemberStatus = Creator
                      | Administrator
                      | Member
                      | MemberLeft
                      | MemberKicked
  deriving (Eq, Show)

-- | Types of updates for `getUpdates`
data UpdateType = MESSAGE
                | EDITEDMESSAGE
                | CHANNELPOST
                | EDITEDCHANNELPOST
                | INLINEQUERY
                | CHOSENINLINERESULT
                | CALLBACKQUERY
  deriving (Eq,Show)
