module Web.Telegram.Types.Static where

-- | Type of chat.
data ChatType = Private
              | Group
              | Supergroup
              | Channel
    deriving (Show)

-- | Parse mode for text message
data ParseMode = Markdown
               | HTML
    deriving (Show)

-- | MIME type for InlineQueryResultVideo
data VideoMIME = TextHTML
               | VideoMP4
    deriving (Show)

-- | MIME type for InlineQueryResultDocument
data DocumentMIME = ApplicationPDF
                  | ApplicationZIP
    deriving (Show)

-- | Type of action to broadcast.
data ChatAction = Typing
                | UploadPhoto
                | RecordVideo
                | UploadVideo
                | RecordAudio
                | UploadAudio
                | UploadDocument
                | FindLocation
    deriving (Show)
