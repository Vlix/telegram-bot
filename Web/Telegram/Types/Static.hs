module Web.Telegram.Types.Static where

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