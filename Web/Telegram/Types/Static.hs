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