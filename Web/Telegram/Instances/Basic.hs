module Web.Telegram.Instances.Basic where


import           Control.Applicative        ((<|>))
import           Data.Aeson
import           Data.Aeson.Types           (typeMismatch)
import           Data.Text                  (unpack)
import           Data.Maybe                 (isJust,fromMaybe)
import           Data.Monoid                ((<>))
import qualified Data.HashMap.Strict        as HM

import           Web.Telegram.Types.Basic
import           Web.Telegram.Types.Static
import           Web.Telegram.Instances.Static


----------------------
-- ToJSON INSTANCES --
----------------------

instance ToJSON User where
  toJSON (User ident first_name last_name username) =
    object' [ "id"         .=! ident
            , "first_name" .=! first_name
            , "last_name"  .=!! last_name
            , "username"   .=!! username
            ]

instance ToJSON Chat where
  toJSON (PrivateChat ident first_name last_name username) =
    object' [ "id"         .=! ident
            , "type"       .=! toJSON Private
            , "first_name" .=! first_name
            , "last_name"  .=!! last_name
            , "username"   .=!! username
            ]
  toJSON (GroupChat ident title allAdmin) =
    object' [ "id"    .=! ident
            , "type"  .=! toJSON Group
            , "title" .=! title
            , mBool "all_members_are_administrators" False allAdmin
            ]
  toJSON (SuperGroupChat ident title allAdmin username) =
    object' [ "id"       .=! ident
            , "type"     .=! toJSON Supergroup
            , "title"    .=! title
            , mBool "all_members_are_administrators" False allAdmin
            , "username" .=!! username
            ]
  toJSON (ChannelChat ident title username) =
    object' [ "id"       .=! ident
            , "type"     .=! toJSON Channel
            , "title"    .=! title
            , "username" .=!! username
            ]

instance ToJSON Message where
  toJSON (ForwardedMessage message forward_from forward_from_chat forward_from_message_id forward_date)
        | Object msg <- toJSON message = Object $ HM.fromList [ ("forward_from", toJSON forward_from)
                                                              , ("forward_from_chat",toJSON forward_from_chat)
                                                              , ("forward_date",toJSON forward_date)
                                                              , ("forward_from_message_id",toJSON forward_from_message_id)
                                                              ] `HM.union` msg
        | otherwise = error "message parameter isn't an Object in ToJSON Message - ForwardedMessage"
  toJSON (TextMessage message_id from date chat text edit_date reply entities) =
    object' [ "message_id"       .=! message_id
            , "from"             .=! from
            , "date"             .=! date
            , "chat"             .=! chat
            , "text"             .=! text
            , "edit_date"        .=!! edit_date
            , "reply_to_message" .=!! reply
            , mEmptyList "entities" entities
            ]
  toJSON (AudioMessage message_id from date chat audio caption edit_date reply) =
    object' [ "message_id"       .=! message_id
            , "from"             .=! from
            , "date"             .=! date
            , "chat"             .=! chat
            , "audio"            .=! audio
            , "caption"          .=!! caption
            , "edit_date"        .=!! edit_date
            , "reply_to_message" .=!! reply
            ]
  toJSON (DocumentMessage message_id from date chat document caption edit_date reply) =
    object' [ "message_id"       .=! message_id
            , "from"             .=! from
            , "date"             .=! date
            , "chat"             .=! chat
            , "document"         .=! document
            , "caption"          .=!! caption
            , "edit_date"        .=!! edit_date
            , "reply_to_message" .=!! reply
            ]
  toJSON (GameMessage message_id from date chat game edit_date reply) =
    object' [ "message_id"       .=! message_id
            , "from"             .=! from
            , "date"             .=! date
            , "chat"             .=! chat
            , "game"             .=! game
            , "edit_date"        .=!! edit_date
            , "reply_to_message" .=!! reply
            ]
  toJSON (PhotoMessage message_id from date chat photo caption edit_date reply) =
    object' [ "message_id"       .=! message_id
            , "from"             .=! from
            , "date"             .=! date
            , "chat"             .=! chat
            , "photo"            .=! photo
            , "caption"          .=!! caption
            , "edit_date"        .=!! edit_date
            , "reply_to_message" .=!! reply
            ]
  toJSON (StickerMessage message_id from date chat sticker reply) =
    object' [ "message_id"       .=! message_id
            , "from"             .=! from
            , "date"             .=! date
            , "chat"             .=! chat
            , "sticker"          .=! sticker
            , "reply_to_message" .=!! reply
            ]
  toJSON (VideoMessage message_id from date chat video caption edit_date reply) =
    object' [ "message_id"       .=! message_id
            , "from"             .=! from
            , "date"             .=! date
            , "chat"             .=! chat
            , "video"            .=! video
            , "caption"          .=!! caption
            , "edit_date"        .=!! edit_date
            , "reply_to_message" .=!! reply
            ]
  toJSON (VoiceMessage message_id from date chat voice caption edit_date reply) =
    object' [ "message_id"       .=! message_id
            , "from"             .=! from
            , "date"             .=! date
            , "chat"             .=! chat
            , "voice"            .=! voice
            , "caption"          .=!! caption
            , "edit_date"        .=!! edit_date
            , "reply_to_message" .=!! reply
            ]
  toJSON (ContactMessage message_id from date chat contact reply) =
    object' [ "message_id"       .=! message_id
            , "from"             .=! from
            , "date"             .=! date
            , "chat"             .=! chat
            , "contact"          .=! contact
            , "reply_to_message" .=!! reply
            ]
  toJSON (LocationMessage message_id from date chat location reply) =
    object' [ "message_id"       .=! message_id
            , "from"             .=! from
            , "date"             .=! date
            , "chat"             .=! chat
            , "location"         .=! location
            , "reply_to_message" .=!! reply
            ]
  toJSON (VenueMessage message_id from date chat venue reply) =
    object' [ "message_id"       .=! message_id
            , "from"             .=! from
            , "date"             .=! date
            , "chat"             .=! chat
            , "venue"            .=! venue
            , "reply_to_message" .=!! reply
            ]
  toJSON (NewChatParticipantMessage message_id from date chat new_chat_member) =
    object [ "message_id"      .= message_id
           , "from"            .= from
           , "date"            .= date
           , "chat"            .= chat
           , "new_chat_member" .= new_chat_member
           ]
  toJSON (LeftChatParticipantMessage message_id from date chat left_chat_member) =
    object [ "message_id"            .= message_id
           , "from"                  .= from
           , "date"                  .= date
           , "chat"                  .= chat
           , "left_chat_participant" .= left_chat_member
           ]
  toJSON (NewChatTitleMessage message_id from date chat new_chat_title) =
    object [ "message_id"     .= message_id
           , "from"           .= from
           , "date"           .= date
           , "chat"           .= chat
           , "new_chat_title" .= new_chat_title
           ]
  toJSON (NewChatPhotoMessage message_id from date chat new_chat_photo) =
    object [ "message_id"     .= message_id
           , "from"           .= from
           , "date"           .= date
           , "chat"           .= chat
           , "new_chat_photo" .= new_chat_photo
           ]
  toJSON (DeleteChatPhotoMessage message_id from date chat) =
    object [ "message_id"        .= message_id
           , "from"              .= from
           , "date"              .= date
           , "chat"              .= chat
           , "delete_chat_photo" .= Bool True
           ]
  toJSON (GroupChatCreatedMessage message_id from date chat) =
    object [ "message_id"         .= message_id
           , "from"               .= from
           , "date"               .= date
           , "chat"               .= chat
           , "group_chat_created" .= Bool True
           ]
  toJSON (SuperGroupChatCreatedMessage message_id from date chat) =
    object [ "message_id"              .= message_id
           , "from"                    .= from
           , "date"                    .= date
           , "chat"                    .= chat
           , "supergroup_chat_created" .= Bool True
           ]
{-
  toJSON (ChannelChatCreatedMessage message_id from date chat) =
    object [ "message_id"           .= message_id
           , "from"                 .= from
           , "date"                 .= date
           , "chat"                 .= chat
           , "channel_chat_created" .= Bool True
           ]
-}
  toJSON (MigratedToChatMessage message_id from date chat migrate_to_chat_id) =
    object [ "message_id"         .= message_id
           , "from"               .= from
           , "date"               .= date
           , "chat"               .= chat
           , "migrate_to_chat_id" .= migrate_to_chat_id
           ]
  toJSON (MigrateFromChatMessage message_id from date chat migrate_from_chat_id) =
    object [ "message_id"           .= message_id
           , "from"                 .= from
           , "date"                 .= date
           , "chat"                 .= chat
           , "migrate_from_chat_id" .= migrate_from_chat_id
           ]
  toJSON (PinnedMessage message_id from date chat pinned_message) =
    object [ "message_id"     .= message_id
           , "from"           .= from
           , "date"           .= date
           , "chat"           .= chat
           , "pinned_message" .= pinned_message
           ]

instance ToJSON MessageEntity where
  toJSON (MentionEntity offset length) =
    object [ "type"   .= String "mention"
           , "offset" .= offset
           , "length" .= length
           ]
  toJSON (HashtagEntity offset length) =
    object [ "type"   .= String "hashtag"
           , "offset" .= offset
           , "length" .= length
           ]
  toJSON (BotCommandEntity offset length) =
    object [ "type"   .= String "bot_command"
           , "offset" .= offset
           , "length" .= length
           ]
  toJSON (UrlEntity offset length) =
    object [ "type"   .= String "url"
           , "offset" .= offset
           , "length" .= length
           ]
  toJSON (EmailEntity offset length) =
    object [ "type"   .= String "email"
           , "offset" .= offset
           , "length" .= length
           ]
  toJSON (BoldEntity offset length) =
    object [ "type"   .= String "bold"
           , "offset" .= offset
           , "length" .= length
           ]
  toJSON (ItalicEntity offset length) =
    object [ "type"   .= String "italic"
           , "offset" .= offset
           , "length" .= length
           ]
  toJSON (CodeEntity offset length) =
    object [ "type"   .= String "code"
           , "offset" .= offset
           , "length" .= length
           ]
  toJSON (PreEntity offset length) =
    object [ "type"   .= String "pre"
           , "offset" .= offset
           , "length" .= length
           ]
  toJSON (TextLinkEntity offset length url) =
    object [ "type"   .= String "text_link"
           , "offset" .= offset
           , "length" .= length
           , "url"    .= url
           ]
  toJSON (TextMentionEntity offset length user) =
    object [ "type"   .= String "text_mention"
           , "offset" .= offset
           , "length" .= length
           , "user"   .= user
           ]

instance ToJSON PhotoSize where
  toJSON (PhotoSize file_id width height file_size) =
    object' [ "file_id"   .=! file_id
            , "width"     .=! width
            , "height"    .=! height
            , "file_size" .=!! file_size
            ]

instance ToJSON Audio where
  toJSON (Audio file_id duration performer title mime_type file_size) =
    object' [ "file_id"   .=! file_id
            , "duration"  .=! duration
            , "performer" .=!! performer
            , "title"     .=!! title
            , "mime_type" .=!! mime_type
            , "file_size" .=!! file_size
            ]

instance ToJSON Document where
  toJSON (Document file_id thumb file_name mime_type file_size) =
    object' [ "file_id"   .=! file_id
            , "thumb"     .=!! thumb
            , "file_name" .=!! file_name
            , "mime_type" .=!! mime_type
            , "file_size" .=!! file_size
            ]

instance ToJSON Game where
  toJSON (Game title description photo text text_entities animation) =
    object' [ "title"         .=! title
            , "description"   .=! description
            , "photo"         .=! photo
            , "text"          .=!! text
            , mEmptyList "text_entities" text_entities
            , "animation"     .=!! animation
            ]

instance ToJSON Animation where
  toJSON (Animation file_id thumb file_name mime_type file_size) =
    object' [ "file_id"   .=! file_id
            , "thumb"     .=!! thumb
            , "file_name" .=!! file_name
            , "mime_type" .=!! mime_type
            , "file_size" .=!! file_size
            ]

instance ToJSON Sticker where
  toJSON (Sticker file_id width height thumb emoji file_size) =
    object' [ "file_id"   .=! file_id
            , "width"     .=! width
            , "height"    .=! height
            , "thumb"     .=!! thumb
            , "emoji"     .=!! emoji
            , "file_size" .=!! file_size
            ]

instance ToJSON Video where
  toJSON (Video file_id width height duration thumb mime_type file_size) =
    object' [ "file_id"   .=! file_id
            , "width"     .=! width
            , "height"    .=! height
            , "duration"  .=! duration
            , "thumb"     .=!! thumb
            , "mime_type" .=!! mime_type
            , "file_size" .=!! file_size
            ]

instance ToJSON Voice where
  toJSON (Voice file_id duration mime_type file_size) =
    object' [ "file_id"   .=! file_id
            , "duration"  .=! duration
            , "mime_type" .=!! mime_type
            , "file_size" .=!! file_size
            ]

instance ToJSON Contact where
  toJSON (Contact phone_number first_name last_name user_id) =
    object' [ "phone_number" .=! phone_number
            , "first_name"   .=! first_name
            , "last_name"    .=!! last_name
            , "user_id"      .=!! user_id
            ]

instance ToJSON Location where
  toJSON (Location longitude latitude) =
    object [ "longitude" .= longitude
           , "latitude"  .= latitude
           ]

instance ToJSON Venue where
  toJSON (Venue location title address foursquare) =
    object' [ "location"      .=! location
            , "title"         .=! title
            , "address"       .=! address
            , "foursquare_id" .=!! foursquare
            ]

instance ToJSON UserProfilePhotos where
  toJSON (UserProfilePhotos total_count photos) =
    object [ "total_count" .= total_count
           , "photos"      .= photos
           ]

instance ToJSON File where
  toJSON (File ident size path) =
    object' [ "file_id"   .=! ident
            , "file_size" .=!! size
            , "file_path" .=!! path
            ]

instance ToJSON ReplyKeyboard where
  toJSON (ReplyKeyboardMarkup buttons resize onetime sel) =
    object' [ "keyboard"          .=! buttons
            , mBool "resize_keyboard"   False resize
            , mBool "one_time_keyboard" False onetime
            , mBool "selective"         False sel
            ]
  toJSON (InlineKeyboardMarkup buttons) =
    object [ "inline_keyboard" .= buttons ]
  toJSON (ReplyKeyboardRemove sel) =
    object' [ mBool "selective" False sel
            , "remove_keyboard" .=! Bool True
            ]
  toJSON (ForceReply sel) =
    object' [ mBool "selective" False sel
            , "force_reply" .=! Bool True
            ]

instance ToJSON KeyboardButton where
  toJSON (TextButton txt) =
    object [ "text" .= txt ]
  toJSON (ContactButton txt) =
    object [ "text"            .= txt
           , "request_contact" .= Bool True
           ]
  toJSON (LocationButton txt) =
    object [ "text" .= txt
           , "request_location" .= Bool True
           ]

instance ToJSON InlineKeyboardButton where
  toJSON (InlineUrlButton txt url) =
    object [ "text" .= txt
           , "url" .= url
           ]
  toJSON (InlineCallbackButton txt cb) =
    object [ "text"          .= txt
           , "callback_data" .= cb
           ]
  toJSON (InlineSwitchButton txt switch) =
    object [ "text"                .= txt
           , "switch_inline_query" .= fromMaybe "" switch
           ]
  toJSON (InlineSwitchCurrentButton txt switch) =
    object [ "text"                             .= txt
           , "switch_inline_query_current_chat" .= fromMaybe "" switch
           ]
  toJSON (InlineGameButton txt game) =
    object [ "text"          .= txt
           , "callback_game" .= Object HM.empty
           ]

instance ToJSON CallbackGame where
  toJSON _ = Object HM.empty

instance ToJSON GameHighScore where
  toJSON (GameHighScore position user score) = 
    object [ "position" .= position
           , "user"     .= user
           , "score"    .= score
           ]

instance ToJSON CallbackQuery where
  toJSON (CallbackMessage ident from msg chat_instance dataa) =
    object [ "id"            .= ident
           , "from"          .= from
           , "message"       .= msg
           , "chat_instance" .= chat_instance
           , "data"          .= dataa
           ]
  toJSON (CallbackInline ident from inline chat_instance dataa) =
    object [ "id"                .= ident
           , "from"              .= from
           , "inline_message_id" .= inline
           , "chat_instance"     .= chat_instance
           , "data"              .= dataa
           ]
  toJSON (CallbackGameMessage ident from msg chat_instance game_short_name) =
    object [ "id"              .= ident
           , "from"            .= from
           , "message"         .= msg
           , "chat_instance"   .= chat_instance
           , "game_short_name" .= game_short_name
           ]
  toJSON (CallbackGameInline ident from inline chat_instance game_short_name) =
    object [ "id"                .= ident
           , "from"              .= from
           , "inline_message_id" .= inline
           , "chat_instance"     .= chat_instance
           , "game_short_name"   .= game_short_name
           ]

instance ToJSON ChatMember where
  toJSON (ChatMember user status) =
    object [ "user"   .= user
           , "status" .= status
           ]

instance ToJSON ResponseParameters where
  toJSON (ResponseParameters migrate retry) =
    object' [ "migrate_to_chat_id" .=!! migrate
            , "retry_after"        .=!! retry
            ]

------------------------
-- FromJSON INSTANCES --
------------------------

instance FromJSON User where
    parseJSON (Object o) = User <$> o .: "id"
                                <*> o .: "first_name"
                                <*> o .:? "last_name"
                                <*> o .:? "username" 
    parseJSON wat = typeMismatch "User" wat

instance FromJSON Chat where
    parseJSON (Object o) = case HM.lookup "type" o of
        Just (String "private")    -> PrivateChat <$> o .: "id"
                                                  <*> o .: "first_name"
                                                  <*> o .:? "last_name"
                                                  <*> o .:? "username"
        Just (String "group")      -> GroupChat <$> o .: "id"
                                                <*> o .: "title"
                                                <*> o .:? "all_members_are_administrators" .!= False
        Just (String "supergroup") -> SuperGroupChat <$> o .: "id"
                                                     <*> o .: "title"
                                                     <*> o .:? "all_members_are_administrators" .!= False
                                                     <*> o .:? "username"
        Just (String "channel")    -> ChannelChat <$> o .: "id"
                                                  <*> o .: "title"
                                                  <*> o .:? "username"
        _ -> fail "wrong type of Chat type in Chat object"
    parseJSON wat = typeMismatch "Chat" wat

instance FromJSON Message where
  parseJSON (Object o)
    | isJust $ HM.lookup "forward_date" o =
        ForwardedMessage <$> parseJSON (Object $ HM.delete "forward_date" o)
                         <*> o .:? "forward_from"
                         <*> o .:? "forward_from_chat"
                         <*> o .:? "forward_from_message_id"
                         <*> o .: "forward_date"
    | isJust $ HM.lookup "delete_chat_photo" o =
        DeleteChatPhotoMessage <$> o .: "message_id"
                               <*> o .: "from"
                               <*> o .: "date"
                               <*> o .: "chat"
    | isJust $ HM.lookup "group_chat_created" o =
        GroupChatCreatedMessage <$> o .: "message_id"
                                <*> o .: "from"
                                <*> o .: "date"
                                <*> o .: "chat"
    | isJust $ HM.lookup "supergroup_chat_created" o =
        SuperGroupChatCreatedMessage <$> o .: "message_id"
                                     <*> o .: "from"
                                     <*> o .: "date"
                                     <*> o .: "chat"
    {-| isJust $ HM.lookup "channel_chat_created" o = ChannelChatCreatedMessage <$> o .: "message_id"
                          <*> o .: "from"
                          <*> o .: "date"
                          <*> o .: "chat"-}
    | otherwise = TextMessage <$> o .: "message_id"
                              <*> o .: "from"
                              <*> o .: "date"
                              <*> o .: "chat"
                              <*> o .: "text"
                              <*> o .:? "edit_date"
                              <*> o .:? "reply_to_message"
                              <*> o .:? "entities" .!= []
              <|> AudioMessage <$> o .: "message_id"
                               <*> o .: "from"
                               <*> o .: "date"
                               <*> o .: "chat"
                               <*> o .: "audio"
                               <*> o .:? "caption"
                               <*> o .:? "edit_date"
                               <*> o .:? "reply_to_message"
              <|> DocumentMessage <$> o .: "message_id"
                                  <*> o .: "from"
                                  <*> o .: "date"
                                  <*> o .: "chat"
                                  <*> o .: "document"
                                  <*> o .:? "caption"
                                  <*> o .:? "edit_date"
                                  <*> o .:? "reply_to_message"
              <|> GameMessage <$> o .: "message_id"
                              <*> o .: "from"
                              <*> o .: "date"
                              <*> o .: "chat"
                              <*> o .: "game"
                              <*> o .:? "edit_date"
                              <*> o .:? "reply_to_message"
              <|> PhotoMessage <$> o .: "message_id"
                               <*> o .: "from"
                               <*> o .: "date"
                               <*> o .: "chat"
                               <*> o .: "photo"
                               <*> o .:? "caption"
                               <*> o .:? "edit_date"
                               <*> o .:? "reply_to_message"
              <|> StickerMessage <$> o .: "message_id"
                                 <*> o .: "from"
                                 <*> o .: "date"
                                 <*> o .: "chat"
                                 <*> o .: "sticker"
                                 <*> o .:? "reply_to_message"
              <|> VideoMessage <$> o .: "message_id"
                               <*> o .: "from"
                               <*> o .: "date"
                               <*> o .: "chat"
                               <*> o .: "video"
                               <*> o .:? "caption"
                               <*> o .:? "edit_date"
                               <*> o .:? "reply_to_message"
              <|> VoiceMessage <$> o .: "message_id"
                               <*> o .: "from"
                               <*> o .: "date"
                               <*> o .: "chat"
                               <*> o .: "voice"
                               <*> o .:? "caption"
                               <*> o .:? "edit_date"
                               <*> o .:? "reply_to_message"
              <|> ContactMessage <$> o .: "message_id"
                                 <*> o .: "from"
                                 <*> o .: "date"
                                 <*> o .: "chat"
                                 <*> o .: "contact"
                                 <*> o .:? "reply_to_message"
              <|> LocationMessage <$> o .: "message_id"
                                  <*> o .: "from"
                                  <*> o .: "date"
                                  <*> o .: "chat"
                                  <*> o .: "location"
                                  <*> o .:? "reply_to_message"
              <|> VenueMessage <$> o .: "message_id"
                               <*> o .: "from"
                               <*> o .: "date"
                               <*> o .: "chat"
                               <*> o .: "venue"
                               <*> o .:? "reply_to_message"
              <|> NewChatParticipantMessage <$> o .: "message_id"
                                            <*> o .: "from"
                                            <*> o .: "date"
                                            <*> o .: "chat"
                                            <*> o .: "new_chat_participant"
              <|> LeftChatParticipantMessage <$> o .: "message_id"
                                             <*> o .: "from"
                                             <*> o .: "date"
                                             <*> o .: "chat"
                                             <*> o .: "left_chat_participant"
              <|> NewChatTitleMessage <$> o .: "message_id"
                                      <*> o .: "from"
                                      <*> o .: "date"
                                      <*> o .: "chat"
                                      <*> o .: "new_chat_title"
              <|> NewChatPhotoMessage <$> o .: "message_id"
                                      <*> o .: "from"
                                      <*> o .: "date"
                                      <*> o .: "chat"
                                      <*> o .: "new_chat_photo"
              <|> MigratedToChatMessage <$> o .: "message_id"
                                        <*> o .: "from"
                                        <*> o .: "date"
                                        <*> o .: "chat"
                                        <*> o .: "migrate_to_chat_id"
              <|> MigrateFromChatMessage <$> o .: "message_id"
                                         <*> o .: "from"
                                         <*> o .: "date"
                                         <*> o .: "chat"
                                         <*> o .: "migrate_from_chat_id"
              <|> PinnedMessage <$> o .: "message_id"
                                <*> o .: "from"
                                <*> o .: "date"
                                <*> o .: "chat"
                                <*> o .: "pinned_message"
  parseJSON wat = typeMismatch "Message" wat

instance FromJSON MessageEntity where
  parseJSON (Object o) = case "type" `HM.lookup` o of
    Just (String "mention") ->
      MentionEntity <$> o .: "offset"
                    <*> o .: "length"
    Just (String "hashtag") ->
      HashtagEntity <$> o .: "offset"
                    <*> o .: "length"
    Just (String "bot_command") ->
      BotCommandEntity <$> o .: "offset"
                       <*> o .: "length"
    Just (String "url") ->
      UrlEntity <$> o .: "offset"
                <*> o .: "length"
    Just (String "email") ->
      EmailEntity <$> o .: "offset"
                  <*> o .: "length"
    Just (String "bold") ->
      BoldEntity <$> o .: "offset"
                 <*> o .: "length"
    Just (String "italic") ->
      ItalicEntity <$> o .: "offset"
                   <*> o .: "length"
    Just (String "code") ->
      CodeEntity <$> o .: "offset"
                 <*> o .: "length"
    Just (String "pre") ->
      PreEntity <$> o .: "offset"
                <*> o .: "length"
    Just (String "text_link") ->
      TextLinkEntity <$> o .: "offset"
                     <*> o .: "length"
                     <*> o .: "url"
    Just (String "text_mention") ->
      TextMentionEntity <$> o .: "offset"
                        <*> o .: "length"
                        <*> o .: "user"
    Just (String wat) -> fail $ "Wrong String \"" <> unpack wat <> "\" in MessageEntity's [type] parameter"
    Just _          -> fail "Wrong type in MessageEntity's [type] parameter"
    Nothing         -> fail "No [type] parameter in MessageEntity"
  parseJSON wat = typeMismatch "MessageEntity" wat

instance FromJSON PhotoSize where
  parseJSON (Object o) =
    PhotoSize <$> o .: "file_id"
              <*> o .: "width"
              <*> o .: "height"
              <*> o .:? "file_size" 
  parseJSON wat = typeMismatch "PhotoSize" wat

instance FromJSON Audio where
  parseJSON (Object o) =
    Audio <$> o .: "file_id"
          <*> o .: "duration"
          <*> o .:? "performer"
          <*> o .:? "title"
          <*> o .:? "mime_type"
          <*> o .:? "file_size"
  parseJSON wat = typeMismatch "Audio" wat

instance FromJSON Document where
  parseJSON (Object o) =
    Document <$> o .: "file_id"
             <*> o .:? "thumb"
             <*> o .:? "file_name"
             <*> o .:? "mime_type"
             <*> o .:? "file_size"
  parseJSON wat = typeMismatch "Document" wat

instance FromJSON Game where
  parseJSON (Object o) =
    Game <$> o .: "title"
         <*> o .: "description"
         <*> o .: "photo"
         <*> o .:? "text"
         <*> o .:? "text_entities" .!= []
         <*> o .:? "animation"
  parseJSON wat = typeMismatch "Game" wat

instance FromJSON Animation where
  parseJSON (Object o) =
    Animation <$> o .: "file_id"
              <*> o .:? "thumb"
              <*> o .:? "file_name"
              <*> o .:? "mime_type"
              <*> o .:? "file_size"
  parseJSON wat = typeMismatch "Animation" wat

instance FromJSON Sticker where
  parseJSON (Object o) =
    Sticker <$> o .: "file_id"
            <*> o .: "width"
            <*> o .: "height"
            <*> o .:? "thumb"
            <*> o .:? "emoji"
            <*> o .:? "file_size"
  parseJSON wat = typeMismatch "Sticker" wat

instance FromJSON Video where
  parseJSON (Object o) =
    Video <$> o .: "file_id"
          <*> o .: "width"
          <*> o .: "height"
          <*> o .: "duration"
          <*> o .:? "thumb"
          <*> o .:? "mime_type"
          <*> o .:? "file_size"
  parseJSON wat = typeMismatch "Video" wat

instance FromJSON Voice where
  parseJSON (Object o) =
    Voice <$> o .: "file_id"
          <*> o .: "duration"
          <*> o .:? "mime_type"
          <*> o .:? "file_size"
  parseJSON wat = typeMismatch "Voice" wat

instance FromJSON Contact where
  parseJSON (Object o) =
    Contact <$> o .: "phone_number"
            <*> o .: "first_name"
            <*> o .:? "last_name"
            <*> o .:? "user_id"
  parseJSON wat = typeMismatch "Contact" wat

instance FromJSON Location where
  parseJSON (Object o) =
    Location <$> o .: "longitude"
             <*> o .: "latitude"
  parseJSON wat = typeMismatch "Location" wat

instance FromJSON Venue where
  parseJSON (Object o) =
    Venue <$> o .: "location"
          <*> o .: "title"
          <*> o .: "address"
          <*> o .:? "foursquare_id"
  parseJSON wat = typeMismatch "Venue" wat

instance FromJSON UserProfilePhotos where
  parseJSON (Object o) =
    UserProfilePhotos <$> o .: "total_count"
                      <*> o .: "photos"
  parseJSON wat = typeMismatch "UserProfilePhotos" wat

instance FromJSON File where
  parseJSON (Object o) =
    File <$> o .: "file_id"
         <*> o .:? "file_size"
         <*> o .:? "file_path"
  parseJSON wat = typeMismatch "File" wat

instance FromJSON ReplyKeyboard where
  parseJSON (Object o)
    | "remove_keyboard" `HM.lookup` o == Just (Bool True) = ReplyKeyboardRemove <$> o .:? "selective" .!= False
    | "force_reply"     `HM.lookup` o == Just (Bool True) = ForceReply <$> o .:? "selective" .!= False
    | otherwise =
        InlineKeyboardMarkup <$> o .: "inline_keyboard"
    <|> ReplyKeyboardMarkup <$> o .: "keyboard"
                            <*> o .:? "resize_keyboard"   .!= False
                            <*> o .:? "one_time_keyboard" .!= False
                            <*> o .:? "selective"         .!= False
  parseJSON wat = typeMismatch "ReplyKeyboard" wat

instance FromJSON KeyboardButton where
  parseJSON (Object o)
    | HM.lookup "request_contact" o  == Just (Bool True) = ContactButton <$> o .: "text"
    | HM.lookup "request_location" o == Just (Bool True) = LocationButton <$> o .: "text"
    | otherwise = TextButton <$> o .: "text"
  parseJSON wat = typeMismatch "KeyboardButton" wat

instance FromJSON InlineKeyboardButton where
  parseJSON (Object o) =
    InlineUrlButton <$> o .: "text"
                    <*> o .: "url"
    <|> InlineCallbackButton <$> o .: "text"
                             <*> o .: "callback_data"
    <|> InlineSwitchButton <$> o .: "text"
                           <*> o .:? "switch_inline_query"
    <|> InlineSwitchCurrentButton <$> o .: "text"
                                  <*> o .:? "switch_inline_query_current_chat"
    <|> InlineGameButton <$> o .: "text"
                         <*> o .: "callback_game"
  parseJSON wat = typeMismatch "InlineKeyboardButton" wat

instance FromJSON CallbackGame where
  parseJSON _ = pure CallbackGame

instance FromJSON GameHighScore where
  parseJSON (Object o) = GameHighScore <$> o .: "position"
                                       <*> o .: "user"
                                       <*> o .: "score"
  parseJSON wat = typeMismatch "GameHighScore" wat

instance FromJSON CallbackQuery where
  parseJSON (Object o) =
    CallbackMessage <$> o .: "id"
                    <*> o .: "from"
                    <*> o .: "message"
                    <*> o .: "chat_instance"
                    <*> o .: "data"
    <|> CallbackInline <$> o .: "id"
                       <*> o .: "from"
                       <*> o .: "inline_message_id"
                       <*> o .: "chat_instance"
                       <*> o .: "data"
    <|> CallbackGameMessage <$> o .: "id"
                            <*> o .: "from"
                            <*> o .: "message"
                            <*> o .: "chat_instance"
                            <*> o .: "game_short_name"
    <|> CallbackGameInline <$> o .: "id"
                           <*> o .: "from"
                           <*> o .: "inline_message_id"
                           <*> o .: "chat_instance"
                           <*> o .: "game_short_name"
  parseJSON wat = typeMismatch "CallbackQuery" wat

instance FromJSON ChatMember where
  parseJSON (Object o) =
    ChatMember <$> o .: "user"
               <*> o .: "status"
  parseJSON wat = typeMismatch "ChatMember" wat

instance FromJSON ResponseParameters where
  parseJSON (Object o) =
    ResponseParameters <$> o .:? "migrate_to_chat_id"
                       <*> o .:? "retry_after"
  parseJSON wat = typeMismatch "ResponseParameters" wat