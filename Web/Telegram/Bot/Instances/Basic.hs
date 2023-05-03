{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Telegram.Bot.Instances.Basic where


import           Control.Applicative        ((<|>))
import           Data.Aeson
import           Data.Aeson.Types           (typeMismatch)
import           Data.Text                  (unpack)
import           Data.Maybe                 (isJust,fromMaybe)
import qualified Data.Aeson.KeyMap          as KM

import           Web.Telegram.Bot.Types.Basic
import           Web.Telegram.Bot.Types.Static
import           Web.Telegram.Bot.Instances.Static()


----------------------
-- ToJSON INSTANCES --
----------------------

instance ToJSON User where
  toJSON User{..} =
    object' [ "id"         .=! user_id
            , "first_name" .=! user_first_name
            , "last_name"  .=!! user_last_name
            , "username"   .=!! user_username
            ]

instance ToJSON Chat where
  toJSON PrivateChat{..} =
    object' [ "id"         .=! chat_id
            , "type"       .=! toJSON Private
            , "first_name" .=! chat_first_name
            , "last_name"  .=!! chat_last_name
            , "username"   .=!! chat_username
            ]
  toJSON GroupChat{..} =
    object' [ "id"    .=! chat_id
            , "type"  .=! toJSON Group
            , "title" .=! chat_title
            , mBool "all_members_are_administrators" False chat_all_admin
            ]
  toJSON SuperGroupChat{..} =
    object' [ "id"       .=! chat_id
            , "type"     .=! toJSON Supergroup
            , "title"    .=! chat_title
            , mBool "all_members_are_administrators" False chat_all_admin
            , "username" .=!! chat_username
            ]
  toJSON ChannelChat{..} =
    object' [ "id"       .=! chat_id
            , "type"     .=! toJSON Channel
            , "title"    .=! chat_title
            , "username" .=!! chat_username
            ]

instance ToJSON Message where
  toJSON ForwardedMessage{..}
        | Object msg <- toJSON forward_message =
            Object $ KM.fromList [ ("forward_from", toJSON forward_from)
                                 , ("forward_from_chat",toJSON forward_from_chat)
                                 , ("forward_date",toJSON forward_date)
                                 , ("forward_from_message_id",toJSON forward_from_message_id)
                                 ] `KM.union` msg
        | otherwise = error "message parameter isn't an Object in ToJSON Message - ForwardedMessage"
  toJSON other = object' $ extra ++ basis
   where
    basis = [ "message_id"       .=! message_id other
            , "from"             .=! from other
            , "date"             .=! date other
            , "chat"             .=! chat other ]
    extra = case other of
      TextMessage{..} ->
        [ "text"             .=! text
        , "edit_date"        .=!! edit_date
        , "reply_to_message" .=!! reply_to_message
        , mEmptyList "entities" entities ]
      AudioMessage{..} ->
        [ "audio"            .=! audio
        , "caption"          .=!! caption
        , "edit_date"        .=!! edit_date
        , "reply_to_message" .=!! reply_to_message ]
      DocumentMessage{..} ->
        [ "document"         .=! document
        , "caption"          .=!! caption
        , "edit_date"        .=!! edit_date
        , "reply_to_message" .=!! reply_to_message ]
      GameMessage{..} ->
        [ "game"             .=! game
        , "edit_date"        .=!! edit_date
        , "reply_to_message" .=!! reply_to_message ]
      PhotoMessage{..} ->
        [ "photo"            .=! photo
        , "caption"          .=!! caption
        , "edit_date"        .=!! edit_date
        , "reply_to_message" .=!! reply_to_message ]
      StickerMessage{..} ->
        [ "sticker"          .=! sticker
        , "reply_to_message" .=!! reply_to_message ]
      VideoMessage{..} ->
        [ "video"            .=! video
        , "caption"          .=!! caption
        , "edit_date"        .=!! edit_date
        , "reply_to_message" .=!! reply_to_message ]
      VoiceMessage{..} ->
        [ "voice"            .=! voice
        , "caption"          .=!! caption
        , "edit_date"        .=!! edit_date
        , "reply_to_message" .=!! reply_to_message ]
      ContactMessage{..} ->
        [ "contact"          .=! contact
        , "reply_to_message" .=!! reply_to_message ]
      LocationMessage{..} ->
        [ "location"         .=! location
        , "reply_to_message" .=!! reply_to_message ]
      VenueMessage{..} ->
        [ "venue"            .=! venue
        , "reply_to_message" .=!! reply_to_message ]
      NewChatParticipantMessage{..}    -> [ "new_chat_member"         .=! new_chat_member ]
      LeftChatParticipantMessage{..}   -> [ "left_chat_participant"   .=! left_chat_member ]
      NewChatTitleMessage{..}          -> [ "new_chat_title"          .=! new_chat_title ]
      NewChatPhotoMessage{..}          -> [ "new_chat_photo"          .=! new_chat_photo ]
      DeleteChatPhotoMessage{}         -> [ "delete_chat_photo"       .=! Bool True ]
      GroupChatCreatedMessage{}        -> [ "group_chat_created"      .=! Bool True ]
      SuperGroupChatCreatedMessage{}   -> [ "supergroup_chat_created" .=! Bool True ]
      ChannelChatCreatedMessage{}      -> [ "channel_chat_created"    .=! Bool True ]
      MigratedToChatMessage{..}        -> [ "migrate_to_chat_id"      .=! migrate_to_chat_id ]
      MigrateFromChatMessage{..}       -> [ "migrate_from_chat_id"    .=! migrate_from_chat_id ]
      PinnedMessage{..}                -> [ "pinned_message"          .=! pinned_message ]
      -- _ -> [ "error" .=! String "this should never occur, please report"]

instance ToJSON ChannelMessage where
  toJSON (ChannelMessage mId date chat txt editDate ents) =
      object [ "message_id" .= mId
             , "date" .= date
             , "chat" .= chat
             , "text" .= txt
             , "edit_date" .= editDate
             , "entities" .= ents
             ]

instance ToJSON MessageEntity where
  toJSON ent = object $ extra ++ basis
   where
    basis = [ "offset" .= entity_offset ent
            , "length" .= entity_length ent ]
    extra = case ent of
      MentionEntity{}    -> [ "type" .= String "mention" ]
      HashtagEntity{}    -> [ "type" .= String "hashtag" ]
      BotCommandEntity{} -> [ "type" .= String "bot_command" ]
      UrlEntity{}        -> [ "type" .= String "url" ]
      EmailEntity{}      -> [ "type" .= String "email" ]
      PhoneNumberEntity{} -> [ "type" .= String "phone_number" ]
      BoldEntity{}       -> [ "type" .= String "bold" ]
      ItalicEntity{}     -> [ "type" .= String "italic" ]
      CodeEntity{}       -> [ "type" .= String "code" ]
      PreEntity{}        -> [ "type" .= String "pre" ]
      TextLinkEntity{..} ->
        [ "type" .= String "text_link"
        , "url"  .= entity_url ]
      TextMentionEntity{..} ->
        [ "type" .= String "text_mention"
        , "user" .= entity_user ]

instance ToJSON PhotoSize where
  toJSON PhotoSize{..} =
    object' [ "file_id"   .=! photo_file_id
            , "width"     .=! photo_width
            , "height"    .=! photo_height
            , "file_size" .=!! photo_file_size
            ]

instance ToJSON Audio where
  toJSON Audio{..} =
    object' [ "file_id"   .=! audio_file_id
            , "duration"  .=! audio_duration
            , "performer" .=!! audio_performer
            , "title"     .=!! audio_title
            , "mime_type" .=!! audio_mime_type
            , "file_size" .=!! audio_file_size
            ]

instance ToJSON Document where
  toJSON Document{..} =
    object' [ "file_id"   .=! doc_file_id
            , "thumb"     .=!! doc_thumb
            , "file_name" .=!! doc_file_name
            , "mime_type" .=!! doc_mime_type
            , "file_size" .=!! doc_file_size
            ]

instance ToJSON Game where
  toJSON Game{..} =
    object' [ "title"         .=! game_title
            , "description"   .=! game_description
            , "photo"         .=! game_photo
            , "text"          .=!! game_text
            , mEmptyList "text_entities" game_text_entities
            , "animation"     .=!! game_animation
            ]

instance ToJSON Animation where
  toJSON Animation{..} =
    object' [ "file_id"   .=! animation_file_id
            , "thumb"     .=!! animation_thumb
            , "file_name" .=!! animation_file_name
            , "mime_type" .=!! animation_mime_type
            , "file_size" .=!! animation_file_size
            ]

instance ToJSON Sticker where
  toJSON Sticker{..} =
    object' [ "file_id"   .=! sticker_file_id
            , "width"     .=! sticker_width
            , "height"    .=! sticker_height
            , "thumb"     .=!! sticker_thumb
            , "emoji"     .=!! sticker_emoji
            , "file_size" .=!! sticker_file_size
            ]

instance ToJSON Video where
  toJSON Video{..} =
    object' [ "file_id"   .=! video_file_id
            , "width"     .=! video_width
            , "height"    .=! video_height
            , "duration"  .=! video_duration
            , "thumb"     .=!! video_thumb
            , "mime_type" .=!! video_mime_type
            , "file_size" .=!! video_file_size
            ]

instance ToJSON Voice where
  toJSON Voice{..} =
    object' [ "file_id"   .=! voice_file_id
            , "duration"  .=! voice_duration
            , "mime_type" .=!! voice_mime_type
            , "file_size" .=!! voice_file_size
            ]

instance ToJSON Contact where
  toJSON Contact{..} =
    object' [ "phone_number" .=! contact_phone_number
            , "first_name"   .=! contact_first_name
            , "last_name"    .=!! contact_last_name
            , "user_id"      .=!! contact_user_id
            ]

instance ToJSON Location where
  toJSON (Location longitude latitude) =
    object [ "longitude" .= longitude
           , "latitude"  .= latitude
           ]

instance ToJSON Venue where
  toJSON Venue{..} =
    object' [ "location"      .=! venue_location
            , "title"         .=! venue_title
            , "address"       .=! venue_address
            , "foursquare_id" .=!! venue_foursquare_id
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
  toJSON ReplyKeyboardMarkup{..} =
    object' [ "keyboard"          .=! reply_keyboard
            , mBool "resize_keyboard"   False reply_resize_keyboard
            , mBool "one_time_keyboard" False reply_one_time_keyboard
            , mBool "selective"         False reply_selective
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
  toJSON (InlineGameButton txt _game) =
    object [ "text"          .= txt
           , "callback_game" .= Object KM.empty
           ]

instance ToJSON CallbackGame where
  toJSON _ = Object KM.empty

instance ToJSON GameHighScore where
  toJSON (GameHighScore position user score) =
    object [ "position" .= position
           , "user"     .= user
           , "score"    .= score
           ]

instance ToJSON CallbackQuery where
  toJSON CallbackMessage{..} =
    object [ "id"            .= callback_query_id
           , "from"          .= callback_query_from
           , "message"       .= callback_query_message
           , "chat_instance" .= callback_query_chat_instance
           , "data"          .= callback_query_data
           ]
  toJSON CallbackInline{..} =
    object [ "id"                .= callback_query_id
           , "from"              .= callback_query_from
           , "inline_message_id" .= callback_query_inline_message_id
           , "chat_instance"     .= callback_query_chat_instance
           , "data"              .= callback_query_data
           ]
  toJSON CallbackGameMessage{..} =
    object [ "id"              .= callback_query_id
           , "from"            .= callback_query_from
           , "message"         .= callback_query_message
           , "chat_instance"   .= callback_query_chat_instance
           , "game_short_name" .= callback_query_game_short_name
           ]
  toJSON CallbackGameInline{..} =
    object [ "id"                .= callback_query_id
           , "from"              .= callback_query_from
           , "inline_message_id" .= callback_query_inline_message_id
           , "chat_instance"     .= callback_query_chat_instance
           , "game_short_name"   .= callback_query_game_short_name
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

instance ToJSON WebhookInfo where
  toJSON WebhookInfo{..} =
    object' [ "url"                    .=! webhookinfo_url
            , "has_custom_certificate" .=! webhookinfo_has_custom_certificate
            , "pending_update_count"   .=! webhookinfo_pending_update_count
            , "last_error_date"        .=!! webhookinfo_last_error_date
            , "last_error_message"     .=!! webhookinfo_last_error_message
            , "max_connections"        .=!! webhookinfo_max_connections
            , mEmptyList "allowed_updates" webhookinfo_allowed_updates
            ]

------------------------
-- FromJSON INSTANCES --
------------------------

instance FromJSON User where
    parseJSON = withObject "User" $ \o -> User <$> o .: "id"
                                <*> o .: "first_name"
                                <*> o .:? "last_name"
                                <*> o .:? "username"

instance FromJSON Chat where
  parseJSON = withObject "Chat" $ \o ->
    case "type" `KM.lookup` o of
      Nothing  -> fail "no 'type' argument in Chat object"
      Just val -> go o val
   where
    go o = withText "Chat(type)" $ \s ->
      case s of
        "private"    -> PrivateChat <$> o .: "id"
                                    <*> o .: "first_name"
                                    <*> o .:? "last_name"
                                    <*> o .:? "username"
        "group"      -> GroupChat <$> o .: "id"
                                  <*> o .: "title"
                                  <*> o .:? "all_members_are_administrators" .!= False
        "supergroup" -> SuperGroupChat <$> o .: "id"
                                       <*> o .: "title"
                                       <*> o .:? "all_members_are_administrators" .!= False
                                       <*> o .:? "username"
        "channel"    -> ChannelChat <$> o .: "id"
                                    <*> o .: "title"
                                    <*> o .:? "username"
        wat -> fail $ "wrong string as Chat type in Chat object: " <> unpack wat

instance FromJSON Message where
  parseJSON (Object o)
    | isJust $ "forward_date" `KM.lookup` o =
        ForwardedMessage <$> parseJSON (Object $ KM.delete "forward_date" o)
                         <*> o .:? "forward_from"
                         <*> o .:? "forward_from_chat"
                         <*> o .:? "forward_from_message_id"
                         <*> o .: "forward_date"
    | isJust $ "delete_chat_photo" `KM.lookup` o =
        DeleteChatPhotoMessage <$> o .: "message_id"
                               <*> o .: "from"
                               <*> o .: "date"
                               <*> o .: "chat"
    | isJust $ "group_chat_created" `KM.lookup` o =
        GroupChatCreatedMessage <$> o .: "message_id"
                                <*> o .: "from"
                                <*> o .: "date"
                                <*> o .: "chat"
    | isJust $ "supergroup_chat_created" `KM.lookup` o =
        SuperGroupChatCreatedMessage <$> o .: "message_id"
                                     <*> o .: "from"
                                     <*> o .: "date"
                                     <*> o .: "chat"
    {- -| isJust $ KM.lookup "channel_chat_created" o = ChannelChatCreatedMessage <$> o .: "message_id"
                          <*> o .: "from"
                          <*> o .: "date"
                          <*> o .: "chat" -}
    | otherwise = do
        message_id <- o .: "message_id"
        from       <- o .: "from"
        date       <- o .: "date"
        chat       <- o .: "chat"
        TextMessage message_id from date chat <$> o .: "text"
                                              <*> o .:? "edit_date"
                                              <*> o .:? "reply_to_message"
                                              <*> o .:? "entities" .!= []
          <|> AudioMessage message_id from date chat <$> o .: "audio"
                                                     <*> o .:? "caption"
                                                     <*> o .:? "edit_date"
                                                     <*> o .:? "reply_to_message"
          <|> DocumentMessage message_id from date chat <$> o .: "document"
                                                        <*> o .:? "caption"
                                                        <*> o .:? "edit_date"
                                                        <*> o .:? "reply_to_message"
          <|> GameMessage message_id from date chat <$> o .: "game"
                                                    <*> o .:? "edit_date"
                                                    <*> o .:? "reply_to_message"
          <|> PhotoMessage message_id from date chat <$> o .: "photo"
                                                     <*> o .:? "caption"
                                                     <*> o .:? "edit_date"
                                                     <*> o .:? "reply_to_message"
          <|> StickerMessage message_id from date chat <$> o .: "sticker"
                                                       <*> o .:? "reply_to_message"
          <|> VideoMessage message_id from date chat <$> o .: "video"
                                                     <*> o .:? "caption"
                                                     <*> o .:? "edit_date"
                                                     <*> o .:? "reply_to_message"
          <|> VoiceMessage message_id from date chat <$> o .: "voice"
                                                     <*> o .:? "caption"
                                                     <*> o .:? "edit_date"
                                                     <*> o .:? "reply_to_message"
          <|> ContactMessage message_id from date chat <$> o .: "contact"
                                                       <*> o .:? "reply_to_message"
          <|> LocationMessage message_id from date chat <$> o .: "location"
                                                        <*> o .:? "reply_to_message"
          <|> VenueMessage message_id from date chat <$> o .: "venue"
                                                     <*> o .:? "reply_to_message"
          <|> NewChatParticipantMessage message_id from date chat <$> o .: "new_chat_participant"
          <|> LeftChatParticipantMessage message_id from date chat <$> o .: "left_chat_participant"
          <|> NewChatTitleMessage message_id from date chat <$> o .: "new_chat_title"
          <|> NewChatPhotoMessage message_id from date chat <$> o .: "new_chat_photo"
          <|> MigratedToChatMessage message_id from date chat <$> o .: "migrate_to_chat_id"
          <|> MigrateFromChatMessage message_id from date chat <$> o .: "migrate_from_chat_id"
          <|> PinnedMessage message_id from date chat <$> o .: "pinned_message"
  parseJSON wat = typeMismatch "Message" wat

instance FromJSON ChannelMessage where
  parseJSON = withObject "ChannelMessage" $ \o ->
      ChannelMessage <$> o .: "message_id"
                     <*> o .: "date"
                     <*> o .: "chat"
                     <*> o .:? "text"
                     <*> o .:? "edit_date"
                     <*> o .:? "entities" .!= []

instance FromJSON MessageEntity where
  parseJSON = withObject "MessageEntity" $ \o ->
    case "type" `KM.lookup` o of
      Nothing  -> fail "No 'type' parameter in MessageEntity"
      Just val -> go o val
   where
    go o = withText "MessageEntity(type)" $ \s -> do
      offset  <- o .: "offset"
      length' <- o .: "length"
      case s of
        "mention"     -> return $ MentionEntity     offset length'
        "hashtag"     -> return $ HashtagEntity     offset length'
        "bot_command" -> return $ BotCommandEntity  offset length'
        "url"         -> return $ UrlEntity         offset length'
        "email"       -> return $ EmailEntity       offset length'
        "phone_number" -> return $ PhoneNumberEntity offset length'
        "bold"        -> return $ BoldEntity        offset length'
        "italic"      -> return $ ItalicEntity      offset length'
        "code"        -> return $ CodeEntity        offset length'
        "pre"         -> return $ PreEntity         offset length'
        "text_link"   ->          TextLinkEntity    offset length' <$> o .: "url"
        "text_mention" ->         TextMentionEntity offset length' <$> o .: "user"
        wat -> fail $ "Wrong String \"" <> unpack wat <> "\" in MessageEntity's 'type' parameter"

instance FromJSON PhotoSize where
  parseJSON = withObject "PhotoSize" $ \o ->
    PhotoSize <$> o .: "file_id"
              <*> o .: "width"
              <*> o .: "height"
              <*> o .:? "file_size"

instance FromJSON Audio where
  parseJSON = withObject "Audio" $ \o ->
    Audio <$> o .: "file_id"
          <*> o .: "duration"
          <*> o .:? "performer"
          <*> o .:? "title"
          <*> o .:? "mime_type"
          <*> o .:? "file_size"

instance FromJSON Document where
  parseJSON = withObject "Document" $ \o ->
    Document <$> o .: "file_id"
             <*> o .:? "thumb"
             <*> o .:? "file_name"
             <*> o .:? "mime_type"
             <*> o .:? "file_size"

instance FromJSON Game where
  parseJSON = withObject "Game" $ \o ->
    Game <$> o .: "title"
         <*> o .: "description"
         <*> o .: "photo"
         <*> o .:? "text"
         <*> o .:? "text_entities" .!= []
         <*> o .:? "animation"

instance FromJSON Animation where
  parseJSON = withObject "Animation" $ \o ->
    Animation <$> o .: "file_id"
              <*> o .:? "thumb"
              <*> o .:? "file_name"
              <*> o .:? "mime_type"
              <*> o .:? "file_size"

instance FromJSON Sticker where
  parseJSON = withObject "Sticker" $ \o ->
    Sticker <$> o .: "file_id"
            <*> o .: "width"
            <*> o .: "height"
            <*> o .:? "thumb"
            <*> o .:? "emoji"
            <*> o .:? "file_size"

instance FromJSON Video where
  parseJSON = withObject "Video" $ \o ->
    Video <$> o .: "file_id"
          <*> o .: "width"
          <*> o .: "height"
          <*> o .: "duration"
          <*> o .:? "thumb"
          <*> o .:? "mime_type"
          <*> o .:? "file_size"

instance FromJSON Voice where
  parseJSON = withObject "Voice" $ \o ->
    Voice <$> o .: "file_id"
          <*> o .: "duration"
          <*> o .:? "mime_type"
          <*> o .:? "file_size"

instance FromJSON Contact where
  parseJSON = withObject "Contact" $ \o ->
    Contact <$> o .: "phone_number"
            <*> o .: "first_name"
            <*> o .:? "last_name"
            <*> o .:? "user_id"

instance FromJSON Location where
  parseJSON = withObject "Location" $ \o ->
    Location <$> o .: "longitude"
             <*> o .: "latitude"

instance FromJSON Venue where
  parseJSON = withObject "Venue" $ \o ->
    Venue <$> o .: "location"
          <*> o .: "title"
          <*> o .: "address"
          <*> o .:? "foursquare_id"

instance FromJSON UserProfilePhotos where
  parseJSON = withObject "UserProfilePhotos" $ \o ->
    UserProfilePhotos <$> o .: "total_count"
                      <*> o .: "photos"

instance FromJSON File where
  parseJSON = withObject "File" $ \o ->
    File <$> o .: "file_id"
         <*> o .:? "file_size"
         <*> o .:? "file_path"

instance FromJSON ReplyKeyboard where
  parseJSON (Object o)
    | "remove_keyboard" `KM.lookup` o == Just (Bool True) = ReplyKeyboardRemove <$> o .:? "selective" .!= False
    | "force_reply"     `KM.lookup` o == Just (Bool True) = ForceReply <$> o .:? "selective" .!= False
    | otherwise =
        InlineKeyboardMarkup <$> o .: "inline_keyboard"
    <|> ReplyKeyboardMarkup <$> o .: "keyboard"
                            <*> o .:? "resize_keyboard"   .!= False
                            <*> o .:? "one_time_keyboard" .!= False
                            <*> o .:? "selective"         .!= False
  parseJSON wat = typeMismatch "ReplyKeyboard" wat

instance FromJSON KeyboardButton where
  parseJSON (Object o)
    | KM.lookup "request_contact" o  == Just (Bool True) = ContactButton <$> o .: "text"
    | KM.lookup "request_location" o == Just (Bool True) = LocationButton <$> o .: "text"
    | otherwise = TextButton <$> o .: "text"
  parseJSON wat = typeMismatch "KeyboardButton" wat

instance FromJSON InlineKeyboardButton where
  parseJSON = withObject "InlineKeyboardButton" $ \o -> do
    txt <- o .: "text"
    InlineUrlButton                 txt <$> o .: "url"
      <|> InlineCallbackButton      txt <$> o .: "callback_data"
      <|> InlineSwitchButton        txt <$> o .:? "switch_inline_query"
      <|> InlineSwitchCurrentButton txt <$> o .:? "switch_inline_query_current_chat"
      <|> InlineGameButton          txt <$> o .: "callback_game"

instance FromJSON CallbackGame where
  parseJSON _ = pure CallbackGame

instance FromJSON GameHighScore where
  parseJSON = withObject "GameHighScore" $ \o ->
    GameHighScore <$> o .: "position"
                  <*> o .: "user"
                  <*> o .: "score"

instance FromJSON CallbackQuery where
  parseJSON = withObject "CallbackQuery" $ \o -> do
    ident <- o .: "id"
    from  <- o .: "from"
    CallbackMessage ident from <$> o .: "message"
                               <*> o .: "chat_instance"
                               <*> o .: "data"
      <|> CallbackInline ident from <$> o .: "inline_message_id"
                                    <*> o .: "chat_instance"
                                    <*> o .: "data"
      <|> CallbackGameMessage ident from <$> o .: "message"
                                         <*> o .: "chat_instance"
                                         <*> o .: "game_short_name"
      <|> CallbackGameInline ident from <$> o .: "inline_message_id"
                                        <*> o .: "chat_instance"
                                        <*> o .: "game_short_name"

instance FromJSON ChatMember where
  parseJSON = withObject "ChatMember" $ \o ->
    ChatMember <$> o .: "user"
               <*> o .: "status"

instance FromJSON ResponseParameters where
  parseJSON = withObject "ResponseParameters" $ \o ->
    ResponseParameters <$> o .:? "migrate_to_chat_id"
                       <*> o .:? "retry_after"

instance FromJSON WebhookInfo where
  parseJSON = withObject "WebhookInfo" $ \o ->
    WebhookInfo <$> o .: "url"
                <*> o .: "has_custom_certificate"
                <*> o .: "pending_update_count"
                <*> o .:? "last_error_date"
                <*> o .:? "last_error_message"
                <*> o .:? "max_connections"
                <*> o .:? "allowed_updates" .!= []
