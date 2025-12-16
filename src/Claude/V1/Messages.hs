-- | @\/v1\/messages@
--
-- This module provides types and utilities for the Claude Messages API.
module Claude.V1.Messages
    ( -- * Main types
      CreateMessage(..)
    , _CreateMessage
    , MessageResponse(..)
    , MessageStreamEvent(..)
      -- * Content types
    , Content(..)
    , ContentBlock(..)
    , TextContent(..)
    , ImageSource(..)
    , ToolUseContent(..)
    , ToolResultContent(..)
      -- * Message types
    , Message(..)
    , Role(..)
      -- * Response types
    , StopReason(..)
    , Usage(..)
      -- * Tool types
    , Tool(..)
    , ToolChoice(..)
    , InputSchema(..)
      -- * Streaming types
    , ContentBlockDelta(..)
    , TextDelta(..)
    , InputJsonDelta(..)
    , MessageDelta(..)
    , StreamUsage(..)
      -- * Servant
    , API
    ) where

import Claude.Prelude

import qualified Data.Aeson as Aeson

-- | Role of a message participant
data Role = User | Assistant
    deriving stock (Eq, Generic, Show)

instance FromJSON Role where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Role where
    toJSON = genericToJSON aesonOptions

-- | Image source for vision capabilities
data ImageSource = ImageSource
    { type_ :: Text           -- ^ "base64"
    , media_type :: Text      -- ^ "image/jpeg", "image/png", etc.
    , data_ :: Text           -- ^ Base64 encoded image data
    } deriving stock (Generic, Show)

instance FromJSON ImageSource where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON ImageSource where
    toJSON = genericToJSON aesonOptions

-- | Text content block
data TextContent = TextContent
    { text :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Tool use content block (in assistant messages)
data ToolUseContent = ToolUseContent
    { id :: Text
    , name :: Text
    , input :: Value
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Tool result content (in user messages, following tool use)
data ToolResultContent = ToolResultContent
    { tool_use_id :: Text
    , content :: Maybe Text
    , is_error :: Maybe Bool
    } deriving stock (Generic, Show)

instance FromJSON ToolResultContent where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON ToolResultContent where
    toJSON = genericToJSON aesonOptions

-- | Content block in a message (for requests)
data Content
    = Content_Text { text :: Text }
    | Content_Image { source :: ImageSource }
    | Content_Tool_Use { id :: Text, name :: Text, input :: Value }
    | Content_Tool_Result { tool_use_id :: Text, content :: Maybe Text, is_error :: Maybe Bool }
    deriving stock (Generic, Show)

contentOptions :: Options
contentOptions = aesonOptions
    { sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
    , tagSingleConstructors = True
    , constructorTagModifier = stripPrefix "Content_"
    }

instance FromJSON Content where
    parseJSON = genericParseJSON contentOptions

instance ToJSON Content where
    toJSON = genericToJSON contentOptions

-- | Content block in a response
data ContentBlock
    = ContentBlock_Text { text :: Text }
    | ContentBlock_Tool_Use { id :: Text, name :: Text, input :: Value }
    deriving stock (Generic, Show)

contentBlockOptions :: Options
contentBlockOptions = aesonOptions
    { sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
    , tagSingleConstructors = True
    , constructorTagModifier = stripPrefix "ContentBlock_"
    }

instance FromJSON ContentBlock where
    parseJSON = genericParseJSON contentBlockOptions

instance ToJSON ContentBlock where
    toJSON = genericToJSON contentBlockOptions

-- | A message in the conversation
data Message = Message
    { role :: Role
    , content :: Vector Content
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Reason why the model stopped generating
data StopReason
    = End_Turn
    | Max_Tokens
    | Stop_Sequence
    | Tool_Use
    deriving stock (Eq, Generic, Show)

stopReasonOptions :: Options
stopReasonOptions = aesonOptions
    { constructorTagModifier = stripPrefix "" }

instance FromJSON StopReason where
    parseJSON = genericParseJSON stopReasonOptions

instance ToJSON StopReason where
    toJSON = genericToJSON stopReasonOptions

-- | Token usage information
data Usage = Usage
    { input_tokens :: Natural
    , output_tokens :: Natural
    , cache_creation_input_tokens :: Maybe Natural
    , cache_read_input_tokens :: Maybe Natural
    } deriving stock (Generic, Show)

instance FromJSON Usage where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Usage where
    toJSON = genericToJSON aesonOptions

-- | Tool input schema (JSON Schema)
data InputSchema = InputSchema
    { type_ :: Text
    , properties :: Maybe Value
    , required :: Maybe (Vector Text)
    } deriving stock (Generic, Show)

instance FromJSON InputSchema where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON InputSchema where
    toJSON = genericToJSON aesonOptions

-- | A tool that can be used by Claude
data Tool = Tool
    { name :: Text
    , description :: Maybe Text
    , input_schema :: InputSchema
    } deriving stock (Generic, Show)

instance FromJSON Tool where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Tool where
    toJSON = genericToJSON aesonOptions

-- | Controls which tool the model should use
data ToolChoice
    = ToolChoice_Auto
    | ToolChoice_Any
    | ToolChoice_Tool { name :: Text }
    deriving stock (Generic, Show)

instance FromJSON ToolChoice where
    parseJSON (Aeson.Object o) = do
        t <- o Aeson..: "type"
        case (t :: Text) of
            "auto" -> pure ToolChoice_Auto
            "any" -> pure ToolChoice_Any
            "tool" -> ToolChoice_Tool <$> o Aeson..: "name"
            _ -> fail "Unknown tool choice type"
    parseJSON _ = fail "Invalid tool choice"

instance ToJSON ToolChoice where
    toJSON ToolChoice_Auto = Aeson.object ["type" Aeson..= ("auto" :: Text)]
    toJSON ToolChoice_Any = Aeson.object ["type" Aeson..= ("any" :: Text)]
    toJSON (ToolChoice_Tool n) = Aeson.object
        [ "type" Aeson..= ("tool" :: Text)
        , "name" Aeson..= n
        ]

-- | Response from the Messages API
data MessageResponse = MessageResponse
    { id :: Text
    , type_ :: Text
    , role :: Role
    , content :: Vector ContentBlock
    , model :: Text
    , stop_reason :: Maybe StopReason
    , stop_sequence :: Maybe Text
    , usage :: Usage
    } deriving stock (Generic, Show)

instance FromJSON MessageResponse where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON MessageResponse where
    toJSON = genericToJSON aesonOptions

-- | Request body for @\/v1\/messages@
data CreateMessage = CreateMessage
    { model :: Text
    , messages :: Vector Message
    , max_tokens :: Natural
    , system :: Maybe Text
    , temperature :: Maybe Double
    , top_p :: Maybe Double
    , top_k :: Maybe Natural
    , stop_sequences :: Maybe (Vector Text)
    , stream :: Maybe Bool
    , metadata :: Maybe (Map Text Text)
    , tools :: Maybe (Vector Tool)
    , tool_choice :: Maybe ToolChoice
    } deriving stock (Generic, Show)

instance FromJSON CreateMessage where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON CreateMessage where
    toJSON = genericToJSON aesonOptions

-- | Default CreateMessage with only required fields
_CreateMessage :: CreateMessage
_CreateMessage = CreateMessage
    { model = ""
    , messages = mempty
    , max_tokens = 1024
    , system = Nothing
    , temperature = Nothing
    , top_p = Nothing
    , top_k = Nothing
    , stop_sequences = Nothing
    , stream = Nothing
    , metadata = Nothing
    , tools = Nothing
    , tool_choice = Nothing
    }

-- | Text delta in streaming
data TextDelta = TextDelta
    { text :: Text
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Input JSON delta in streaming (for tool use)
data InputJsonDelta = InputJsonDelta
    { partial_json :: Text
    } deriving stock (Generic, Show)

instance FromJSON InputJsonDelta where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON InputJsonDelta where
    toJSON = genericToJSON aesonOptions

-- | Content block delta in streaming
data ContentBlockDelta
    = Delta_Text_Delta { text :: Text }
    | Delta_Input_Json_Delta { partial_json :: Text }
    deriving stock (Generic, Show)

contentBlockDeltaOptions :: Options
contentBlockDeltaOptions = aesonOptions
    { sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
    , tagSingleConstructors = True
    , constructorTagModifier = \s -> case s of
        "Delta_Text_Delta" -> "text_delta"
        "Delta_Input_Json_Delta" -> "input_json_delta"
        _ -> s
    }

instance FromJSON ContentBlockDelta where
    parseJSON = genericParseJSON contentBlockDeltaOptions

instance ToJSON ContentBlockDelta where
    toJSON = genericToJSON contentBlockDeltaOptions

-- | Message delta in streaming (for stop_reason, etc.)
data MessageDelta = MessageDelta
    { stop_reason :: Maybe StopReason
    , stop_sequence :: Maybe Text
    } deriving stock (Generic, Show)

instance FromJSON MessageDelta where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON MessageDelta where
    toJSON = genericToJSON aesonOptions

-- | Usage in streaming message_delta events
data StreamUsage = StreamUsage
    { output_tokens :: Natural
    } deriving stock (Generic, Show)

instance FromJSON StreamUsage where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON StreamUsage where
    toJSON = genericToJSON aesonOptions

-- | Streaming events for @\/v1\/messages@
data MessageStreamEvent
    = Message_Start
        { message :: MessageResponse
        }
    | Content_Block_Start
        { index :: Natural
        , content_block :: ContentBlock
        }
    | Content_Block_Delta
        { index :: Natural
        , delta :: ContentBlockDelta
        }
    | Content_Block_Stop
        { index :: Natural
        }
    | Message_Delta
        { message_delta :: MessageDelta
        , usage :: StreamUsage
        }
    | Message_Stop
    | Ping
    | Error
        { error :: Value
        }
    deriving stock (Generic, Show)

messageStreamEventOptions :: Options
messageStreamEventOptions = aesonOptions
    { sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "" }
    , tagSingleConstructors = True
    , constructorTagModifier = \s -> case s of
        "Message_Start" -> "message_start"
        "Content_Block_Start" -> "content_block_start"
        "Content_Block_Delta" -> "content_block_delta"
        "Content_Block_Stop" -> "content_block_stop"
        "Message_Delta" -> "message_delta"
        "Message_Stop" -> "message_stop"
        "Ping" -> "ping"
        "Error" -> "error"
        _ -> s
    , fieldLabelModifier = \s -> case s of
        "message_delta" -> "delta"
        other -> labelModifier other
    }

instance FromJSON MessageStreamEvent where
    parseJSON = genericParseJSON messageStreamEventOptions

instance ToJSON MessageStreamEvent where
    toJSON = genericToJSON messageStreamEventOptions

-- | Servant API for @\/v1\/messages@
type API =
        "messages"
    :>  ReqBody '[JSON] CreateMessage
    :>  Post '[JSON] MessageResponse
