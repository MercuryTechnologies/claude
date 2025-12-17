-- | @\/v1\/messages@
--
-- This module provides types and utilities for the Claude Messages API.
module Claude.V1.Messages
    ( -- * Main types
      CreateMessage(..)
    , _CreateMessage
    , MessageResponse(..)
    , MessageStreamEvent(..)
      -- * Structured outputs
    , OutputFormat(..)
    , jsonSchemaFormat
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
    , ServerToolUseUsage(..)
      -- * Tool search response types
    , ToolReference(..)
    , ToolSearchToolResultContent(..)
      -- * Programmatic tool calling (PTC) types
    , ContainerInfo(..)
    , ToolCaller(..)
    , CodeExecutionResult(..)
    , CodeExecutionToolResultContent(..)
    , contentBlockToContent
      -- * Tool types (re-exported from Claude.V1.Tool)
    , Tool(..)
    , ToolChoice(..)
    , InputSchema(..)
    , ToolDefinition(..)
    , ToolSearchTool(..)
    , ToolSearchToolType(..)
    , functionTool
    , strictFunctionTool
    , inlineTool
    , deferredTool
    , toolSearchRegex
    , toolSearchBm25
    , codeExecutionTool
    , allowedCallersCodeExecution
    , allowCallers
    , toolChoiceAuto
    , toolChoiceAny
    , toolChoiceTool
      -- * Streaming types
    , ContentBlockDelta(..)
    , TextDelta(..)
    , InputJsonDelta(..)
    , MessageDelta(..)
    , StreamUsage(..)
      -- * Token counting
    , CountTokensRequest(..)
    , _CountTokensRequest
    , TokenCount(..)
      -- * Prompt caching
    , CacheControl(..)
    , ephemeralCache
      -- * Convenience constructors
    , textContent
    , imageContent
      -- * Servant
    , API
    , MessagesAPI
    , CountTokensAPI
    ) where

import           Claude.Prelude
import           Claude.V1.Tool
    ( InputSchema(..)
    , Tool(..)
    , ToolChoice(..)
    , ToolDefinition(..)
    , ToolSearchTool(..)
    , ToolSearchToolType(..)
    , allowCallers
    , allowedCallersCodeExecution
    , codeExecutionTool
    , deferredTool
    , functionTool
    , inlineTool
    , strictFunctionTool
    , toolChoiceAny
    , toolChoiceAuto
    , toolChoiceTool
    , toolSearchBm25
    , toolSearchRegex
    )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Time (UTCTime)

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

-- | Cache control for prompt caching
data CacheControl = CacheControl
    { type_ :: Text  -- ^ Currently only "ephemeral" is supported
    } deriving stock (Generic, Show)

instance FromJSON CacheControl where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON CacheControl where
    toJSON = genericToJSON aesonOptions

-- | Convenience constructor for ephemeral cache control
ephemeralCache :: CacheControl
ephemeralCache = CacheControl{ type_ = "ephemeral" }

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
--
-- For programmatic tool calling (PTC), replay assistant messages using:
--
-- * 'Content_Tool_Use' with optional @caller@ field
-- * 'Content_Server_Tool_Use' for code execution invocations
data Content
    = Content_Text
        { text :: Text
        , cache_control :: Maybe CacheControl
        }
    | Content_Image
        { source :: ImageSource
        , cache_control :: Maybe CacheControl
        }
    | Content_Tool_Use
        { id :: Text
        , name :: Text
        , input :: Value
        , caller :: Maybe ToolCaller
        }
    | Content_Server_Tool_Use
        { id :: Text
        , name :: Text
        , input :: Value
        }
    | Content_Tool_Result { tool_use_id :: Text, content :: Maybe Text, is_error :: Maybe Bool }
    deriving stock (Generic, Show)

-- | Create a text content block without cache control
textContent :: Text -> Content
textContent t = Content_Text{ text = t, cache_control = Nothing }

-- | Create an image content block without cache control
imageContent :: ImageSource -> Content
imageContent src = Content_Image{ source = src, cache_control = Nothing }

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

-- | Convert a response 'ContentBlock' to a request 'Content' for replaying
--
-- This is useful for programmatic tool calling (PTC) where you need to
-- replay assistant messages in subsequent requests.
--
-- Returns 'Nothing' for content blocks that don't need to be replayed:
--
-- * 'ContentBlock_Tool_Search_Tool_Result'
-- * 'ContentBlock_Code_Execution_Tool_Result'
-- * 'ContentBlock_Unknown'
contentBlockToContent :: ContentBlock -> Maybe Content
contentBlockToContent (ContentBlock_Text t) =
    Just Content_Text{ text = t, cache_control = Nothing }
contentBlockToContent (ContentBlock_Tool_Use toolId toolName toolInput toolCaller) =
    Just Content_Tool_Use
        { id = toolId
        , name = toolName
        , input = toolInput
        , caller = toolCaller
        }
contentBlockToContent (ContentBlock_Server_Tool_Use toolId toolName toolInput) =
    Just Content_Server_Tool_Use
        { id = toolId
        , name = toolName
        , input = toolInput
        }
contentBlockToContent ContentBlock_Tool_Search_Tool_Result{} = Nothing
contentBlockToContent ContentBlock_Code_Execution_Tool_Result{} = Nothing
contentBlockToContent ContentBlock_Unknown{} = Nothing

-- | A reference to a tool found by tool search
data ToolReference = ToolReference
    { tool_name :: Text
    } deriving stock (Eq, Generic, Show)

instance FromJSON ToolReference where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON ToolReference where
    toJSON = genericToJSON aesonOptions

-- | Content of a tool search tool result
--
-- This can be either successful search results with tool references,
-- an error, or an unknown type for forward compatibility.
data ToolSearchToolResultContent
    = ToolSearchResult
        { tool_references :: Vector ToolReference
        }
    | ToolSearchError
        { error_code :: Text
        }
    | ToolSearchResultContent_Unknown Value
    deriving stock (Eq, Show)

instance FromJSON ToolSearchToolResultContent where
    parseJSON = Aeson.withObject "ToolSearchToolResultContent" $ \o -> do
        t <- o Aeson..: "type"
        case (t :: Text) of
            "tool_search_result" -> do
                tool_references <- o Aeson..: "tool_references"
                pure ToolSearchResult{ tool_references }
            "error" -> do
                error_code <- o Aeson..: "error_code"
                pure ToolSearchError{ error_code }
            _ -> pure (ToolSearchResultContent_Unknown (Aeson.Object o))

instance ToJSON ToolSearchToolResultContent where
    toJSON (ToolSearchResult refs) = Aeson.object
        [ "type" Aeson..= ("tool_search_result" :: Text)
        , "tool_references" Aeson..= refs
        ]
    toJSON (ToolSearchError code) = Aeson.object
        [ "type" Aeson..= ("error" :: Text)
        , "error_code" Aeson..= code
        ]
    toJSON (ToolSearchResultContent_Unknown v) = v

-- | Container information for programmatic tool calling (PTC)
--
-- When using code execution, Claude runs code in a container. The container
-- can be reused across multiple turns by passing its @id@ in subsequent requests.
data ContainerInfo = ContainerInfo
    { id :: Text
    , expires_at :: UTCTime
    } deriving stock (Eq, Generic, Show)

instance FromJSON ContainerInfo where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON ContainerInfo where
    toJSON = genericToJSON aesonOptions

-- | Identifies who called a tool (for programmatic tool calling)
--
-- * 'ToolCaller_Direct': Claude called the tool directly
-- * 'ToolCaller_CodeExecution': Code execution called the tool programmatically
-- * 'ToolCaller_Unknown': Unknown caller type (forward compatibility)
data ToolCaller
    = ToolCaller_Direct
    | ToolCaller_CodeExecution { tool_id :: Text }
    | ToolCaller_Unknown Value
    deriving stock (Eq, Show)

instance FromJSON ToolCaller where
    parseJSON = Aeson.withObject "ToolCaller" $ \o -> do
        t <- o Aeson..: "type"
        case (t :: Text) of
            "direct" -> pure ToolCaller_Direct
            "code_execution_20250825" -> do
                tool_id <- o Aeson..: "tool_id"
                pure ToolCaller_CodeExecution{ tool_id }
            _ -> pure (ToolCaller_Unknown (Aeson.Object o))

instance ToJSON ToolCaller where
    toJSON ToolCaller_Direct = Aeson.object
        [ "type" Aeson..= ("direct" :: Text)
        ]
    toJSON (ToolCaller_CodeExecution tid) = Aeson.object
        [ "type" Aeson..= ("code_execution_20250825" :: Text)
        , "tool_id" Aeson..= tid
        ]
    toJSON (ToolCaller_Unknown v) = v

-- | Result from code execution
--
-- Contains stdout, stderr, return code, and any additional content
-- (e.g., generated files, images).
data CodeExecutionResult = CodeExecutionResult
    { stdout :: Text
    , stderr :: Text
    , return_code :: Int
    , content :: Vector Value
    } deriving stock (Eq, Generic, Show)

instance FromJSON CodeExecutionResult where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON CodeExecutionResult where
    toJSON = genericToJSON aesonOptions

-- | Content of a code execution tool result
--
-- * 'CodeExecutionResultContent': Successful execution with stdout/stderr
-- * 'CodeExecutionToolResultContent_Unknown': Unknown content type (forward compatibility)
data CodeExecutionToolResultContent
    = CodeExecutionResultContent CodeExecutionResult
    | CodeExecutionToolResultContent_Unknown Value
    deriving stock (Eq, Show)

instance FromJSON CodeExecutionToolResultContent where
    parseJSON = Aeson.withObject "CodeExecutionToolResultContent" $ \o -> do
        t <- o Aeson..: "type"
        case (t :: Text) of
            "code_execution_result" -> do
                result <- Aeson.parseJSON (Aeson.Object o)
                pure (CodeExecutionResultContent result)
            _ -> pure (CodeExecutionToolResultContent_Unknown (Aeson.Object o))

instance ToJSON CodeExecutionToolResultContent where
    toJSON (CodeExecutionResultContent result) =
        case Aeson.toJSON result of
            Aeson.Object o -> Aeson.Object (KeyMap.insert "type" (Aeson.String "code_execution_result") o)
            v -> v
    toJSON (CodeExecutionToolResultContent_Unknown v) = v

-- | Content block in a response
--
-- Extended to support:
--
-- * @server_tool_use@: Server-initiated tool use (e.g., tool search, code execution)
-- * @tool_search_tool_result@: Results from server-side tool search
-- * @code_execution_tool_result@: Results from code execution (PTC)
--
-- For programmatic tool calling, 'ContentBlock_Tool_Use' includes an optional
-- @caller@ field indicating whether the tool was called directly by Claude
-- or programmatically by code execution.
data ContentBlock
    = ContentBlock_Text { text :: Text }
    | ContentBlock_Tool_Use
        { id :: Text
        , name :: Text
        , input :: Value
        , caller :: Maybe ToolCaller
        }
    | ContentBlock_Server_Tool_Use { id :: Text, name :: Text, input :: Value }
    | ContentBlock_Tool_Search_Tool_Result
        { tool_use_id :: Text
        , tool_search_content :: ToolSearchToolResultContent
        }
    | ContentBlock_Code_Execution_Tool_Result
        { tool_use_id :: Text
        , code_execution_content :: CodeExecutionToolResultContent
        }
    | ContentBlock_Unknown { type_ :: Text, raw :: Value }
    deriving stock (Generic, Show)

instance FromJSON ContentBlock where
    parseJSON = Aeson.withObject "ContentBlock" $ \o -> do
        t <- o Aeson..: "type"
        case (t :: Text) of
            "text" -> ContentBlock_Text <$> o Aeson..: "text"
            "tool_use" -> ContentBlock_Tool_Use
                <$> o Aeson..: "id"
                <*> o Aeson..: "name"
                <*> o Aeson..: "input"
                <*> o Aeson..:? "caller"
            "server_tool_use" -> ContentBlock_Server_Tool_Use
                <$> o Aeson..: "id"
                <*> o Aeson..: "name"
                <*> o Aeson..: "input"
            "tool_search_tool_result" -> do
                toolUseId <- o Aeson..: "tool_use_id"
                searchContent <- o Aeson..: "content"
                pure ContentBlock_Tool_Search_Tool_Result
                    { tool_use_id = toolUseId
                    , tool_search_content = searchContent
                    }
            "code_execution_tool_result" -> do
                toolUseId <- o Aeson..: "tool_use_id"
                execContent <- o Aeson..: "content"
                pure ContentBlock_Code_Execution_Tool_Result
                    { tool_use_id = toolUseId
                    , code_execution_content = execContent
                    }
            _ -> pure (ContentBlock_Unknown t (Aeson.Object o))

instance ToJSON ContentBlock where
    toJSON (ContentBlock_Text t) = Aeson.object
        [ "type" Aeson..= ("text" :: Text)
        , "text" Aeson..= t
        ]
    toJSON (ContentBlock_Tool_Use toolId toolName toolInput toolCaller) = Aeson.object $
        [ "type" Aeson..= ("tool_use" :: Text)
        , "id" Aeson..= toolId
        , "name" Aeson..= toolName
        , "input" Aeson..= toolInput
        ] <> maybe [] (\c -> ["caller" Aeson..= c]) toolCaller
    toJSON (ContentBlock_Server_Tool_Use toolId toolName toolInput) = Aeson.object
        [ "type" Aeson..= ("server_tool_use" :: Text)
        , "id" Aeson..= toolId
        , "name" Aeson..= toolName
        , "input" Aeson..= toolInput
        ]
    toJSON (ContentBlock_Tool_Search_Tool_Result toolUseId searchContent) = Aeson.object
        [ "type" Aeson..= ("tool_search_tool_result" :: Text)
        , "tool_use_id" Aeson..= toolUseId
        , "content" Aeson..= searchContent
        ]
    toJSON (ContentBlock_Code_Execution_Tool_Result toolUseId execContent) = Aeson.object
        [ "type" Aeson..= ("code_execution_tool_result" :: Text)
        , "tool_use_id" Aeson..= toolUseId
        , "content" Aeson..= execContent
        ]
    toJSON (ContentBlock_Unknown _typeName rawVal) = rawVal

-- | A message in the conversation
--
-- The optional @cache_control@ field allows setting cache breakpoints at
-- the message level (in addition to content-level caching).
data Message = Message
    { role :: Role
    , content :: Vector Content
    , cache_control :: Maybe CacheControl
    } deriving stock (Generic, Show)

instance FromJSON Message where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Message where
    toJSON = genericToJSON aesonOptions

-- | Reason why the model stopped generating
data StopReason
    = End_Turn
    | Max_Tokens
    | Stop_Sequence
    | Tool_Use
    | Refusal
    -- ^ Model refused the request for safety reasons (structured outputs only)
    deriving stock (Eq, Generic, Show)

stopReasonOptions :: Options
stopReasonOptions = aesonOptions
    { constructorTagModifier = stripPrefix "" }

instance FromJSON StopReason where
    parseJSON = genericParseJSON stopReasonOptions

instance ToJSON StopReason where
    toJSON = genericToJSON stopReasonOptions

-- | Server tool use usage information (e.g., tool search requests, web search requests)
data ServerToolUseUsage = ServerToolUseUsage
    { web_search_requests :: Maybe Natural
    , tool_search_requests :: Maybe Natural
    } deriving stock (Eq, Generic, Show)

instance FromJSON ServerToolUseUsage where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON ServerToolUseUsage where
    toJSON = genericToJSON aesonOptions

-- | Token usage information
data Usage = Usage
    { input_tokens :: Natural
    , output_tokens :: Natural
    , cache_creation_input_tokens :: Maybe Natural
    , cache_read_input_tokens :: Maybe Natural
    , server_tool_use :: Maybe ServerToolUseUsage
    } deriving stock (Generic, Show)

instance FromJSON Usage where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Usage where
    toJSON = genericToJSON aesonOptions

-- | Response from the Messages API
--
-- For programmatic tool calling (PTC), the @container@ field contains
-- information about the code execution container, which can be reused
-- across turns by passing its @id@ in subsequent requests.
data MessageResponse = MessageResponse
    { id :: Text
    , type_ :: Text
    , role :: Role
    , content :: Vector ContentBlock
    , model :: Text
    , stop_reason :: Maybe StopReason
    , stop_sequence :: Maybe Text
    , usage :: Usage
    , container :: Maybe ContainerInfo
    } deriving stock (Generic, Show)

instance FromJSON MessageResponse where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON MessageResponse where
    toJSON = genericToJSON aesonOptions

-- | Output format specification for structured outputs
--
-- Use with the @structured-outputs-2025-11-13@ beta header.
--
-- Example:
--
-- @
-- let outputFormat = jsonSchemaFormat $ Aeson.object
--         [ "type" .= ("object" :: Text)
--         , "properties" .= Aeson.object
--             [ "name" .= Aeson.object ["type" .= ("string" :: Text)]
--             , "age" .= Aeson.object ["type" .= ("integer" :: Text)]
--             ]
--         , "required" .= (["name", "age"] :: [Text])
--         , "additionalProperties" .= False
--         ]
-- @
data OutputFormat = OutputFormat
    { type_ :: Text    -- ^ Currently only "json_schema" is supported
    , schema :: Value  -- ^ JSON Schema for the output
    } deriving stock (Eq, Generic, Show)

instance FromJSON OutputFormat where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON OutputFormat where
    toJSON = genericToJSON aesonOptions

-- | Create a JSON schema output format
--
-- This is the primary way to use structured outputs.
jsonSchemaFormat :: Value -> OutputFormat
jsonSchemaFormat s = OutputFormat
    { type_ = "json_schema"
    , schema = s
    }

-- | Request body for @\/v1\/messages@
--
-- For programmatic tool calling (PTC), use the @container@ field to reuse
-- a code execution container from a previous response.
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
    , tools :: Maybe (Vector ToolDefinition)
    , tool_choice :: Maybe ToolChoice
    , container :: Maybe Text
    , output_format :: Maybe OutputFormat
    -- ^ Structured output format (requires @structured-outputs-2025-11-13@ beta header)
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
    , container = Nothing
    , output_format = Nothing
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

-- | Request body for @\/v1\/messages\/count_tokens@
--
-- Note: This differs from CreateMessage - it doesn't include max_tokens
-- and other generation parameters.
data CountTokensRequest = CountTokensRequest
    { model :: Text
    , messages :: Vector Message
    , system :: Maybe Text
    , tools :: Maybe (Vector ToolDefinition)
    , tool_choice :: Maybe ToolChoice
    } deriving stock (Generic, Show)

instance FromJSON CountTokensRequest where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON CountTokensRequest where
    toJSON = genericToJSON aesonOptions

-- | Default CountTokensRequest
_CountTokensRequest :: CountTokensRequest
_CountTokensRequest = CountTokensRequest
    { model = ""
    , messages = mempty
    , system = Nothing
    , tools = Nothing
    , tool_choice = Nothing
    }

-- | Response from the token counting endpoint
data TokenCount = TokenCount
    { input_tokens :: Natural
    } deriving stock (Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Servant API for @\/v1\/messages@
type MessagesAPI =
        ReqBody '[JSON] CreateMessage
    :>  Post '[JSON] MessageResponse

-- | Servant API for @\/v1\/messages\/count_tokens@
type CountTokensAPI =
        "count_tokens"
    :>  ReqBody '[JSON] CountTokensRequest
    :>  Post '[JSON] TokenCount

-- | Combined Servant API
type API =
        "messages"
    :>  (MessagesAPI :<|> CountTokensAPI)
