-- | Tool types for Claude API
--
-- This module provides types and utilities for defining tools that Claude can use.
--
-- Example usage:
--
-- @
-- import Claude.V1.Tool
--
-- -- Define a simple tool
-- weatherTool :: Tool
-- weatherTool = functionTool \"get_weather\"
--     (Just \"Get the current weather for a location\")
--     (Aeson.object
--         [ \"type\" .= (\"object\" :: Text)
--         , \"properties\" .= Aeson.object
--             [ \"location\" .= Aeson.object
--                 [ \"type\" .= (\"string\" :: Text)
--                 , \"description\" .= (\"City and state, e.g. San Francisco, CA\" :: Text)
--                 ]
--             ]
--         , \"required\" .= ([\"location\"] :: [Text])
--         ])
-- @
module Claude.V1.Tool
    ( -- * Types
      Tool(..)
    , ToolChoice(..)
    , InputSchema(..)
      -- * Tool constructors
    , functionTool
    , simpleInputSchema
      -- * ToolChoice constructors
    , toolChoiceAuto
    , toolChoiceAny
    , toolChoiceTool
      -- * Helpers for processing tool calls
    , isToolUse
    , getToolUseBlocks
    , makeToolResult
    , makeToolResultError
    ) where

import Claude.Prelude

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Vector as Vector

-- | Tool input schema (JSON Schema)
--
-- The schema follows JSON Schema format. At minimum, specify @type_@ as \"object\".
data InputSchema = InputSchema
    { type_ :: Text
    , properties :: Maybe Value
    , required :: Maybe (Vector Text)
    } deriving stock (Generic, Show)

instance FromJSON InputSchema where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON InputSchema where
    toJSON = genericToJSON aesonOptions

-- | Create a simple input schema with properties and required fields
simpleInputSchema
    :: Value           -- ^ Properties object
    -> Vector Text     -- ^ Required field names
    -> InputSchema
simpleInputSchema props reqs = InputSchema
    { type_ = "object"
    , properties = Just props
    , required = Just reqs
    }

-- | A tool that can be used by Claude
--
-- Tools allow Claude to call external functions. When Claude decides to use a tool,
-- it will return a @tool_use@ content block with the tool name and input arguments.
data Tool = Tool
    { name :: Text
    , description :: Maybe Text
    , input_schema :: InputSchema
    } deriving stock (Generic, Show)

instance FromJSON Tool where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Tool where
    toJSON = genericToJSON aesonOptions

-- | Create a function tool with a name, description, and JSON schema for parameters
--
-- This is the primary way to define tools for Claude.
functionTool
    :: Text           -- ^ Tool name (must match [a-zA-Z0-9_-]+)
    -> Maybe Text     -- ^ Description of what the tool does
    -> Value          -- ^ JSON Schema for the input parameters
    -> Tool
functionTool toolName toolDescription schema = Tool
    { name = toolName
    , description = toolDescription
    , input_schema = InputSchema
        { type_ = "object"
        , properties = case schema of
            Aeson.Object o -> case lookupKey "properties" o of
                Just props -> Just props
                Nothing -> Just schema
            _ -> Just schema
        , required = case schema of
            Aeson.Object o -> case lookupKey "required" o of
                Just (Aeson.Array arr) -> Just (Vector.mapMaybe getString arr)
                _ -> Nothing
            _ -> Nothing
        }
    }
  where
    getString (Aeson.String s) = Just s
    getString _ = Nothing
    lookupKey k obj = KeyMap.lookup (Key.fromText k) obj

-- | Controls which tool the model should use
data ToolChoice
    = ToolChoice_Auto
    -- ^ Let Claude decide whether to use tools
    | ToolChoice_Any
    -- ^ Force Claude to use one of the provided tools
    | ToolChoice_Tool { name :: Text }
    -- ^ Force Claude to use a specific tool
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

-- | Convenience: auto tool choice (let Claude decide)
toolChoiceAuto :: ToolChoice
toolChoiceAuto = ToolChoice_Auto

-- | Convenience: any tool choice (force tool use)
toolChoiceAny :: ToolChoice
toolChoiceAny = ToolChoice_Any

-- | Convenience: specific tool choice
toolChoiceTool :: Text -> ToolChoice
toolChoiceTool = ToolChoice_Tool

-- | Content block types (duplicated here for helper functions)
-- These mirror the types in Messages but are needed for the helper functions.

-- | Check if a content block is a tool use block
isToolUse :: Value -> Bool
isToolUse (Aeson.Object o) = case KeyMap.lookup (Key.fromText "type") o of
    Just (Aeson.String "tool_use") -> True
    _ -> False
isToolUse _ = False

-- | Extract tool use blocks from a response's content array
--
-- Returns a list of (id, name, input) tuples for each tool_use block
getToolUseBlocks :: Vector Value -> [(Text, Text, Value)]
getToolUseBlocks content = Vector.toList $ Vector.mapMaybe extractToolUse content
  where
    extractToolUse (Aeson.Object o) = do
        Aeson.String "tool_use" <- KeyMap.lookup (Key.fromText "type") o
        Aeson.String toolId <- KeyMap.lookup (Key.fromText "id") o
        Aeson.String toolName <- KeyMap.lookup (Key.fromText "name") o
        toolInput <- KeyMap.lookup (Key.fromText "input") o
        pure (toolId, toolName, toolInput)
    extractToolUse _ = Nothing

-- | Create a tool result content block for a successful tool call
makeToolResult
    :: Text    -- ^ tool_use_id from the tool_use block
    -> Text    -- ^ Result content (typically JSON encoded)
    -> Value
makeToolResult toolUseId resultContent = Aeson.object
    [ "type" Aeson..= ("tool_result" :: Text)
    , "tool_use_id" Aeson..= toolUseId
    , "content" Aeson..= resultContent
    ]

-- | Create a tool result content block for a failed tool call
makeToolResultError
    :: Text    -- ^ tool_use_id from the tool_use block
    -> Text    -- ^ Error message
    -> Value
makeToolResultError toolUseId errorMsg = Aeson.object
    [ "type" Aeson..= ("tool_result" :: Text)
    , "tool_use_id" Aeson..= toolUseId
    , "content" Aeson..= errorMsg
    , "is_error" Aeson..= True
    ]
