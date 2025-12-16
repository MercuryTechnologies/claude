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
      -- * Tool definition (heterogeneous tools array)
    , ToolDefinition(..)
    , ToolSearchTool(..)
    , ToolSearchToolType(..)
      -- * Tool constructors
    , functionTool
    , simpleInputSchema
      -- * ToolDefinition constructors
    , inlineTool
    , deferredTool
    , toolSearchRegex
    , toolSearchBm25
      -- * Code execution tool (PTC)
    , codeExecutionTool
    , allowedCallersCodeExecution
    , allowCallers
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
    } deriving stock (Eq, Generic, Show)

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
    } deriving stock (Eq, Generic, Show)

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

-- | Tool search tool type variants (for server-side tool search)
data ToolSearchToolType
    = ToolSearchTool_Regex_20251119
    | ToolSearchTool_Bm25_20251119
    deriving stock (Eq, Show)

instance FromJSON ToolSearchToolType where
    parseJSON = Aeson.withText "ToolSearchToolType" $ \t -> case t of
        "tool_search_tool_regex_20251119" -> pure ToolSearchTool_Regex_20251119
        "tool_search_tool_bm25_20251119" -> pure ToolSearchTool_Bm25_20251119
        _ -> fail $ "Unknown tool search tool type: " <> show t

instance ToJSON ToolSearchToolType where
    toJSON ToolSearchTool_Regex_20251119 = Aeson.String "tool_search_tool_regex_20251119"
    toJSON ToolSearchTool_Bm25_20251119 = Aeson.String "tool_search_tool_bm25_20251119"

-- | Tool search tool configuration
--
-- Used to enable server-side tool search, which allows Claude to efficiently
-- search through large numbers of tools using regex or BM25 matching.
data ToolSearchTool = ToolSearchTool
    { name :: Text
    , type_ :: ToolSearchToolType
    } deriving stock (Eq, Show)

instance FromJSON ToolSearchTool where
    parseJSON = Aeson.withObject "ToolSearchTool" $ \o -> do
        name <- o Aeson..: "name"
        type_ <- o Aeson..: "type"
        pure ToolSearchTool{ name, type_ }

instance ToJSON ToolSearchTool where
    toJSON ToolSearchTool{ name, type_ } = Aeson.object
        [ "name" Aeson..= name
        , "type" Aeson..= type_
        ]

-- | A tool definition for the @tools@ array
--
-- The @tools@ array in Claude API requests is heterogeneous:
--
-- * Function tools: regular tools with name, description, and input schema
-- * Tool search tools: server-side tool search configuration
-- * Code execution tool: for programmatic tool calling (PTC)
--
-- Use 'inlineTool' or 'deferredTool' to wrap a 'Tool', or 'toolSearchRegex'/'toolSearchBm25'
-- to add tool search capability. Use 'codeExecutionTool' for PTC.
data ToolDefinition
    = ToolDef_Function
        { tool :: Tool
        , defer_loading :: Maybe Bool
        , allowed_callers :: Maybe (Vector Text)
        }
    | ToolDef_SearchTool ToolSearchTool
    | ToolDef_CodeExecutionTool
        { name :: Text
        , type_ :: Text
        }
    deriving stock (Eq, Show)

instance FromJSON ToolDefinition where
    parseJSON = Aeson.withObject "ToolDefinition" $ \o -> do
        -- Check if this is a tool search tool or code execution tool by looking for "type" field
        mType <- o Aeson..:? "type"
        case mType of
            Just t | isToolSearchType t -> do
                searchTool <- Aeson.parseJSON (Aeson.Object o)
                pure (ToolDef_SearchTool searchTool)
            Just t | isCodeExecutionType t -> do
                name <- o Aeson..: "name"
                pure ToolDef_CodeExecutionTool{ name, type_ = t }
            _ -> do
                -- Parse as function tool
                tool <- Aeson.parseJSON (Aeson.Object o)
                defer_loading <- o Aeson..:? "defer_loading"
                allowed_callers <- o Aeson..:? "allowed_callers"
                pure ToolDef_Function{ tool, defer_loading, allowed_callers }
      where
        isToolSearchType :: Text -> Bool
        isToolSearchType t = t == "tool_search_tool_regex_20251119"
                          || t == "tool_search_tool_bm25_20251119"
        isCodeExecutionType :: Text -> Bool
        isCodeExecutionType t = t == "code_execution_20250825"

instance ToJSON ToolDefinition where
    toJSON (ToolDef_Function Tool{ name, description, input_schema } defer_loading allowed_callers) =
        Aeson.Object (baseMap <> optionalFields)
      where
        baseObj = Aeson.object $
            [ "name" Aeson..= name
            , "input_schema" Aeson..= input_schema
            ] <> maybe [] (\d -> ["description" Aeson..= d]) description
        baseMap = case baseObj of
            Aeson.Object m -> m
            _ -> KeyMap.empty
        optionalFields = KeyMap.fromList $
            maybe [] (\dl -> [("defer_loading", Aeson.toJSON dl)]) defer_loading <>
            maybe [] (\ac -> [("allowed_callers", Aeson.toJSON ac)]) allowed_callers
    toJSON (ToolDef_SearchTool searchTool) = Aeson.toJSON searchTool
    toJSON (ToolDef_CodeExecutionTool name type_) = Aeson.object
        [ "name" Aeson..= name
        , "type" Aeson..= type_
        ]

-- | Wrap a tool for inline (non-deferred) loading
inlineTool :: Tool -> ToolDefinition
inlineTool t = ToolDef_Function{ tool = t, defer_loading = Nothing, allowed_callers = Nothing }

-- | Wrap a tool for deferred loading (used with tool search)
deferredTool :: Tool -> ToolDefinition
deferredTool t = ToolDef_Function{ tool = t, defer_loading = Just True, allowed_callers = Nothing }

-- | Code execution tool for programmatic tool calling (PTC)
--
-- Requires @anthropic-beta: advanced-tool-use-2025-11-20@ header.
-- When included in the tools array, Claude can write and execute code
-- to call other tools programmatically.
codeExecutionTool :: ToolDefinition
codeExecutionTool = ToolDef_CodeExecutionTool
    { name = "code_execution"
    , type_ = "code_execution_20250825"
    }

-- | Allowed callers for code execution (PTC)
--
-- Use with 'allowCallers' to mark a function tool as callable by code execution.
allowedCallersCodeExecution :: Vector Text
allowedCallersCodeExecution = ["code_execution_20250825"]

-- | Set allowed_callers on a function tool definition
--
-- Only affects 'ToolDef_Function'; other tool types are returned unchanged.
--
-- Example:
--
-- @
-- allowCallers allowedCallersCodeExecution (inlineTool myTool)
-- @
allowCallers :: Vector Text -> ToolDefinition -> ToolDefinition
allowCallers callers (ToolDef_Function t dl _) = ToolDef_Function t dl (Just callers)
allowCallers _ td = td

-- | Tool search using regex matching
--
-- Requires @anthropic-beta: advanced-tool-use-2025-11-20@ header.
toolSearchRegex :: ToolDefinition
toolSearchRegex = ToolDef_SearchTool ToolSearchTool
    { name = "tool_search_tool_regex"
    , type_ = ToolSearchTool_Regex_20251119
    }

-- | Tool search using BM25 matching
--
-- Requires @anthropic-beta: advanced-tool-use-2025-11-20@ header.
toolSearchBm25 :: ToolDefinition
toolSearchBm25 = ToolDef_SearchTool ToolSearchTool
    { name = "tool_search_tool_bm25"
    , type_ = ToolSearchTool_Bm25_20251119
    }

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
