{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Example demonstrating Programmatic Tool Calling (PTC) with Claude
--
-- This example shows how to use code execution to have Claude programmatically
-- call tools. Instead of Claude making individual tool calls that you handle
-- one by one, Claude writes and executes code that calls multiple tools and
-- aggregates the results.
--
-- Requirements:
-- * anthropic-beta: advanced-tool-use-2025-11-20 header
-- * code_execution_20250825 tool in the tools array
-- * Tools must have allowed_callers = ["code_execution_20250825"]
module Main where

import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Prelude hiding (id)

import qualified Claude.V1 as V1
import qualified Claude.V1.Messages as Messages
import qualified Claude.V1.Tool as Tool
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector
import qualified System.Environment as Environment

-- | Define a query_database tool that Claude can call programmatically
queryDatabaseTool :: Tool.Tool
queryDatabaseTool = Tool.Tool
    { Tool.name = "query_database"
    , Tool.description = Just "Query a regional database for sales data. Returns sales figures for the specified region."
    , Tool.input_schema = Tool.InputSchema
        { Tool.type_ = "object"
        , Tool.properties = Just $ Aeson.object
            [ "region" Aeson..= Aeson.object
                [ "type" Aeson..= ("string" :: Text)
                , "description" Aeson..= ("The region to query: west, east, or central" :: Text)
                , "enum" Aeson..= (["west", "east", "central"] :: [Text])
                ]
            ]
        , Tool.required = Just ["region"]
        , Tool.additionalProperties = Nothing
        }
    , Tool.strict = Nothing
    }

-- | Fake database query - returns sales data for a region
queryDatabase :: Text -> Text
queryDatabase region = case region of
    "west"    -> "{\"region\": \"west\", \"sales\": 125000, \"units\": 450}"
    "east"    -> "{\"region\": \"east\", \"sales\": 198000, \"units\": 720}"
    "central" -> "{\"region\": \"central\", \"sales\": 87000, \"units\": 310}"
    _         -> "{\"error\": \"Unknown region\"}"

main :: IO ()
main = do
    key <- Text.pack <$> Environment.getEnv "ANTHROPIC_KEY"
    env <- V1.getClientEnv "https://api.anthropic.com"

    -- Create methods with beta header for PTC
    let options = V1.defaultClientOptions
            { V1.apiKey = key
            , V1.anthropicBeta = Just "advanced-tool-use-2025-11-20"
            }
    let V1.Methods{ V1.createMessage } = V1.makeMethodsWith env options

    -- Tools: code execution + query_database with allowed_callers
    let tools =
            [ Tool.codeExecutionTool
            , Tool.allowCallers Tool.allowedCallersCodeExecution (Tool.inlineTool queryDatabaseTool)
            ]

    -- Initial message asking for aggregated data from multiple regions
    let initialMessage = Messages.Message
            { Messages.role = Messages.User
            , Messages.content =
                [ Messages.textContent
                    "Query the database for all three regions (west, east, central) and calculate the total sales and units across all regions. Use code execution to make the queries and aggregate the results."
                ]
            , Messages.cache_control = Nothing
            }

    Text.IO.putStrLn "=== Programmatic Tool Calling Example ==="
    Text.IO.putStrLn "Sending request with code execution tool..."
    Text.IO.putStrLn ""

    -- Loop until we get a final response
    loop createMessage tools [initialMessage] Nothing 0

  where
    maxIterations :: Int
    maxIterations = 10

    loop _ _ _ _ iteration | iteration >= maxIterations = do
        Text.IO.putStrLn $ "Max iterations (" <> Text.pack (show maxIterations) <> ") reached"

    loop createMessage tools messages containerId iteration = do
        Text.IO.putStrLn $ "--- Turn " <> Text.pack (show (iteration + 1)) <> " ---"

        response <- createMessage Messages._CreateMessage
            { Messages.model = "claude-sonnet-4-5-20250929"
            , Messages.messages = Vector.fromList messages
            , Messages.max_tokens = 4096
            , Messages.tools = Just tools
            , Messages.container = containerId
            }

        let Messages.MessageResponse
                { Messages.stop_reason = stopReason
                , Messages.content = responseContent
                , Messages.container = containerInfo
                } = response

        -- Extract container ID for reuse
        let newContainerId = case containerInfo of
                Just Messages.ContainerInfo{ Messages.id = cid } -> Just cid
                Nothing -> containerId

        Text.IO.putStrLn $ "Stop reason: " <> Text.pack (show stopReason)
        when (containerId /= newContainerId) $
            Text.IO.putStrLn $ "Container ID: " <> maybe "none" (\x -> x) newContainerId

        -- Print response content
        mapM_ printContentBlock (toList responseContent)

        case stopReason of
            Just Messages.End_Turn -> do
                Text.IO.putStrLn ""
                Text.IO.putStrLn "=== Final Response ==="
                mapM_ printFinalContent (toList responseContent)

            Just Messages.Tool_Use -> do
                -- Find all tool_use blocks and process them
                let toolUseBlocks =
                        [ (toolId, toolName, toolInput, toolCaller)
                        | Messages.ContentBlock_Tool_Use
                            { Messages.id = toolId
                            , Messages.name = toolName
                            , Messages.input = toolInput
                            , Messages.caller = toolCaller
                            } <- toList responseContent
                        ]

                Text.IO.putStrLn ""
                Text.IO.putStrLn $ "Processing " <> Text.pack (show (length toolUseBlocks)) <> " tool call(s)..."

                -- Process each tool call
                toolResults <- mapM processToolCall toolUseBlocks

                -- Build assistant replay message
                let assistantContent = mapMaybe Messages.contentBlockToContent (toList responseContent)
                let assistantMessage = Messages.Message
                        { Messages.role = Messages.Assistant
                        , Messages.content = Vector.fromList assistantContent
                        , Messages.cache_control = Nothing
                        }

                -- Build user message with tool results only (required for PTC)
                let userMessage = Messages.Message
                        { Messages.role = Messages.User
                        , Messages.content = Vector.fromList toolResults
                        , Messages.cache_control = Nothing
                        }

                Text.IO.putStrLn ""
                loop createMessage tools
                    (messages <> [assistantMessage, userMessage])
                    newContainerId
                    (iteration + 1)

            _ -> do
                Text.IO.putStrLn $ "Unexpected stop reason: " <> Text.pack (show stopReason)

    when :: Bool -> IO () -> IO ()
    when True action = action
    when False _ = pure ()

-- | Process a single tool call
processToolCall :: (Text, Text, Aeson.Value, Maybe Messages.ToolCaller) -> IO Messages.Content
processToolCall (toolId, toolName, toolInput, caller) = do
    -- Show caller info
    case caller of
        Just Messages.ToolCaller_Direct ->
            Text.IO.putStrLn $ "  [Direct call] " <> toolName
        Just (Messages.ToolCaller_CodeExecution{ Messages.tool_id = tid }) ->
            Text.IO.putStrLn $ "  [Code execution call via " <> tid <> "] " <> toolName
        Just (Messages.ToolCaller_Unknown _) ->
            Text.IO.putStrLn $ "  [Unknown caller] " <> toolName
        Nothing ->
            Text.IO.putStrLn $ "  [No caller info] " <> toolName

    case toolName of
        "query_database" -> do
            let parseRegion = Aeson.withObject "input" (\o -> o Aeson..: "region")
            case Aeson.Types.parseMaybe parseRegion toolInput of
                Just region -> do
                    let result = queryDatabase region
                    Text.IO.putStrLn $ "    Region: " <> region <> " -> " <> result
                    pure Messages.Content_Tool_Result
                        { Messages.tool_use_id = toolId
                        , Messages.content = Just result
                        , Messages.is_error = Nothing
                        }
                Nothing -> do
                    Text.IO.putStrLn "    Error: missing region parameter"
                    pure Messages.Content_Tool_Result
                        { Messages.tool_use_id = toolId
                        , Messages.content = Just "Error: missing region parameter"
                        , Messages.is_error = Just True
                        }
        _ -> do
            Text.IO.putStrLn $ "    Unknown tool: " <> toolName
            pure Messages.Content_Tool_Result
                { Messages.tool_use_id = toolId
                , Messages.content = Just $ "Unknown tool: " <> toolName
                , Messages.is_error = Just True
                }

-- | Print a content block summary
printContentBlock :: Messages.ContentBlock -> IO ()
printContentBlock (Messages.ContentBlock_Text{ Messages.text = t }) =
    Text.IO.putStrLn $ "  [Text] " <> Text.take 80 t <> if Text.length t > 80 then "..." else ""
printContentBlock (Messages.ContentBlock_Tool_Use{ Messages.name = n, Messages.caller = c }) =
    Text.IO.putStrLn $ "  [Tool use] " <> n <> callerInfo c
  where
    callerInfo Nothing = ""
    callerInfo (Just Messages.ToolCaller_Direct) = " (direct)"
    callerInfo (Just Messages.ToolCaller_CodeExecution{}) = " (programmatic)"
    callerInfo (Just Messages.ToolCaller_Unknown{}) = " (unknown caller)"
printContentBlock (Messages.ContentBlock_Server_Tool_Use{ Messages.name = n }) =
    Text.IO.putStrLn $ "  [Server tool use] " <> n
printContentBlock (Messages.ContentBlock_Tool_Search_Tool_Result{ Messages.tool_use_id = tid }) =
    Text.IO.putStrLn $ "  [Tool search result] " <> tid
printContentBlock (Messages.ContentBlock_Code_Execution_Tool_Result{ Messages.tool_use_id = tid, Messages.code_execution_content = c }) = do
    Text.IO.putStrLn $ "  [Code execution result] " <> tid
    case c of
        Messages.CodeExecutionResultContent result -> do
            let Messages.CodeExecutionResult{ Messages.stdout = out, Messages.stderr = err, Messages.return_code = rc } = result
            Text.IO.putStrLn $ "    return_code: " <> Text.pack (show rc)
            unless (Text.null out) $
                Text.IO.putStrLn $ "    stdout: " <> Text.take 200 out <> if Text.length out > 200 then "..." else ""
            unless (Text.null err) $
                Text.IO.putStrLn $ "    stderr: " <> Text.take 200 err <> if Text.length err > 200 then "..." else ""
        Messages.CodeExecutionToolResultContent_Unknown _ ->
            Text.IO.putStrLn "    (unknown content type)"
  where
    unless False action = action
    unless True _ = pure ()
printContentBlock (Messages.ContentBlock_Unknown{ Messages.type_ = t }) =
    Text.IO.putStrLn $ "  [Unknown] " <> t

-- | Print final text content
printFinalContent :: Messages.ContentBlock -> IO ()
printFinalContent (Messages.ContentBlock_Text{ Messages.text = t }) = Text.IO.putStrLn t
printFinalContent _ = pure ()
