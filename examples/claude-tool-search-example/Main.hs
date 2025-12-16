{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Example demonstrating tool search with Claude
--
-- This example shows how to use the server-side tool search feature,
-- which allows Claude to efficiently search through large numbers of tools.
--
-- Tool search requires the beta header: anthropic-beta: advanced-tool-use-2025-11-20
module Main where

import Data.Foldable (toList)
import Data.Text (Text)

import qualified Claude.V1 as V1
import qualified Claude.V1.Messages as Messages
import qualified Claude.V1.Tool as Tool
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified System.Environment as Environment

-- | Define many tools to demonstrate tool search
-- In a real application, you might have hundreds of tools

weatherTool :: Tool.Tool
weatherTool = Tool.Tool
    { Tool.name = "get_weather"
    , Tool.description = Just "Get the current weather for a location"
    , Tool.input_schema = Tool.InputSchema
        { Tool.type_ = "object"
        , Tool.properties = Just $ Aeson.object
            [ "location" Aeson..= Aeson.object
                [ "type" Aeson..= ("string" :: Text)
                , "description" Aeson..= ("City and state, e.g. San Francisco, CA" :: Text)
                ]
            ]
        , Tool.required = Just ["location"]
        }
    }

stockPriceTool :: Tool.Tool
stockPriceTool = Tool.Tool
    { Tool.name = "get_stock_price"
    , Tool.description = Just "Get the current stock price for a ticker symbol"
    , Tool.input_schema = Tool.InputSchema
        { Tool.type_ = "object"
        , Tool.properties = Just $ Aeson.object
            [ "ticker" Aeson..= Aeson.object
                [ "type" Aeson..= ("string" :: Text)
                , "description" Aeson..= ("Stock ticker symbol, e.g. AAPL" :: Text)
                ]
            ]
        , Tool.required = Just ["ticker"]
        }
    }

currencyConvertTool :: Tool.Tool
currencyConvertTool = Tool.Tool
    { Tool.name = "convert_currency"
    , Tool.description = Just "Convert an amount from one currency to another"
    , Tool.input_schema = Tool.InputSchema
        { Tool.type_ = "object"
        , Tool.properties = Just $ Aeson.object
            [ "amount" Aeson..= Aeson.object
                [ "type" Aeson..= ("number" :: Text)
                , "description" Aeson..= ("Amount to convert" :: Text)
                ]
            , "from_currency" Aeson..= Aeson.object
                [ "type" Aeson..= ("string" :: Text)
                , "description" Aeson..= ("Source currency code, e.g. USD" :: Text)
                ]
            , "to_currency" Aeson..= Aeson.object
                [ "type" Aeson..= ("string" :: Text)
                , "description" Aeson..= ("Target currency code, e.g. EUR" :: Text)
                ]
            ]
        , Tool.required = Just ["amount", "from_currency", "to_currency"]
        }
    }

calculatorTool :: Tool.Tool
calculatorTool = Tool.Tool
    { Tool.name = "calculator"
    , Tool.description = Just "Perform basic arithmetic calculations"
    , Tool.input_schema = Tool.InputSchema
        { Tool.type_ = "object"
        , Tool.properties = Just $ Aeson.object
            [ "expression" Aeson..= Aeson.object
                [ "type" Aeson..= ("string" :: Text)
                , "description" Aeson..= ("Math expression to evaluate, e.g. 2+2" :: Text)
                ]
            ]
        , Tool.required = Just ["expression"]
        }
    }

searchWebTool :: Tool.Tool
searchWebTool = Tool.Tool
    { Tool.name = "search_web"
    , Tool.description = Just "Search the web for information"
    , Tool.input_schema = Tool.InputSchema
        { Tool.type_ = "object"
        , Tool.properties = Just $ Aeson.object
            [ "query" Aeson..= Aeson.object
                [ "type" Aeson..= ("string" :: Text)
                , "description" Aeson..= ("Search query" :: Text)
                ]
            ]
        , Tool.required = Just ["query"]
        }
    }

main :: IO ()
main = do
    key <- Text.pack <$> Environment.getEnv "ANTHROPIC_KEY"
    env <- V1.getClientEnv "https://api.anthropic.com"

    -- Use makeMethodsWith to include the beta header for tool search
    let options = V1.defaultClientOptions
            { V1.apiKey = key
            , V1.anthropicBeta = Just "advanced-tool-use-2025-11-20"
            }
    let V1.Methods{ V1.createMessage } = V1.makeMethodsWith env options

    -- Set up tools with tool search enabled
    -- The tool search tool is NOT deferred, while other tools are deferred
    let tools =
            [ Tool.toolSearchRegex  -- Enable regex-based tool search
            , Tool.deferredTool weatherTool
            , Tool.deferredTool stockPriceTool
            , Tool.deferredTool currencyConvertTool
            , Tool.deferredTool calculatorTool
            , Tool.deferredTool searchWebTool
            ]

    let message = Messages.Message
            { Messages.role = Messages.User
            , Messages.content =
                [ Messages.textContent "What's the weather like in Tokyo?"
                ]
            , Messages.cache_control = Nothing
            }

    Text.IO.putStrLn "Sending request with tool search enabled..."
    Text.IO.putStrLn $ "Using " <> Text.pack (show (length tools)) <> " tools (with deferred loading)"

    response <- createMessage Messages._CreateMessage
        { Messages.model = "claude-sonnet-4-5-20250929"
        , Messages.messages = [message]
        , Messages.max_tokens = 1024
        , Messages.tools = Just tools
        }

    let Messages.MessageResponse
            { Messages.stop_reason = stopReason
            , Messages.content = responseContent
            , Messages.usage = Messages.Usage
                { Messages.input_tokens = inputToks
                , Messages.output_tokens = outputToks
                , Messages.server_tool_use = serverToolUse
                }
            } = response

    Text.IO.putStrLn $ "\nStop reason: " <> Text.pack (show stopReason)

    -- Print usage info including tool search requests
    Text.IO.putStrLn $ "Input tokens: " <> Text.pack (show inputToks)
    Text.IO.putStrLn $ "Output tokens: " <> Text.pack (show outputToks)
    case serverToolUse of
        Just Messages.ServerToolUseUsage{ Messages.web_search_requests = webReqs, Messages.tool_search_requests = toolReqs } -> do
            case webReqs of
                Just n -> Text.IO.putStrLn $ "Web search requests: " <> Text.pack (show n)
                Nothing -> pure ()
            case toolReqs of
                Just n -> Text.IO.putStrLn $ "Tool search requests: " <> Text.pack (show n)
                Nothing -> pure ()
        Nothing -> pure ()

    Text.IO.putStrLn "\nResponse content:"
    mapM_ printContent (toList responseContent)

-- | Print a content block
printContent :: Messages.ContentBlock -> IO ()
printContent (Messages.ContentBlock_Text{ Messages.text = t }) =
    Text.IO.putStrLn $ "  [text] " <> t
printContent (Messages.ContentBlock_Tool_Use{ Messages.name = n, Messages.id = tid }) =
    Text.IO.putStrLn $ "  [tool_use] " <> n <> " (id: " <> tid <> ")"
printContent (Messages.ContentBlock_Server_Tool_Use{ Messages.name = n, Messages.id = tid }) =
    Text.IO.putStrLn $ "  [server_tool_use] " <> n <> " (id: " <> tid <> ")"
printContent (Messages.ContentBlock_Tool_Search_Tool_Result{ Messages.tool_use_id = tid, Messages.content = resultContent }) = do
    Text.IO.putStrLn $ "  [tool_search_result] for tool_use_id: " <> tid
    case resultContent of
        Messages.ToolSearchResult{ Messages.tool_references = refs } -> do
            Text.IO.putStrLn $ "    Found " <> Text.pack (show (length refs)) <> " matching tools:"
            mapM_ (\Messages.ToolReference{ Messages.tool_name = tn } ->
                Text.IO.putStrLn $ "      - " <> tn) (toList refs)
        Messages.ToolSearchError{ Messages.error_code = code } ->
            Text.IO.putStrLn $ "    Error: " <> code
        Messages.ToolSearchResultContent_Unknown _ ->
            Text.IO.putStrLn "    Unknown result type"
printContent (Messages.ContentBlock_Unknown{ Messages.type_ = t }) =
    Text.IO.putStrLn $ "  [unknown] type: " <> t
