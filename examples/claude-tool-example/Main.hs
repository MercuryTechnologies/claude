{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Example demonstrating tool use with Claude
--
-- This example defines a simple "get_weather" tool and shows how to:
-- 1. Send a request with tools
-- 2. Handle tool_use responses
-- 3. Send tool results back to Claude
module Main where

import Data.Aeson (FromJSON(..), Value, withObject, (.:), (.:?), (.=))
import Data.Foldable (toList)
import Data.Text (Text)

import qualified Claude.V1 as V1
import qualified Claude.V1.Messages as Messages
import qualified Claude.V1.Tool as Tool
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector
import qualified System.Environment as Environment

-- | Arguments for our weather tool
data WeatherArgs = WeatherArgs
    { location :: Text
    , unit :: Maybe Text
    } deriving Show

instance FromJSON WeatherArgs where
    parseJSON = withObject "WeatherArgs" $ \obj -> do
        loc <- obj .: "location"
        u <- obj .:? "unit"
        pure WeatherArgs{ location = loc, unit = u }

-- | Fake weather function - in real use, this would call a weather API
getWeather :: Text -> Maybe Text -> Text
getWeather loc u =
    "The weather in " <> loc <> " is 72°" <> tempUnit <> " and sunny."
  where
    tempUnit = case u of
        Just "celsius" -> "C"
        _ -> "F"

-- | Define our weather tool
weatherTool :: Tool.Tool
weatherTool = Tool.Tool
    { Tool.name = "get_weather"
    , Tool.description = Just "Get the current weather for a location"
    , Tool.input_schema = Tool.InputSchema
        { Tool.type_ = "object"
        , Tool.properties = Just $ Aeson.object
            [ "location" .= Aeson.object
                [ "type" .= ("string" :: Text)
                , "description" .= ("City and state, e.g. San Francisco, CA" :: Text)
                ]
            , "unit" .= Aeson.object
                [ "type" .= ("string" :: Text)
                , "enum" .= (["celsius", "fahrenheit"] :: [Text])
                , "description" .= ("Temperature unit" :: Text)
                ]
            ]
        , Tool.required = Just ["location"]
        }
    }

main :: IO ()
main = do
    key <- Text.pack <$> Environment.getEnv "ANTHROPIC_KEY"
    env <- V1.getClientEnv "https://api.anthropic.com"

    let V1.Methods{ V1.createMessage } = V1.makeMethods env key (Just "2023-06-01")

    -- Initial request asking about weather
    let initialMessage = Messages.Message
            { Messages.role = Messages.User
            , Messages.content = [Messages.Content_Text{ Messages.text = "What's the weather like in San Francisco?" }]
            }

    Text.IO.putStrLn "Sending initial request with weather tool..."

    firstResponse <- createMessage Messages._CreateMessage
        { Messages.model = "claude-sonnet-4-5-20250929"
        , Messages.messages = [initialMessage]
        , Messages.max_tokens = 1024
        , Messages.tools = Just [weatherTool]
        }

    let Messages.MessageResponse{ Messages.stop_reason = stopReason, Messages.content = responseContent } = firstResponse

    Text.IO.putStrLn $ "Stop reason: " <> Text.pack (show stopReason)

    -- Check if Claude wants to use a tool
    case stopReason of
        Just Messages.Tool_Use -> do
            Text.IO.putStrLn "Claude wants to use a tool!"

            -- Find the tool_use block(s) in the response
            let toolUseBlocks = [ (toolId, toolName, toolInput)
                    | Messages.ContentBlock_Tool_Use{ Messages.id = toolId, Messages.name = toolName, Messages.input = toolInput }
                        <- toList responseContent
                    ]

            -- Process each tool call
            toolResults <- mapM processToolCall toolUseBlocks

            Text.IO.putStrLn "Sending tool results back to Claude..."

            -- Send the tool results back
            let assistantMessage = Messages.Message
                    { Messages.role = Messages.Assistant
                    , Messages.content = Vector.fromList
                        [ Messages.Content_Tool_Use{ Messages.id = tid, Messages.name = tname, Messages.input = tinput }
                        | (tid, tname, tinput) <- toolUseBlocks
                        ]
                    }

            let userToolResults = Messages.Message
                    { Messages.role = Messages.User
                    , Messages.content = Vector.fromList toolResults
                    }

            finalResponse <- createMessage Messages._CreateMessage
                { Messages.model = "claude-sonnet-4-5-20250929"
                , Messages.messages = [initialMessage, assistantMessage, userToolResults]
                , Messages.max_tokens = 1024
                , Messages.tools = Just [weatherTool]
                }

            -- Print the final response
            let Messages.MessageResponse{ Messages.content = finalContent } = finalResponse
            Text.IO.putStrLn "\nClaude's final response:"
            mapM_ printContent (toList finalContent)

        _ -> do
            -- No tool use, just print the response
            Text.IO.putStrLn "Claude responded directly:"
            mapM_ printContent (toList responseContent)

-- | Process a single tool call and return the result content
processToolCall :: (Text, Text, Value) -> IO Messages.Content
processToolCall (toolId, toolName, toolInput) = do
    Text.IO.putStrLn $ "Processing tool call: " <> toolName
    Text.IO.putStrLn $ "Input: " <> Text.Encoding.decodeUtf8 (LBS.toStrict (Aeson.encode toolInput))

    case toolName of
        "get_weather" -> do
            case Aeson.fromJSON toolInput of
                Aeson.Error err -> do
                    Text.IO.putStrLn $ "Error parsing args: " <> Text.pack err
                    pure Messages.Content_Tool_Result
                        { Messages.tool_use_id = toolId
                        , Messages.content = Just $ "Error: " <> Text.pack err
                        , Messages.is_error = Just True
                        }
                Aeson.Success WeatherArgs{ location = loc, unit = u } -> do
                    let result = getWeather loc u
                    Text.IO.putStrLn $ "Weather result: " <> result
                    pure Messages.Content_Tool_Result
                        { Messages.tool_use_id = toolId
                        , Messages.content = Just result
                        , Messages.is_error = Nothing
                        }
        _ -> do
            Text.IO.putStrLn $ "Unknown tool: " <> toolName
            pure Messages.Content_Tool_Result
                { Messages.tool_use_id = toolId
                , Messages.content = Just $ "Unknown tool: " <> toolName
                , Messages.is_error = Just True
                }

-- | Print a content block
printContent :: Messages.ContentBlock -> IO ()
printContent (Messages.ContentBlock_Text{ Messages.text = t }) = Text.IO.putStrLn t
printContent (Messages.ContentBlock_Tool_Use{ Messages.name = n }) =
    Text.IO.putStrLn $ "[Tool use: " <> n <> "]"
