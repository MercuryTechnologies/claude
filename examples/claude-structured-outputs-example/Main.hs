{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Example demonstrating structured outputs with Claude
--
-- This example shows how to use:
-- 1. JSON outputs (output_format) to get structured JSON responses
-- 2. Strict tool use (strict: true) to validate tool parameters
--
-- Requires the structured-outputs-2025-11-13 beta header.
module Main where

import Data.Aeson (FromJSON(..), (.:), (.=))
import Data.Foldable (toList)
import Data.Text (Text)

import qualified Claude.V1 as V1
import qualified Claude.V1.Messages as Messages
import qualified Claude.V1.Tool as Tool
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.IO as Text.IO
import qualified System.Environment as Environment

-- | Structured output type for contact extraction
data ContactInfo = ContactInfo
    { name :: Text
    , email :: Text
    , company :: Maybe Text
    , interested_in :: Text
    } deriving Show

instance FromJSON ContactInfo where
    parseJSON = Aeson.withObject "ContactInfo" $ \obj -> do
        n <- obj .: "name"
        e <- obj .: "email"
        c <- obj .: "company"
        i <- obj .: "interested_in"
        pure ContactInfo{ name = n, email = e, company = c, interested_in = i }

main :: IO ()
main = do
    key <- Text.pack <$> Environment.getEnv "ANTHROPIC_KEY"
    env <- V1.getClientEnv "https://api.anthropic.com"

    -- Use makeMethodsWith to pass the structured outputs beta header
    let options = V1.defaultClientOptions
            { V1.apiKey = key
            , V1.anthropicBeta = Just "structured-outputs-2025-11-13"
            }

    let V1.Methods{ V1.createMessage } = V1.makeMethodsWith env options

    -- Example 1: JSON outputs for data extraction
    Text.IO.putStrLn "=== Example 1: JSON Outputs (Data Extraction) ===\n"
    jsonOutputsExample createMessage

    Text.IO.putStrLn "\n"

    -- Example 2: Strict tool use for validated parameters
    Text.IO.putStrLn "=== Example 2: Strict Tool Use ===\n"
    strictToolUseExample createMessage

-- | Example using output_format to get structured JSON responses
jsonOutputsExample :: (Messages.CreateMessage -> IO Messages.MessageResponse) -> IO ()
jsonOutputsExample createMessage = do
    let emailText = "Hi there! I'm John Smith from Acme Corp (john.smith@acme.com). \
                    \I saw your demo at the conference and I'm very interested in \
                    \your Enterprise plan. Could we schedule a call next week?"

    -- Define the JSON schema for our expected output
    let outputSchema = Aeson.object
            [ "type" .= ("object" :: Text)
            , "properties" .= Aeson.object
                [ "name" .= Aeson.object
                    [ "type" .= ("string" :: Text)
                    , "description" .= ("Full name of the contact" :: Text)
                    ]
                , "email" .= Aeson.object
                    [ "type" .= ("string" :: Text)
                    , "description" .= ("Email address" :: Text)
                    ]
                , "company" .= Aeson.object
                    [ "type" .= ("string" :: Text)
                    , "description" .= ("Company name if mentioned" :: Text)
                    ]
                , "interested_in" .= Aeson.object
                    [ "type" .= ("string" :: Text)
                    , "description" .= ("What product/service they're interested in" :: Text)
                    ]
                ]
            , "required" .= (["name", "email", "interested_in"] :: [Text])
            , "additionalProperties" .= False
            ]

    let message = Messages.Message
            { Messages.role = Messages.User
            , Messages.content = [Messages.textContent $ "Extract contact information from this email:\n\n" <> emailText]
            , Messages.cache_control = Nothing
            }

    Text.IO.putStrLn $ "Input email: " <> emailText
    Text.IO.putStrLn "\nExtracting structured data..."

    response <- createMessage Messages._CreateMessage
        { Messages.model = "claude-sonnet-4-5-20250929"
        , Messages.messages = [message]
        , Messages.max_tokens = 1024
        , Messages.output_format = Just (Messages.jsonSchemaFormat outputSchema)
        }

    let Messages.MessageResponse{ Messages.content = responseContent, Messages.stop_reason = stopReason } = response

    Text.IO.putStrLn $ "Stop reason: " <> Text.pack (show stopReason)

    -- The response text should be valid JSON matching our schema
    case toList responseContent of
        [Messages.ContentBlock_Text{ Messages.text = jsonText }] -> do
            Text.IO.putStrLn $ "\nRaw JSON response:\n" <> jsonText

            -- Parse and display the structured data
            case Aeson.eitherDecodeStrict (Text.Encoding.encodeUtf8 jsonText) of
                Left err -> Text.IO.putStrLn $ "Parse error: " <> Text.pack err
                Right ContactInfo{ name, email, company, interested_in } -> do
                    Text.IO.putStrLn "\nParsed contact info:"
                    Text.IO.putStrLn $ "  Name: " <> name
                    Text.IO.putStrLn $ "  Email: " <> email
                    Text.IO.putStrLn $ "  Company: " <> maybe "(not specified)" id company
                    Text.IO.putStrLn $ "  Interested in: " <> interested_in
        _ -> Text.IO.putStrLn "Unexpected response format"

-- | Example using strict: true for validated tool parameters
strictToolUseExample :: (Messages.CreateMessage -> IO Messages.MessageResponse) -> IO ()
strictToolUseExample createMessage = do
    -- Define a tool with strict mode enabled
    -- This guarantees the tool input will match the schema exactly
    let flightSearchTool = Tool.strictFunctionTool
            "search_flights"
            (Just "Search for available flights between two cities")
            $ Aeson.object
                [ "type" .= ("object" :: Text)
                , "properties" .= Aeson.object
                    [ "origin" .= Aeson.object
                        [ "type" .= ("string" :: Text)
                        , "description" .= ("Origin airport code (e.g., SFO)" :: Text)
                        ]
                    , "destination" .= Aeson.object
                        [ "type" .= ("string" :: Text)
                        , "description" .= ("Destination airport code (e.g., JFK)" :: Text)
                        ]
                    , "date" .= Aeson.object
                        [ "type" .= ("string" :: Text)
                        , "format" .= ("date" :: Text)
                        , "description" .= ("Travel date in YYYY-MM-DD format" :: Text)
                        ]
                    , "passengers" .= Aeson.object
                        [ "type" .= ("integer" :: Text)
                        , "enum" .= ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10] :: [Int])
                        , "description" .= ("Number of passengers (1-10)" :: Text)
                        ]
                    ]
                , "required" .= (["origin", "destination", "date", "passengers"] :: [Text])
                , "additionalProperties" .= False
                ]

    let message = Messages.Message
            { Messages.role = Messages.User
            , Messages.content = [Messages.textContent "Find me flights from San Francisco to New York for 2 people on January 15th, 2025"]
            , Messages.cache_control = Nothing
            }

    Text.IO.putStrLn "Asking Claude to search for flights with strict tool validation..."

    response <- createMessage Messages._CreateMessage
        { Messages.model = "claude-sonnet-4-5-20250929"
        , Messages.messages = [message]
        , Messages.max_tokens = 1024
        , Messages.tools = Just [Tool.inlineTool flightSearchTool]
        , Messages.tool_choice = Just Tool.toolChoiceAny  -- Force tool use
        }

    let Messages.MessageResponse{ Messages.content = responseContent, Messages.stop_reason = stopReason } = response

    Text.IO.putStrLn $ "Stop reason: " <> Text.pack (show stopReason)

    -- With strict mode, we're guaranteed the tool input matches the schema
    mapM_ processBlock (toList responseContent)
  where
    processBlock (Messages.ContentBlock_Tool_Use{ Messages.id = toolId, Messages.name = toolName, Messages.input = toolInput }) = do
        Text.IO.putStrLn $ "\nTool called: " <> toolName
        Text.IO.putStrLn $ "Tool ID: " <> toolId
        Text.IO.putStrLn $ "Input (guaranteed to match schema):"
        Text.IO.putStrLn $ "  " <> Text.Encoding.decodeUtf8 (LBS.toStrict (Aeson.encode toolInput))

        -- In strict mode, we can safely extract fields without error handling
        case toolInput of
            Aeson.Object obj -> do
                let origin = lookupString "origin" obj
                let destination = lookupString "destination" obj
                let date = lookupString "date" obj
                let passengers = lookupInt "passengers" obj
                Text.IO.putStrLn "\nParsed parameters:"
                Text.IO.putStrLn $ "  Origin: " <> origin
                Text.IO.putStrLn $ "  Destination: " <> destination
                Text.IO.putStrLn $ "  Date: " <> date
                Text.IO.putStrLn $ "  Passengers: " <> Text.pack (show passengers)
            _ -> pure ()

    processBlock (Messages.ContentBlock_Text{ Messages.text = t }) =
        Text.IO.putStrLn $ "Text: " <> t

    processBlock _ = pure ()

    lookupString key obj = case KeyMap.lookup key obj of
        Just (Aeson.String s) -> s
        _ -> "(missing)"

    lookupInt key obj = case KeyMap.lookup key obj of
        Just (Aeson.Number n) -> round n :: Int
        _ -> 0
