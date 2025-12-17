{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import Claude.V1 (Methods(..))
import Prelude hiding (id)

import qualified Claude.V1 as V1
import qualified Claude.V1.Messages as Messages
import qualified Claude.V1.Tool as Tool
import qualified Control.Concurrent as Concurrent
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import           Data.Foldable (toList)
import qualified Data.IORef as IORef
import           Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import qualified Network.HTTP.Client as HTTP.Client
import qualified Network.HTTP.Client.TLS as TLS
import qualified Servant.Client as Client
import qualified System.Environment as Environment
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

main :: IO ()
main = do
    let managerSettings =
            TLS.tlsManagerSettings
                { HTTP.Client.managerResponseTimeout =
                    HTTP.Client.responseTimeoutNone
                }

    manager <- TLS.newTlsManagerWith managerSettings

    baseUrl <- Client.parseBaseUrl "https://api.anthropic.com"

    let clientEnv = Client.mkClientEnv manager baseUrl

    key <- Environment.getEnv "ANTHROPIC_KEY"

    let model = "claude-sonnet-4-5-20250929"
    let version = Just "2023-06-01"
    let Methods{..} = V1.makeMethods clientEnv (Text.pack key) version

    let messagesMinimalTest =
            HUnit.testCase "Create message - minimal" do
                Messages.MessageResponse{ content } <-
                    createMessage
                        Messages._CreateMessage
                            { Messages.model = model
                            , Messages.messages =
                                [ Messages.Message
                                    { Messages.role = Messages.User
                                    , Messages.content =
                                        [ Messages.textContent "Say hello in one word."
                                        ]
                                    , Messages.cache_control = Nothing
                                    }
                                ]
                            , Messages.max_tokens = 100
                            }

                HUnit.assertBool "Response should have content"
                    (not (null content))

    let messagesWithSystemTest =
            HUnit.testCase "Create message - with system prompt" do
                Messages.MessageResponse{ content } <-
                    createMessage
                        Messages._CreateMessage
                            { Messages.model = model
                            , Messages.messages =
                                [ Messages.Message
                                    { Messages.role = Messages.User
                                    , Messages.content =
                                        [ Messages.textContent "What are you?"
                                        ]
                                    , Messages.cache_control = Nothing
                                    }
                                ]
                            , Messages.max_tokens = 100
                            , Messages.system = Just "You are a helpful pirate. Respond in pirate speak."
                            }

                HUnit.assertBool "Response should have content"
                    (not (null content))

    let messagesStreamingTest =
            HUnit.testCase "Create message - streaming" do
                acc <- IORef.newIORef Text.empty
                done <- Concurrent.newEmptyMVar

                let onEvent (Left _err) = Concurrent.putMVar done ()
                    onEvent (Right ev) = case ev of
                        Messages.Content_Block_Delta{ Messages.delta = d } ->
                            case d of
                                Messages.Delta_Text_Delta{ Messages.text = t } ->
                                    IORef.modifyIORef' acc (<> t)
                                _ -> pure ()
                        Messages.Message_Stop ->
                            Concurrent.putMVar done ()
                        _ -> pure ()

                createMessageStreamTyped
                    Messages._CreateMessage
                        { Messages.model = model
                        , Messages.messages =
                            [ Messages.Message
                                { Messages.role = Messages.User
                                , Messages.content =
                                    [ Messages.textContent "Write a haiku about code."
                                    ]
                                , Messages.cache_control = Nothing
                                }
                            ]
                        , Messages.max_tokens = 200
                        }
                    onEvent

                _ <- Concurrent.takeMVar done
                text <- IORef.readIORef acc
                HUnit.assertBool "Expected non-empty streamed text"
                    (not (Text.null text))

    let messagesConversationTest =
            HUnit.testCase "Create message - multi-turn conversation" do
                Messages.MessageResponse{ content } <-
                    createMessage
                        Messages._CreateMessage
                            { Messages.model = model
                            , Messages.messages =
                                [ Messages.Message
                                    { Messages.role = Messages.User
                                    , Messages.content =
                                        [ Messages.textContent "My name is Alice."
                                        ]
                                    , Messages.cache_control = Nothing
                                    }
                                , Messages.Message
                                    { Messages.role = Messages.Assistant
                                    , Messages.content =
                                        [ Messages.textContent "Hello Alice! Nice to meet you."
                                        ]
                                    , Messages.cache_control = Nothing
                                    }
                                , Messages.Message
                                    { Messages.role = Messages.User
                                    , Messages.content =
                                        [ Messages.textContent "What is my name?"
                                        ]
                                    , Messages.cache_control = Nothing
                                    }
                                ]
                            , Messages.max_tokens = 100
                            }

                HUnit.assertBool "Response should have content"
                    (not (null content))

    let toolUseTest =
            HUnit.testCase "Create message - tool use" do
                let calculatorTool = Tool.Tool
                        { Tool.name = "calculator"
                        , Tool.description = Just "Perform basic arithmetic"
                        , Tool.input_schema = Tool.InputSchema
                            { Tool.type_ = "object"
                            , Tool.properties = Just $ Aeson.object
                                [ "expression" Aeson..= Aeson.object
                                    [ "type" Aeson..= ("string" :: Text.Text)
                                    , "description" Aeson..= ("Math expression like 2+2" :: Text.Text)
                                    ]
                                ]
                            , Tool.required = Just ["expression"]
                            , Tool.additionalProperties = Nothing
                            }
                        , Tool.strict = Nothing
                        }

                Messages.MessageResponse{ stop_reason, content } <-
                    createMessage
                        Messages._CreateMessage
                            { Messages.model = model
                            , Messages.messages =
                                [ Messages.Message
                                    { Messages.role = Messages.User
                                    , Messages.content =
                                        [ Messages.textContent "What is 15 + 27? Use the calculator tool."
                                        ]
                                    , Messages.cache_control = Nothing
                                    }
                                ]
                            , Messages.max_tokens = 200
                            , Messages.tools = Just [Tool.inlineTool calculatorTool]
                            , Messages.tool_choice = Just Tool.ToolChoice_Any
                            }

                -- Should stop for tool use
                HUnit.assertEqual "Should stop for tool use"
                    (Just Messages.Tool_Use)
                    stop_reason

                -- Should have a tool_use content block
                let isToolUseBlock (Messages.ContentBlock_Tool_Use{}) = True
                    isToolUseBlock _ = False
                let hasToolUse = any isToolUseBlock (toList content)
                HUnit.assertBool "Should have tool_use content block" hasToolUse

    let tokenCountingTest =
            HUnit.testCase "Count tokens" do
                Messages.TokenCount{ input_tokens } <-
                    countTokens
                        Messages.CountTokensRequest
                            { Messages.model = model
                            , Messages.messages =
                                [ Messages.Message
                                    { Messages.role = Messages.User
                                    , Messages.content =
                                        [ Messages.textContent "Hello, world!"
                                        ]
                                    , Messages.cache_control = Nothing
                                    }
                                ]
                            , Messages.system = Nothing
                            , Messages.tools = Nothing
                            , Messages.tool_choice = Nothing
                            }

                HUnit.assertBool "Should have positive token count"
                    (input_tokens > 0)

    -- Structured outputs tests require beta header
    let structuredOutputsOptions = V1.defaultClientOptions
            { V1.apiKey = Text.pack key
            , V1.anthropicBeta = Just "structured-outputs-2025-11-13"
            }
    let V1.Methods{ V1.createMessage = createMessageStructured } =
            V1.makeMethodsWith clientEnv structuredOutputsOptions

    let jsonOutputsTest =
            HUnit.testCase "Create message - JSON outputs" do
                -- Define output schema
                let outputSchema = Aeson.object
                        [ "type" Aeson..= ("object" :: Text.Text)
                        , "properties" Aeson..= Aeson.object
                            [ "name" Aeson..= Aeson.object
                                [ "type" Aeson..= ("string" :: Text.Text)
                                ]
                            , "age" Aeson..= Aeson.object
                                [ "type" Aeson..= ("integer" :: Text.Text)
                                ]
                            ]
                        , "required" Aeson..= (["name", "age"] :: [Text.Text])
                        , "additionalProperties" Aeson..= False
                        ]

                Messages.MessageResponse{ stop_reason, content } <-
                    createMessageStructured
                        Messages._CreateMessage
                            { Messages.model = model
                            , Messages.messages =
                                [ Messages.Message
                                    { Messages.role = Messages.User
                                    , Messages.content =
                                        [ Messages.textContent "Extract: John Smith is 30 years old."
                                        ]
                                    , Messages.cache_control = Nothing
                                    }
                                ]
                            , Messages.max_tokens = 200
                            , Messages.output_format = Just (Messages.jsonSchemaFormat outputSchema)
                            }

                -- Should complete normally
                HUnit.assertEqual "Should complete with end_turn"
                    (Just Messages.End_Turn)
                    stop_reason

                -- Should have text content that is valid JSON
                case toList content of
                    [Messages.ContentBlock_Text{ Messages.text = jsonText }] -> do
                        case Aeson.eitherDecodeStrict (Text.encodeUtf8 jsonText) of
                            Left err -> HUnit.assertFailure $ "Invalid JSON: " <> err
                            Right (obj :: Aeson.Value) -> do
                                -- Check it has the expected structure
                                case obj of
                                    Aeson.Object _ -> pure ()
                                    _ -> HUnit.assertFailure "Expected JSON object"
                    _ -> HUnit.assertFailure "Expected single text content block"

    let strictToolUseTest =
            HUnit.testCase "Create message - strict tool use" do
                -- Define a tool with strict mode
                let strictTool = Tool.strictFunctionTool
                        "get_user"
                        (Just "Get user by ID")
                        $ Aeson.object
                            [ "type" Aeson..= ("object" :: Text.Text)
                            , "properties" Aeson..= Aeson.object
                                [ "user_id" Aeson..= Aeson.object
                                    [ "type" Aeson..= ("integer" :: Text.Text)
                                    , "description" Aeson..= ("The user ID" :: Text.Text)
                                    ]
                                ]
                            , "required" Aeson..= (["user_id"] :: [Text.Text])
                            ]

                Messages.MessageResponse{ stop_reason, content } <-
                    createMessageStructured
                        Messages._CreateMessage
                            { Messages.model = model
                            , Messages.messages =
                                [ Messages.Message
                                    { Messages.role = Messages.User
                                    , Messages.content =
                                        [ Messages.textContent "Get user 42"
                                        ]
                                    , Messages.cache_control = Nothing
                                    }
                                ]
                            , Messages.max_tokens = 200
                            , Messages.tools = Just [Tool.inlineTool strictTool]
                            , Messages.tool_choice = Just Tool.ToolChoice_Any
                            }

                -- Should stop for tool use
                HUnit.assertEqual "Should stop for tool use"
                    (Just Messages.Tool_Use)
                    stop_reason

                -- Should have a tool_use block with integer user_id
                let toolUseBlocks =
                        [ (toolId, toolInput)
                        | Messages.ContentBlock_Tool_Use{ Messages.id = toolId, Messages.input = toolInput }
                            <- toList content
                        ]
                case toolUseBlocks of
                    [(_, toolInput)] -> do
                        -- Verify user_id is an integer (strict mode guarantees this)
                        case Aeson.Types.parseMaybe (Aeson.withObject "input" (\o -> o Aeson..: "user_id")) toolInput of
                            Just (userId :: Int) ->
                                HUnit.assertEqual "user_id should be 42" 42 userId
                            Nothing ->
                                HUnit.assertFailure "user_id should be present and be an integer"
                    _ -> HUnit.assertFailure "Expected exactly one tool_use block"

    -- Tool search test requires beta header
    let betaOptions = V1.defaultClientOptions
            { V1.apiKey = Text.pack key
            , V1.anthropicBeta = Just "advanced-tool-use-2025-11-20"
            }
    let V1.Methods{ V1.createMessage = createMessageBeta } =
            V1.makeMethodsWith clientEnv betaOptions

    let toolSearchTest =
            HUnit.testCase "Create message - tool search" do
                -- Define several tools to search through
                let weatherTool = Tool.Tool
                        { Tool.name = "get_weather"
                        , Tool.description = Just "Get the current weather for a location"
                        , Tool.input_schema = Tool.InputSchema
                            { Tool.type_ = "object"
                            , Tool.properties = Just $ Aeson.object
                                [ "location" Aeson..= Aeson.object
                                    [ "type" Aeson..= ("string" :: Text.Text)
                                    ]
                                ]
                            , Tool.required = Just ["location"]
                            , Tool.additionalProperties = Nothing
                            }
                        , Tool.strict = Nothing
                        }

                let stockTool = Tool.Tool
                        { Tool.name = "get_stock_price"
                        , Tool.description = Just "Get the stock price for a ticker"
                        , Tool.input_schema = Tool.InputSchema
                            { Tool.type_ = "object"
                            , Tool.properties = Just $ Aeson.object
                                [ "ticker" Aeson..= Aeson.object
                                    [ "type" Aeson..= ("string" :: Text.Text)
                                    ]
                                ]
                            , Tool.required = Just ["ticker"]
                            , Tool.additionalProperties = Nothing
                            }
                        , Tool.strict = Nothing
                        }

                let calculatorTool' = Tool.Tool
                        { Tool.name = "calculator"
                        , Tool.description = Just "Perform arithmetic calculations"
                        , Tool.input_schema = Tool.InputSchema
                            { Tool.type_ = "object"
                            , Tool.properties = Just $ Aeson.object
                                [ "expression" Aeson..= Aeson.object
                                    [ "type" Aeson..= ("string" :: Text.Text)
                                    ]
                                ]
                            , Tool.required = Just ["expression"]
                            , Tool.additionalProperties = Nothing
                            }
                        , Tool.strict = Nothing
                        }

                -- Use tool search with deferred tools
                let tools =
                        [ Tool.toolSearchRegex
                        , Tool.deferredTool weatherTool
                        , Tool.deferredTool stockTool
                        , Tool.deferredTool calculatorTool'
                        ]

                Messages.MessageResponse{ stop_reason, content } <-
                    createMessageBeta
                        Messages._CreateMessage
                            { Messages.model = model
                            , Messages.messages =
                                [ Messages.Message
                                    { Messages.role = Messages.User
                                    , Messages.content =
                                        [ Messages.textContent "What's the weather in Paris?"
                                        ]
                                    , Messages.cache_control = Nothing
                                    }
                                ]
                            , Messages.max_tokens = 200
                            , Messages.tools = Just tools
                            , Messages.tool_choice = Just Tool.ToolChoice_Any
                            }

                -- Should stop for tool use
                HUnit.assertEqual "Should stop for tool use"
                    (Just Messages.Tool_Use)
                    stop_reason

                -- Should have either a tool_use or server_tool_use content block
                let isToolUseBlock' (Messages.ContentBlock_Tool_Use{}) = True
                    isToolUseBlock' (Messages.ContentBlock_Server_Tool_Use{}) = True
                    isToolUseBlock' _ = False
                let hasToolUse' = any isToolUseBlock' (toList content)
                HUnit.assertBool "Should have tool_use or server_tool_use content block" hasToolUse'

    -- Programmatic tool calling test (uses same beta header)
    let programmaticToolCallingTest =
            HUnit.testCase "Create message - programmatic tool calling" do
                -- Define a simple tool for PTC
                let queryTool = Tool.Tool
                        { Tool.name = "get_data"
                        , Tool.description = Just "Get data for a key"
                        , Tool.input_schema = Tool.InputSchema
                            { Tool.type_ = "object"
                            , Tool.properties = Just $ Aeson.object
                                [ "key" Aeson..= Aeson.object
                                    [ "type" Aeson..= ("string" :: Text.Text)
                                    ]
                                ]
                            , Tool.required = Just ["key"]
                            , Tool.additionalProperties = Nothing
                            }
                        , Tool.strict = Nothing
                        }

                -- Tools: code execution + query tool with allowed_callers
                let tools =
                        [ Tool.codeExecutionTool
                        , Tool.allowCallers Tool.allowedCallersCodeExecution (Tool.inlineTool queryTool)
                        ]

                -- Initial request prompting code execution
                let initialMessage = Messages.Message
                        { Messages.role = Messages.User
                        , Messages.content =
                            [ Messages.textContent
                                "Use code execution to call the get_data tool twice: once with key='alpha' and once with key='beta'. Then calculate the sum of the returned values."
                            ]
                        , Messages.cache_control = Nothing
                        }

                -- First request
                Messages.MessageResponse{ stop_reason, content, container } <-
                    createMessageBeta
                        Messages._CreateMessage
                            { Messages.model = model
                            , Messages.messages = [initialMessage]
                            , Messages.max_tokens = 4096
                            , Messages.tools = Just tools
                            }

                -- Should stop for tool use
                HUnit.assertEqual "Should stop for tool use"
                    (Just Messages.Tool_Use)
                    stop_reason

                -- Should have a server_tool_use for code_execution
                let hasCodeExecution = any isCodeExecutionServerToolUse (toList content)
                HUnit.assertBool "Should have server_tool_use for code_execution" hasCodeExecution

                -- Should have container info
                HUnit.assertBool "Should have container info" (container /= Nothing)

                -- Should have tool_use with programmatic caller
                let programmaticToolUses =
                        [ ()
                        | Messages.ContentBlock_Tool_Use{ Messages.caller = Just (Messages.ToolCaller_CodeExecution{}) }
                            <- toList content
                        ]
                HUnit.assertBool "Should have at least one programmatic tool call"
                    (not (null programmaticToolUses))

                -- Multi-turn loop: continue until end_turn or code_execution_tool_result
                let containerId = fmap (\Messages.ContainerInfo{ Messages.id = cid } -> cid) container

                -- Build initial assistant message from first response
                let assistantContent1 = mapMaybe Messages.contentBlockToContent (toList content)
                let assistantMessage1 = Messages.Message
                        { Messages.role = Messages.Assistant
                        , Messages.content = Vector.fromList assistantContent1
                        , Messages.cache_control = Nothing
                        }
                let toolResults1 = processTestToolCalls content
                let userMessage1 = Messages.Message
                        { Messages.role = Messages.User
                        , Messages.content = Vector.fromList toolResults1
                        , Messages.cache_control = Nothing
                        }

                -- Loop function
                let loop :: [Messages.Message] -> Maybe Text.Text -> Int -> IO ()
                    loop _ _ turn | turn > 5 = HUnit.assertFailure "Max turns reached"
                    loop msgs containerId' turn = do
                        Messages.MessageResponse{ stop_reason = sr, content = c, container = cont } <-
                            createMessageBeta
                                Messages._CreateMessage
                                    { Messages.model = model
                                    , Messages.messages = Vector.fromList msgs
                                    , Messages.max_tokens = 4096
                                    , Messages.tools = Just tools
                                    , Messages.container = containerId'
                                    }

                        let newContainerId = case cont of
                                Just Messages.ContainerInfo{ Messages.id = cid } -> Just cid
                                Nothing -> containerId'

                        let hasCodeExecutionResult = any isCodeExecutionResult (toList c)
                        let isEndTurn = sr == Just Messages.End_Turn

                        if hasCodeExecutionResult || isEndTurn
                            then pure ()  -- Success!
                            else if sr == Just Messages.Tool_Use
                                then do
                                    -- Build next assistant and user messages
                                    let assistantContentN = mapMaybe Messages.contentBlockToContent (toList c)
                                    let assistantMessageN = Messages.Message
                                            { Messages.role = Messages.Assistant
                                            , Messages.content = Vector.fromList assistantContentN
                                            , Messages.cache_control = Nothing
                                            }
                                    let toolResultsN = processTestToolCalls c
                                    let userMessageN = Messages.Message
                                            { Messages.role = Messages.User
                                            , Messages.content = Vector.fromList toolResultsN
                                            , Messages.cache_control = Nothing
                                            }
                                    loop (msgs <> [assistantMessageN, userMessageN]) newContainerId (turn + 1)
                                else HUnit.assertFailure $ "Unexpected stop_reason: " <> show sr

                -- Start the loop
                loop [initialMessage, assistantMessage1, userMessage1] containerId 1

    let tests =
            [ messagesMinimalTest
            , messagesWithSystemTest
            , messagesStreamingTest
            , messagesConversationTest
            , toolUseTest
            , tokenCountingTest
            , jsonOutputsTest
            , strictToolUseTest
            , toolSearchTest
            , programmaticToolCallingTest
            ]

    Tasty.defaultMain (Tasty.testGroup "Claude API Tests" tests)

-- | Check if a content block is a server_tool_use for code_execution
isCodeExecutionServerToolUse :: Messages.ContentBlock -> Bool
isCodeExecutionServerToolUse (Messages.ContentBlock_Server_Tool_Use{ Messages.name = "code_execution" }) = True
isCodeExecutionServerToolUse _ = False

-- | Check if a content block is a code_execution_tool_result
isCodeExecutionResult :: Messages.ContentBlock -> Bool
isCodeExecutionResult (Messages.ContentBlock_Code_Execution_Tool_Result{}) = True
isCodeExecutionResult _ = False

-- | Process tool calls for testing - returns fake results
processTestToolCalls :: Vector.Vector Messages.ContentBlock -> [Messages.Content]
processTestToolCalls content =
    [ Messages.Content_Tool_Result
        { Messages.tool_use_id = toolId
        , Messages.content = Just result
        , Messages.is_error = Nothing
        }
    | Messages.ContentBlock_Tool_Use{ Messages.id = toolId, Messages.name = toolName, Messages.input = toolInput, Messages.caller = _ }
        <- toList content
    , let result = case toolName of
            "get_data" -> case Aeson.Types.parseMaybe (Aeson.withObject "input" (\o -> o Aeson..: "key")) toolInput of
                Just ("alpha" :: Text.Text) -> "{\"value\": 100}"
                Just "beta" -> "{\"value\": 200}"
                _ -> "{\"value\": 0}"
            _ -> "{\"error\": \"unknown tool\"}"
    ]
