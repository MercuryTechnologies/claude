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
import           Data.Foldable (toList)
import qualified Data.IORef as IORef
import qualified Data.Text as Text
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
                                        [ Messages.Content_Text
                                            { Messages.text = "Say hello in one word."
                                            }
                                        ]
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
                                        [ Messages.Content_Text
                                            { Messages.text = "What are you?"
                                            }
                                        ]
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
                                    [ Messages.Content_Text
                                        { Messages.text = "Write a haiku about code."
                                        }
                                    ]
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
                                        [ Messages.Content_Text
                                            { Messages.text = "My name is Alice."
                                            }
                                        ]
                                    }
                                , Messages.Message
                                    { Messages.role = Messages.Assistant
                                    , Messages.content =
                                        [ Messages.Content_Text
                                            { Messages.text = "Hello Alice! Nice to meet you."
                                            }
                                        ]
                                    }
                                , Messages.Message
                                    { Messages.role = Messages.User
                                    , Messages.content =
                                        [ Messages.Content_Text
                                            { Messages.text = "What is my name?"
                                            }
                                        ]
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
                            }
                        }

                Messages.MessageResponse{ stop_reason, content } <-
                    createMessage
                        Messages._CreateMessage
                            { Messages.model = model
                            , Messages.messages =
                                [ Messages.Message
                                    { Messages.role = Messages.User
                                    , Messages.content =
                                        [ Messages.Content_Text
                                            { Messages.text = "What is 15 + 27? Use the calculator tool."
                                            }
                                        ]
                                    }
                                ]
                            , Messages.max_tokens = 200
                            , Messages.tools = Just [calculatorTool]
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

    let tests =
            [ messagesMinimalTest
            , messagesWithSystemTest
            , messagesStreamingTest
            , messagesConversationTest
            , toolUseTest
            ]

    Tasty.defaultMain (Tasty.testGroup "Claude API Tests" tests)
