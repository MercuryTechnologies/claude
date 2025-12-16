{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Example demonstrating vision capabilities with Claude
--
-- This example shows how to send images to Claude for analysis.
-- Images can be sent as base64-encoded data.
module Main where

import Claude.V1
import Claude.V1.Messages
import Data.Foldable (traverse_)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.IO as Text.IO
import qualified System.Environment as Environment

main :: IO ()
main = do
    key <- Environment.getEnv "ANTHROPIC_KEY"
    clientEnv <- getClientEnv "https://api.anthropic.com"

    let Methods{ createMessage } = makeMethods clientEnv (Text.pack key) (Just "2023-06-01")

    -- Read an image file and encode it as base64
    -- You can replace this path with any image you want to analyze
    Text.IO.putStrLn "Reading image file..."
    imageBytes <- BS.readFile "examples/claude-vision-example/test-image.png"
    let base64Image = Text.Encoding.decodeUtf8 (Base64.encode imageBytes)

    Text.IO.putStrLn "Sending image to Claude for analysis..."

    -- Create message with image
    MessageResponse{ content } <- createMessage _CreateMessage
        { model = "claude-sonnet-4-5-20250929"
        , messages =
            [ Message
                { role = User
                , content =
                    [ Content_Image
                        { source = ImageSource
                            { type_ = "base64"
                            , media_type = "image/png"
                            , data_ = base64Image
                            }
                        , cache_control = Nothing
                        }
                    , textContent "What do you see in this image? Please describe it in detail."
                    ]
                , cache_control = Nothing
                }
            ]
        , max_tokens = 1024
        }

    Text.IO.putStrLn "\nClaude's response:"
    let display (ContentBlock_Text{ text = t }) = Text.IO.putStrLn t
        display _ = pure ()

    traverse_ display content
