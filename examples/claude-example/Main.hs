{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import Claude.V1
import Claude.V1.Messages
import Data.Foldable (traverse_)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified System.Environment as Environment

main :: IO ()
main = do
    key <- Environment.getEnv "ANTHROPIC_KEY"

    clientEnv <- getClientEnv "https://api.anthropic.com"

    let Methods{ createMessage } = makeMethods clientEnv (Text.pack key) (Just "2023-06-01")

    Text.IO.putStrLn "Enter your message:"
    text <- Text.IO.getLine

    MessageResponse{ content } <- createMessage _CreateMessage
        { model = "claude-sonnet-4-20250514"
        , messages =
            [ Message
                { role = User
                , content = [ Content_Text{ text } ]
                }
            ]
        , max_tokens = 1024
        }

    let display (ContentBlock_Text{ text = t }) = Text.IO.putStrLn t
        display _ = pure ()

    traverse_ display content
