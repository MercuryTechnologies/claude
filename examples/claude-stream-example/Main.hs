{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import System.Environment (getEnv)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

import qualified Claude.V1 as V1
import qualified Claude.V1.Messages as Messages
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    key <- T.pack <$> getEnv "ANTHROPIC_KEY"
    env <- V1.getClientEnv "https://api.anthropic.com"

    let V1.Methods{ createMessageStreamTyped } = V1.makeMethods env key (Just "2023-06-01")

    let onEvent (Left err) = hPutStrLn stderr ("stream error: " <> T.unpack err)
        onEvent (Right ev) = case ev of
            -- Print text deltas as they arrive
            Messages.Content_Block_Delta{ Messages.delta = d } ->
                case d of
                    Messages.Delta_Text_Delta{ Messages.text = t } ->
                        TIO.putStr t >> hFlush stdout
                    _ -> pure ()
            -- Print newline when message is done
            Messages.Message_Stop -> putStrLn ""
            -- Ignore other events
            _ -> pure ()

    -- Simple haiku test
    let req = Messages._CreateMessage
            { Messages.model = "claude-sonnet-4-5"
            , Messages.messages =
                [ Messages.Message
                    { Messages.role = Messages.User
                    , Messages.content =
                        [ Messages.Content_Text
                            { Messages.text = "Write a short haiku about the sea."
                            }
                        ]
                    }
                ]
            , Messages.max_tokens = 200
            }

    createMessageStreamTyped req onEvent
