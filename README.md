# `claude`

Haskell bindings to Anthropic's Claude API using `servant`.

## Example usage

```haskell
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

    text <- Text.IO.getLine

    MessageResponse{ content } <- createMessage _CreateMessage
        { model = "claude-sonnet-4-5"
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
```

## Setup

### Using Nix with Flakes (Recommended)

1. Ensure you have Nix with flakes enabled
2. Copy the sample environment file:

```bash
cp .envrc.sample .envrc
```

3. Edit `.envrc` with your Anthropic API key
4. Run `direnv allow`

### Manual Setup

```bash
cabal build
```

## Environment Variables

Set your Anthropic API key as an environment variable:

```bash
# Option 1: Set directly in your shell
export ANTHROPIC_KEY="your-anthropic-api-key"

# Option 2: Using .envrc with direnv (recommended)
(umask 077; cp .envrc.sample .envrc)
# Edit .envrc to add your API key
direnv allow
```

The API key is needed for running the test suite and example programs.

## Testing

Run the test suite:

```bash
cabal test
```

## Running the Examples

```bash
# Make sure your API key is set (either via .envrc or export)
# If using direnv with proper .envrc setup, this happens automatically

# Build and run the example
cabal run claude-example

# Run the streaming example
cabal run claude-stream-example
```
