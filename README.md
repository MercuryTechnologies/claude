# `claude`

Haskell bindings to Anthropic's Claude API using `servant`.

## Example usage

```haskell
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}

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

    MessageResponse{ content } <- createMessage _CreateMessage
        { model = "claude-sonnet-4-5-20250929"
        , messages = [ Message{ role = User, content = [ textContent "Hello!" ] } ]
        , max_tokens = 1024
        }

    let display (ContentBlock_Text{ text }) = Text.IO.putStrLn text
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
cp .envrc.sample .envrc
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

See [examples/README.md](examples/README.md) for descriptions of each example.

```bash
cabal run claude-example
cabal run claude-stream-example
cabal run claude-tool-example
cabal run claude-vision-example
cabal run claude-tool-search-example
cabal run claude-programmatic-tool-calling-example
```

### Advanced Examples

The `claude-tool-search-example` and `claude-programmatic-tool-calling-example` demonstrate beta features that require the `advanced-tool-use-2025-11-20` beta header:

- **[Tool Search Tool](https://platform.claude.com/docs/en/agents-and-tools/tool-use/tool-search-tool)**: Server-side tool search for efficiently handling large numbers of tools
- **[Programmatic Tool Calling (PTC)](https://platform.claude.com/docs/en/agents-and-tools/tool-use/programmatic-tool-calling)**: Claude writes and executes code to call multiple tools and aggregate results

To enable these features, use `makeMethodsWith` with the beta header:

```haskell
let options = defaultClientOptions
        { apiKey = key
        , anthropicBeta = Just "advanced-tool-use-2025-11-20"
        }
let Methods{ createMessage } = makeMethodsWith clientEnv options
```
