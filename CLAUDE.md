# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Development Commands

```bash
# Build
cabal build

# Test (requires ANTHROPIC_KEY env var - tests call live API)
cabal test

# Run examples (see examples/README.md for full list)
cabal run claude-example         # Basic message
cabal run claude-stream-example  # Streaming responses
cabal run claude-tool-example    # Tool/function calling
cabal run claude-vision-example  # Vision/image analysis

# Format code
stylish-haskell -i src/**/*.hs
```

### Environment Setup

**Nix (recommended):**
```bash
cp .envrc.sample .envrc
# Add ANTHROPIC_KEY to .envrc
direnv allow
```

**Manual:**
```bash
export ANTHROPIC_KEY="your-key"
cabal build
```

## Architecture

This is a type-safe Haskell client library for Anthropic's Claude API using Servant.

### Module Structure

```
Claude.V1              -- Main entrypoint: ClientEnv setup, API definition, Methods record
Claude.V1.Messages     -- Message API types: CreateMessage, MessageResponse, Content, streaming
Claude.V1.Messages.Batches -- Batch processing API
Claude.V1.Tool         -- Tool/function calling: Tool, ToolChoice, InputSchema, helpers
Claude.V1.Error        -- Error types
Claude.Prelude         -- Internal: JSON helpers (aesonOptions), re-exports
```

### Key Patterns

- **Servant-based API:** Type-safe REST client via `servant-client`
- **Methods record:** All API methods in a single record returned by `makeMethods`
- **Default values:** Use `_CreateMessage`, `_CreateBatch`, `_CountTokensRequest` as base records
- **Streaming:** Custom SSE parser in `Claude.V1` for streaming responses
- **JSON naming:** Field names auto-converted to snake_case via `aesonOptions` in Prelude
- **Beta features:** Pass `anthropicBeta` in `ClientOptions` via `makeMethodsWith`

### Creating API Requests

```haskell
-- Use default records and override fields
createMessage _CreateMessage
    { model = "claude-sonnet-4-5-20250929"
    , messages = [Message{ role = User, content = [textContent "Hello"] }]
    , max_tokens = 1024
    }
```

### Tool Helpers (Claude.V1.Tool)

```haskell
functionTool "name" "description" schema  -- Create tool definition
strictFunctionTool "name" "desc" schema   -- Tool with strict validation (structured outputs)
toolChoiceAuto / toolChoiceAny            -- Tool selection modes
toolChoiceTool "name"                     -- Force specific tool
```

## Adding New Examples

When adding a new example:
1. Create `examples/<name>/Main.hs`
2. Add executable stanza to `claude.cabal`
3. Document in `examples/README.md`

## Releasing Changes

When making changes to the library:
1. Bump version in `claude.cabal`
2. Add entry to `CHANGELOG.md` describing the changes
