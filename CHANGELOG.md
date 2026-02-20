1.4.0:

- Expand prompt caching support:
  - Add top-level `cache_control` on `CreateMessage` (automatic caching).
  - Add `ttl` support on `CacheControl` via `CacheTTL`.
  - Add cache-control helpers:
    - `ephemeralCacheWithTTL`
    - `ephemeralCacheWithTTLSeconds`
    - `ephemeralCacheWithTTLDuration`
- Add structured system prompt support:
  - `SystemPrompt` (string or blocks)
  - `SystemBlock` with per-block `cache_control`
  - `systemText`, `systemBlocks`, `systemTextBlock`, `systemTextBlockCached`
  - `CreateMessage.system` and `CountTokensRequest.system` now use `Maybe SystemPrompt`
  - breaking: `system = Just someTextVar` (where `someTextVar :: Text`) now needs `system = Just (systemText someTextVar)`
- Add tool-level cache control support:
  - `ToolDefinition` variants now accept optional `cache_control`
  - new helper `withToolCacheControl`

1.3.1:

- Remove `advanced-tool-use-2025-11-20` beta header requirement — tool search,
  programmatic tool calling, and code execution are now generally available
- Fix flaky PTC test by handling non-deterministic model behavior

1.3.0:

- Add request support for Claude Opus 4.6 routing and speed controls:
  - `inference_geo` on `CreateMessage`
  - `speed` on `CreateMessage` with `SpeedStandard` / `SpeedFast`
- Add context management/compaction request types:
  - `context_management` on `CreateMessage`
  - `ContextManagementConfig`, `ContextManagementEdit`
  - `CompactionTrigger` and `inputTokensTrigger` helper
- Add live API tests for:
  - `inference_geo`
  - `context_management` compaction (`compact-2026-01-12`)
  - `speed = fast` (`fast-mode-2026-02-01`, including waitlist-gated handling)

1.2.0:

- Add `Thinking` type with `ThinkingAdaptive` and `ThinkingEnabled` constructors
  - `thinking` field on `CreateMessage`
  - `ContentBlock_Thinking` and `ContentBlock_Redacted_Thinking` response blocks
  - `Content_Thinking` and `Content_Redacted_Thinking` for replaying thinking in multi-turn
  - `Delta_Thinking_Delta` and `Delta_Signature_Delta` for streaming
- Add `Model_Context_Window_Exceeded` constructor to `StopReason`

1.1.0:

- Replace `output_format` with `output_config` on `CreateMessage` (breaking change)
  - New `OutputConfig` type with `effort` and `format` fields
  - `effortConfig` helper for effort-only configuration
  - `jsonSchemaConfig` helper for structured output configuration
- Remove beta header requirement for structured outputs (now GA)

1.0.1:

- Add structured outputs support (beta feature `structured-outputs-2025-11-13`)
  - `OutputFormat` type and `jsonSchemaFormat` helper for JSON outputs
  - `output_format` field on `CreateMessage`
  - `strict` field on `Tool` and `strictFunctionTool` helper for strict tool validation
  - `additionalProperties` field on `InputSchema`
  - `Refusal` constructor for `StopReason`
- Add `claude-structured-outputs-example`

1.0.0:

- Initial release
