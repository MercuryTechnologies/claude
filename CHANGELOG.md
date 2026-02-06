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
