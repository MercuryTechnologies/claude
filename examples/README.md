# Examples

## claude-example

Basic message sending. Reads user input and sends it to Claude, printing the response.

## claude-stream-example

Streaming responses. Sends a request and prints text deltas as they arrive in real-time.

## claude-tool-example

Tool/function calling. Defines a `get_weather` tool, handles `tool_use` responses, and sends tool results back to Claude.

## claude-tool-search-example

Server-side [tool search](https://docs.anthropic.com/en/docs/agents-and-tools/tool-use/tool-search-tool) for working with large tool catalogs. Instead of loading all tool definitions upfront, Claude searches and loads only the tools it needs on-demand. This keeps context efficient and improves tool selection accuracy when you have many tools (30+).

The example defines several tools (weather, stock price, currency conversion, calculator, web search) with `defer_loading: true`, then uses `tool_search_tool_regex` to discover relevant tools at runtime.

Requires beta header: `advanced-tool-use-2025-11-20`

## claude-programmatic-tool-calling-example

[Programmatic Tool Calling (PTC)](https://docs.anthropic.com/en/docs/agents-and-tools/tool-use/programmatic-tool-calling) allows Claude to write code that calls tools within a code execution container, rather than requiring round trips through the model for each tool invocation. This reduces latency for multi-tool workflows and decreases token consumption by allowing Claude to filter or process data before it reaches the context window.

The example defines a `query_database` tool with `allowed_callers = ["code_execution_20250825"]`. Claude then writes Python code that queries multiple database regions in a loop and aggregates the results—all in a single code execution, rather than multiple model round trips.

Requires beta header: `advanced-tool-use-2025-11-20` and the `code_execution_20250825` tool.

## claude-vision-example

Vision/image analysis. Sends a base64-encoded image to Claude and receives a description.

---

Run any example with:

```bash
cabal run <example-name>
```
