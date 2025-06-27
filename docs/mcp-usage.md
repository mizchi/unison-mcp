# MCP Library Usage Guide

## Installation

```ucm
.> lib.install @mizchi/mcp
```

## Viewing the Interface

### List all MCP functions and types
```ucm
.> find mcp
```

### View specific namespace
```ucm
.> ls lib.mizchi_mcp_0_2_0.mcp
```

### View type definitions
```ucm
.> view mcp.Request
.> view mcp.Response
.> view mcp.Error
.> view mcp.Tool
```

## Core Types

### 1. Request and Response Types
```unison
-- MCP Request structure
type mcp.Request = {
  jsonrpc: Text,
  id: Float,
  method: Text,
  params: Optional Json
}

-- MCP Response structure
type mcp.Response = {
  jsonrpc: Text,
  id: Float,
  result: Optional Json,
  error: Optional mcp.Error
}

-- Error structure
type mcp.Error = {
  code: Float,
  message: Text,
  data: Optional Json
}
```

### 2. Tool Definition
```unison
type mcp.Tool = {
  name: Text,
  description: Optional Text,
  inputSchema: Json
}
```

### 3. Client Types
```unison
-- Client configuration
type mcp.ClientConfig = {
  name: Text,
  version: Text,
  protocolVersion: Text
}

-- Stateful client
type mcp.ClientState = {
  nextId: Float,
  config: mcp.ClientConfig
}
```

## Client Usage Examples

### Basic Client Usage (Stateful)

```unison
use mcp

-- 1. Create a client
client = mcp.client.create "my-app" "1.0.0"

-- 2. Initialize connection
(initReq, client2) = mcp.client.initialize client
-- Send initReq via your transport layer
-- Receive response

-- 3. List available tools
(toolsReq, client3) = mcp.client.listTools client2
-- Send toolsReq and receive response

-- 4. Parse the tools response
toolsResult = processResponse toolsResponse  -- Your response
tools = mcp.client.parseTools toolsResult

-- 5. Call a tool
(callReq, client4) = mcp.client.callTool client3 "translate" (Json.Object [
  ("text", Json.Text "Hello world"),
  ("targetLanguage", Json.Text "ja")
])

-- 6. Parse response with error handling
match mcp.client.parseResponse callResponse with
  Right result -> 
    -- Handle successful result
    processResult result
  Left error ->
    -- Handle error
    handleError error
```

### Simple Helper Functions

```unison
-- Quick initialization and tool listing
(initReq, toolsReq, client) = mcp.client.simpleInit "my-app" "1.0.0"

-- Simple tool call with named arguments
(req, newClient) = mcp.client.simpleToolCall client "translate" [
  ("text", Json.Text "Hello"),
  ("language", Json.Text "ja")
]
```

### Low-level API (Manual ID Management)

```unison
-- Create individual requests with manual IDs
initReq = mcp.initializeRequest 1.0
toolsReq = mcp.toolsListRequest 2.0
callReq = mcp.toolsCallRequest 3.0 "translate" (Json.Object [
  ("text", Json.Text "Hello")
])
```

## Server Usage Examples

### Creating an MCP Server

```unison
-- 1. Define your server configuration
myServer : mcp.ServerConfig
myServer = mcp.ServerConfig.ServerConfig 
  "translation-server"     -- name
  "1.0.0"                 -- version
  "AI Translation Server" -- description
  [translateTool, detectLanguageTool]  -- tools

-- 2. Define your tools
translateTool : mcp.Tool
translateTool = mcp.Tool.Tool 
  "translate" 
  (Some "Translate text between languages")
  translateSchema

translateSchema : Json
translateSchema = Json.Object [
  ("type", Json.Text "object"),
  ("properties", Json.Object [
    ("text", Json.Object [
      ("type", Json.Text "string"),
      ("description", Json.Text "Text to translate")
    ]),
    ("targetLanguage", Json.Object [
      ("type", Json.Text "string"),
      ("description", Json.Text "Target language code")
    ])
  ]),
  ("required", Json.Array [Json.Text "text", Json.Text "targetLanguage"])
]

-- 3. Handle incoming requests
handleRequest : Json -> Json
handleRequest request =
  match mcp.getTextField "method" request with
    Some "initialize" -> mcp.handleInitialize myServer request
    Some "tools/list" -> mcp.handleToolsList myServer request
    Some "tools/call" -> handleToolCall request
    _ -> mcp.errorResponse "2.0" 1.0 mcp.errors.methodNotFound "Method not found"

-- 4. Implement tool execution
handleToolCall : Json -> Json
handleToolCall request =
  -- Extract tool name and arguments
  match mcp.getField "params" request with
    Some params ->
      match (mcp.getTextField "name" params, mcp.getField "arguments" params) with
        (Some "translate", Some args) -> executeTranslate args
        (Some "detectLanguage", Some args) -> executeDetectLanguage args
        _ -> mcp.errorResponse "2.0" 1.0 mcp.errors.invalidParams "Unknown tool"
    None -> mcp.errorResponse "2.0" 1.0 mcp.errors.invalidParams "Missing params"
```

## JSON Utilities

```unison
-- Extract fields from JSON
match mcp.getField "method" jsonRequest with
  Some (Json.Text method) -> method
  _ -> "unknown"

-- Get text field directly
methodName = mcp.getTextField "method" jsonRequest

-- Get number field
requestId = mcp.getNumberField "id" jsonRequest
```

## Error Handling

### Standard Error Codes
```unison
mcp.errors.parseError      -- -32700.0
mcp.errors.invalidRequest  -- -32600.0
mcp.errors.methodNotFound  -- -32601.0
mcp.errors.invalidParams   -- -32602.0
mcp.errors.internalError   -- -32603.0
```

### Creating Error Responses
```unison
-- Simple error
errorResp = mcp.errorResponse "2.0" 1.0 mcp.errors.methodNotFound "Unknown method"

-- With custom error code
customError = mcp.errorResponse "2.0" 1.0 -32000.0 "Custom error message"
```

## Complete Example: Echo Server

```unison
-- Echo server that returns the input
echoServer : mcp.ServerConfig
echoServer = mcp.ServerConfig.ServerConfig 
  "echo-server" 
  "1.0.0" 
  "Simple echo server"
  [echoTool]

echoTool : mcp.Tool
echoTool = mcp.Tool.Tool 
  "echo" 
  (Some "Echoes the input text")
  (Json.Object [
    ("type", Json.Text "object"),
    ("properties", Json.Object [
      ("text", Json.Object [
        ("type", Json.Text "string")
      ])
    ])
  ])

handleEchoRequest : Json -> Json
handleEchoRequest request =
  match mcp.getTextField "method" request with
    Some "initialize" -> mcp.handleInitialize echoServer request
    Some "tools/list" -> mcp.handleToolsList echoServer request
    Some "tools/call" -> 
      match mcp.getField "params" request with
        Some params ->
          match mcp.getField "arguments" params with
            Some args ->
              match mcp.getTextField "text" args with
                Some text ->
                  mcp.response "2.0" 1.0 (Json.Object [
                    ("content", Json.Array [
                      Json.Object [
                        ("type", Json.Text "text"),
                        ("text", Json.Text ("Echo: " ++ text))
                      ]
                    ])
                  ])
                None -> mcp.errorResponse "2.0" 1.0 mcp.errors.invalidParams "Missing text"
            None -> mcp.errorResponse "2.0" 1.0 mcp.errors.invalidParams "Missing arguments"
        None -> mcp.errorResponse "2.0" 1.0 mcp.errors.invalidParams "Missing params"
    _ -> mcp.errorResponse "2.0" 1.0 mcp.errors.methodNotFound "Unknown method"
```

## Viewing Available Functions

To see all available functions in the MCP library:

```ucm
-- List all mcp functions
.> find mcp

-- View specific function documentation
.> docs mcp.client.create
.> docs mcp.handleInitialize

-- View function implementation
.> view mcp.client.parseResponse
```

## Tips

1. **Use the stateful client** (`mcp.client`) for automatic ID management
2. **Always handle errors** with `Either` type when parsing responses
3. **Use helper functions** like `simpleInit` and `simpleToolCall` for common patterns
4. **Document your tools** with clear descriptions and schemas
5. **Follow MCP protocol** version "2024-11-05" for compatibility