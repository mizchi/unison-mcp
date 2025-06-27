# MCP Library Naming Convention Refactoring

## Current Issues with Naming

The current naming like `mcp.Client.create` doesn't follow Unison standard library conventions.

## Unison Standard Library Patterns

Based on standard library analysis:

1. **Type names are simple**: `Optional`, `List`, `Nat`, not `List.Type`
2. **Constructors use dot notation**: `Optional.Some`, `Optional.None`
3. **Functions are grouped by what they operate on**: `List.map`, `List.filter`
4. **Submodules for related functionality**: `data.List`, `data.Map`

## Proposed Refactoring

### Before:
```unison
type mcp.Client.Config = { ... }
type mcp.Client.State = { ... }
mcp.Client.create : Text -> Text -> mcp.Client.State
```

### After (Following Unison conventions):
```unison
-- Types at top level
type mcp.ClientConfig = { ... }
type mcp.ClientState = { ... }

-- Functions grouped by what they operate on
mcp.Client.create : Text -> Text -> ClientState
mcp.Client.initialize : ClientState -> (Json, ClientState)
mcp.Client.nextId : ClientState -> (Float, ClientState)

-- Or even simpler:
mcp.client.create : Text -> Text -> ClientState
mcp.client.initialize : ClientState -> (Json, ClientState)
```

## Recommended Structure

```
mcp/
  -- Core types (no nested types)
  Request
  Response  
  Error
  Tool
  ServerConfig
  ClientConfig
  ClientState
  
  -- Response builders
  response : Text -> Float -> Json -> Json
  errorResponse : Text -> Float -> Float -> Text -> Json
  
  -- JSON utilities
  json/
    getField : Text -> Json -> Optional Json
    getTextField : Text -> Json -> Optional Text
    getNumberField : Text -> Json -> Optional Float
  
  -- Client functions
  client/
    create : Text -> Text -> ClientState
    initialize : ClientState -> (Json, ClientState)
    listTools : ClientState -> (Json, ClientState)
    callTool : ClientState -> Text -> Json -> (Json, ClientState)
    parseResponse : Json -> Either Error Json
    parseTools : Json -> [Tool]
    
  -- Server functions  
  server/
    info : ServerConfig -> Json
    capabilities : Json
    handleInitialize : ServerConfig -> Json -> Json
    handleToolsList : ServerConfig -> Json -> Json
    
  -- Error codes
  errors/
    parseError : Float
    invalidRequest : Float
    methodNotFound : Float
    invalidParams : Float
    internalError : Float
```

## Examples Following Convention

```unison
-- Using the refactored API
use mcp

-- Create client (lowercase namespace)
client = mcp.client.create "my-app" "1.0.0"

-- Types are directly under mcp
config : mcp.ClientConfig
state : mcp.ClientState

-- Error handling
match mcp.client.parseResponse resp with
  Right result -> handleResult result
  Left (mcp.Error code message data) -> handleError code
```

This follows the same pattern as:
- `List.map`, `List.filter` (functions on lists)
- `Optional.Some`, `Optional.None` (constructors)
- `Nat.+`, `Nat.*` (operations on Nats)

## Benefits

1. **Consistency**: Matches Unison ecosystem conventions
2. **Discoverability**: Users can guess function names
3. **Simplicity**: No deeply nested types
4. **Clarity**: Clear what functions operate on what