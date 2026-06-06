# haskell-mcp

A Haskell library for building
[Model Context Protocol (MCP)](https://modelcontextprotocol.io) servers. It
handles the JSON-RPC 2.0 transport, protocol lifecycle, and capability dispatch.
You just implement typeclasses for your tools, resources, and prompts.

## Building

```bash
stack build
stack exec haskell-mcp-exe   # runs the example server on port 8080
stack test
```

## Usage

Define your capabilities by implementing the relevant typeclasses, then register
them in an `MCPRegistry`.

### Tool

```haskell
data EchoTool = EchoTool
data EchoInput = EchoInput { message :: Text } deriving (Generic, FromJSON)

instance ToolCapability EchoTool where
  type Input EchoTool = EchoInput
  getMetadata _ = ToolMetadata { name = "echo", ... }
  runTool _ (EchoInput msg) = pure $ ToolResult [ToolText msg] False
```

### Resource

```haskell
data GreetingResource = GreetingResource

instance ResourceCapability GreetingResource where
  getMetadata _ = ResourceMetadata { uri = "text://greeting", name = "greeting", ... }
  readResource _ = pure $ ResourceReadResult [ResourceText "text://greeting" (Just "text/plain") "Hello!"]
```

### Prompt

```haskell
data SummarizePrompt = SummarizePrompt
data SummarizeArgs = SummarizeArgs Text

instance PromptCapability SummarizePrompt where
  type Args SummarizePrompt = SummarizeArgs
  getMetadata _ = PromptMetadata { name = "summarize", ... }
  runPrompt _ (SummarizeArgs t) = PromptMessage { role = User, content = PromptText ("Summarize: " <> t) }
```

### Wiring it up

```haskell
registry :: MCPRegistry
registry = MCPRegistry
  { serverInfo = ServerInfo { serverName = "my-server", ... }
  , serverInstruction = "Send JSON-RPC requests to /mcp"
  , tools     = [SomeTool EchoTool]
  , resources = [SomeResource GreetingResource]
  , prompts   = [SomePrompt SummarizePrompt]
  }

main :: IO ()
main = run 8080 (serve (Proxy :: Proxy JSONRpc) (mcpServer registry))
```

Capabilities advertised in the `initialize` response are derived automatically
from whichever lists are non-empty.

## Using as a dependency

Add to your project's `stack.yaml`:

```yaml
extra-deps:
  - git: git@github.com:haonguyen14/haskell-mcp.git
    commit: <commit-sha>
```

Then add `haskell-mcp` to your `package.yaml` dependencies.

## Protocol

- **Transport:** HTTP POST at `/mcp`, `Content-Type: application/json`
- **Version:** `2025-11-25`
