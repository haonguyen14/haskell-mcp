{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Data.Aeson (FromJSON (parseJSON), KeyValue ((.=)), object, withObject, (.:))
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import McpTypes
import Network.Wai.Handler.Warp (run)
import Servant (Proxy (..), serve)
import Prompt
import Resource
import Server
import Tool

-- Echo tool: returns whatever message the caller sends

data EchoTool = EchoTool

data EchoInput = EchoInput {message :: Text}
  deriving (Generic, FromJSON)

instance ToolCapability EchoTool where
  type Input EchoTool = EchoInput

  getMetadata _ =
    ToolMetadata
      { name = "echo",
        title = Just "Echo",
        description = Just "Echoes back the input message",
        inputSchema =
          object
            [ "type" .= ("object" :: Text),
              "properties" .= object ["message" .= object ["type" .= ("string" :: Text)]],
              "required" .= (["message"] :: [Text])
            ]
      }

  runTool _ (EchoInput msg) = pure $ ToolResult [ToolText msg] False

-- Add tool: adds two integers

data AddTool = AddTool

data AddInput = AddInput {a :: Int, b :: Int}
  deriving (Generic, FromJSON)

instance ToolCapability AddTool where
  type Input AddTool = AddInput

  getMetadata _ =
    ToolMetadata
      { name = "add",
        title = Just "Add",
        description = Just "Returns the sum of two integers",
        inputSchema =
          object
            [ "type" .= ("object" :: Text),
              "properties"
                .= object
                  [ "a" .= object ["type" .= ("integer" :: Text)],
                    "b" .= object ["type" .= ("integer" :: Text)]
                  ],
              "required" .= (["a", "b"] :: [Text])
            ]
      }

  runTool _ (AddInput x y) =
    let result = x + y
     in pure $ ToolResult [ToolText (mconcat [showT x, " + ", showT y, " = ", showT result])] False
    where
      showT = pack . show

-- Summarize prompt: generates a summarization prompt for a given topic

data SummarizePrompt = SummarizePrompt

data SummarizeArgs = SummarizeArgs Text

instance FromJSON SummarizeArgs where
  parseJSON = withObject "SummarizeArgs" $ \v ->
    SummarizeArgs <$> v .: "topic"

instance PromptCapability SummarizePrompt where
  type Args SummarizePrompt = SummarizeArgs

  getMetadata _ =
    PromptMetadata
      { name = "summarize",
        title = Just "Summarize",
        description = Just "Generates a summarization prompt for a given topic",
        arguments =
          Just
            [ PromptArgumentMetadata
                { name = "topic",
                  description = Just "The topic to summarize",
                  required = True
                }
            ]
      }

  runPrompt _ (SummarizeArgs t) =

    PromptMessage
      { role = User,
        content = PromptText ("Please provide a concise summary of: " <> t)
      }

-- Greeting resource: a static text resource

data GreetingResource = GreetingResource

instance ResourceCapability GreetingResource where
  getMetadata _ =
    ResourceMetadata
      { uri = "text://greeting",
        name = "greeting",
        title = Just "Greeting",
        description = Just "A friendly greeting message",
        mimeType = Just "text/plain"
      }

  readResource _ =
    pure $ ResourceReadResult
      [ ResourceText "text://greeting" (Just "text/plain") "Hello from haskell-mcp!"
      ]

registry :: MCPRegistry
registry =
  MCPRegistry
    { serverInfo =
        ServerInfo
          { serverName = "haskell-mcp",
            serverTitle = "Haskell MCP Server",
            serverVersion = "0.1.0.0",
            serverDescription = "An MCP server written in Haskell"
          },
      serverInstruction = "Send JSON-RPC requests to /mcp",
      prompts = [SomePrompt SummarizePrompt],
      tools = [SomeTool EchoTool, SomeTool AddTool],
      resources = [SomeResource GreetingResource]
    }

main :: IO ()
main = do
  let port = 8080
  putStrLn ("Starting on port " ++ show port)
  run port (serve (Proxy :: Proxy JSONRpc) (mcpServer registry))
