{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server
  ( initApplication,
    API,
    MCPRegistry (..),
    OAuthConfig (..),
  )
where

import Auth (authMiddleware, fetchJWKS)
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (parseJSON), KeyValue ((.=)), Result (..), ToJSON (toJSON), Value, fromJSON, object)
import Data.List (find)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import McpTypes
import Prompt (PromptGetParams (..), SomePrompt, callSomePrompt, somePromptMetadata, somePromptName)
import Resource (ResourceReadParams (..), SomeResource, readSomeResource, someResourceMetadata, someResourceUri)
import Servant
import Tool (SomeTool, ToolCallParams (..), callSomeTool, someToolMetadata, someToolName)
import Types (JsonRpcError (..), McpMethod (..), Notification, Request (..), Response (..), ResponseError (..), defaultResponse)

data McpMessage
  = McpRequest Request
  | McpNotification Notification
  deriving (Show, Generic)

instance FromJSON McpMessage where
  parseJSON x =
    (McpRequest <$> parseJSON x)
      <|> (McpNotification <$> parseJSON x)

type JSONRpc = "mcp" :> ReqBody '[JSON] McpMessage :> UVerb 'POST '[JSON] '[WithStatus 200 Value, WithStatus 202 NoContent]

type API =
  JSONRpc
    :<|> ".well-known" :> "oauth-protected-resource" :> Get '[JSON] Value

data OAuthConfig = OAuthConfig
  { issueUrl :: Text,
    audience :: Text
  }

data MCPRegistry = MCPRegistry
  { serverInfo :: ServerInfo,
    serverInstruction :: Text,
    prompts :: [SomePrompt],
    tools :: [SomeTool],
    resources :: [SomeResource],
    oauthConfig :: Maybe OAuthConfig
  }

mcpHandlers :: MCPRegistry -> Server API
mcpHandlers registry = requestHandler registry :<|> protectedResourceHandler registry

type HandlerResult = Handler (Union '[WithStatus 200 Value, WithStatus 202 NoContent])

ok :: (ToJSON a) => a -> HandlerResult
ok = respond . WithStatus @200 . toJSON

protectedResourceHandler :: MCPRegistry -> Handler Value
protectedResourceHandler registry = case oauthConfig registry of
  Just config ->
    return $
      object
        [ "resource" .= audience config,
          "authorization_servers" .= [issueUrl config]
        ]
  Nothing -> throwError err404

requestHandler :: MCPRegistry -> McpMessage -> HandlerResult
requestHandler _ (McpNotification _) = respond (WithStatus @202 NoContent)
requestHandler r (McpRequest req) = case reqMethod req of
  Initialize -> ok (initializeMethod r req)
  ToolsList -> ok (toolsListMethod r req)
  ToolsCall -> liftIO (toolsCallMethod r req) >>= ok
  ResourcesList -> ok (resourcesListMethod r req)
  ResourcesRead -> liftIO (resourcesReadMethod r req) >>= ok
  PromptsList -> ok (promptsListMethod r req)
  PromptsGet -> ok (promptsGetMethod r req)
  UnknownMethod _ ->
    ok $
      Response
        { resJsonRpc = "2.0",
          resId = reqId req,
          resResult = Left $ ResponseError MethodNotFound "Method not found"
        }

defaultServerCap :: ServerCap
defaultServerCap = ServerCap {listChanged = Nothing, subscribe = Nothing}

initializeMethod :: MCPRegistry -> Request -> Response
initializeMethod r req =
  case reqParams req of
    Nothing -> versionError "Missing params"
    Just params -> case fromJSON params of
      Error _ -> versionError "Invalid params"
      Success (ClientInitParams clientVersion)
        | clientVersion == supportedVersion -> successResponse
        | otherwise -> versionError "Unsupported protocol version"
  where
    versionError msg =
      Response
        { resJsonRpc = "2.0",
          resId = reqId req,
          resResult = Left $ ResponseError InvalidParams msg
        }
    successResponse =
      mcpServerInitialize
        (reqId req)
        (serverInfo r)
        (serverInstruction r)
        ( catMaybes
            [ if null (prompts r) then Nothing else Just ("prompts", defaultServerCap),
              if null (tools r) then Nothing else Just ("tools", defaultServerCap),
              if null (resources r) then Nothing else Just ("resources", defaultServerCap)
            ]
        )

resourcesListMethod :: MCPRegistry -> Request -> Response
resourcesListMethod r req =
  defaultResponse (reqId req) $
    object ["resources" .= map someResourceMetadata (resources r)]

resourcesReadMethod :: MCPRegistry -> Request -> IO Response
resourcesReadMethod r req =
  case reqParams req of
    Nothing -> return $ errorResponse InvalidParams "Missing params"
    Just params -> case fromJSON params of
      Error _ -> return $ errorResponse InvalidParams "Invalid params"
      Success (ResourceReadParams resourceUri) ->
        case find (\res -> someResourceUri res == resourceUri) (resources r) of
          Nothing -> return $ errorResponse MethodNotFound "Resource not found"
          Just res -> do
            result <- readSomeResource res
            return $ defaultResponse (reqId req) result
  where
    errorResponse code msg =
      Response
        { resJsonRpc = "2.0",
          resId = reqId req,
          resResult = Left $ ResponseError code msg
        }

promptsListMethod :: MCPRegistry -> Request -> Response
promptsListMethod r req =
  defaultResponse (reqId req) $
    object ["prompts" .= map somePromptMetadata (prompts r)]

promptsGetMethod :: MCPRegistry -> Request -> Response
promptsGetMethod r req =
  case reqParams req of
    Nothing -> errorResponse InvalidParams "Missing params"
    Just params -> case fromJSON params of
      Error _ -> errorResponse InvalidParams "Invalid params"
      Success (PromptGetParams promptName mArgs) ->
        case find (\p -> somePromptName p == promptName) (prompts r) of
          Nothing -> errorResponse MethodNotFound "Prompt not found"
          Just p ->
            case callSomePrompt p (fromMaybe (object []) mArgs) of
              Left e -> errorResponse InternalError (pack e)
              Right msg -> defaultResponse (reqId req) (object ["messages" .= [msg]])
  where
    errorResponse code msg =
      Response
        { resJsonRpc = "2.0",
          resId = reqId req,
          resResult = Left $ ResponseError code msg
        }

toolsListMethod :: MCPRegistry -> Request -> Response
toolsListMethod r req =
  defaultResponse (reqId req) $
    object ["tools" .= map someToolMetadata (tools r)]

toolsCallMethod :: MCPRegistry -> Request -> IO Response
toolsCallMethod r req =
  case reqParams req of
    Nothing -> return $ errorResponse InvalidParams "Missing params"
    Just params -> case fromJSON params of
      Error _ -> return $ errorResponse InvalidParams "Invalid params"
      Success (ToolCallParams callName mArgs) ->
        case find (\t -> someToolName t == callName) (tools r) of
          Nothing -> return $ errorResponse MethodNotFound "Tool not found"
          Just t -> do
            result <- callSomeTool t (fromMaybe (object []) mArgs)
            case result of
              Left e -> return $ errorResponse InternalError (pack e)
              Right tr -> return $ defaultResponse (reqId req) tr
  where
    errorResponse code msg =
      Response
        { resJsonRpc = "2.0",
          resId = reqId req,
          resResult = Left $ ResponseError code msg
        }

initApplication :: MCPRegistry -> IO Application
initApplication r = case oauthConfig r of
  Nothing ->
    return $ serve (Proxy :: Proxy API) (mcpHandlers r)
  Just cfg -> do
    jwks <- fetchJWKS (issueUrl cfg)
    return $ authMiddleware jwks (audience cfg) (serve (Proxy :: Proxy API) (mcpHandlers r))
