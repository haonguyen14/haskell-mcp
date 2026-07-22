{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Server
  ( initApplication,
    API,
    MCPRegistry (..),
    OAuthConfig (..),
  )
where

import Auth (authMiddleware, fetchJWKS, userCtxKey)
import qualified Data.Vault.Lazy as VaultL
import Network.Wai (vault)
import Servant.Server.Internal (addParameterCheck, withRequest)
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (parseJSON), KeyValue ((.=)), Result (..), ToJSON (toJSON), Value, fromJSON, object)
import Data.List (find)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import System.IO (hPutStrLn, stderr)
import McpTypes
import Prompt (PromptGetParams (..), SomePrompt, callSomePrompt, somePromptMetadata, somePromptName)
import Resource (ResourceReadParams (..), SomeResource, readSomeResource, someResourceMetadata, someResourceUri)
import Servant
import Tool (SomeTool, ToolCallParams (..), callSomeTool, someToolMetadata, someToolName)
import Types (JsonRpcError (..), McpMethod (..), Notification (..), Request (..), Response (..), ResponseError (..), UserCtx, defaultResponse)

data McpMessage
  = McpRequest Request
  | McpNotification Notification
  deriving (Show, Generic)

instance FromJSON McpMessage where
  parseJSON x =
    (McpRequest <$> parseJSON x)
      <|> (McpNotification <$> parseJSON x)

data UserCtxParam

instance (HasServer api ctx) => HasServer (UserCtxParam :> api) ctx where
  type ServerT (UserCtxParam :> api) m = Maybe UserCtx -> ServerT api m
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy @api) pc nt . s
  route Proxy ctx delayed =
    route (Proxy @api) ctx $
      addParameterCheck delayed $
        withRequest $ \req ->
          return $ VaultL.lookup userCtxKey (vault req)

type JSONRpc = UserCtxParam :> "mcp" :> ReqBody '[JSON] McpMessage :> UVerb 'POST '[JSON] '[WithStatus 200 Value, WithStatus 202 NoContent]

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

requestHandler :: MCPRegistry -> Maybe UserCtx -> McpMessage -> HandlerResult
requestHandler _ _ (McpNotification n) = do
  liftIO $ hPutStrLn stderr $ "[mcp] notification method=" <> unpack (notMethod n)
  respond (WithStatus @202 NoContent)
requestHandler r ctx (McpRequest req) = do
  liftIO $ hPutStrLn stderr $ "[mcp] request method=" <> show (reqMethod req)
  case reqMethod req of
    Initialize -> ok (initializeMethod r req)
    ToolsList -> ok (toolsListMethod r req)
    ToolsCall -> liftIO (toolsCallMethod r ctx req) >>= ok
    ResourcesList -> ok (resourcesListMethod r req)
    ResourcesRead -> liftIO (resourcesReadMethod r req) >>= ok
    PromptsList -> ok (promptsListMethod r req)
    PromptsGet -> ok (promptsGetMethod r req)
    UnknownMethod m -> do
      liftIO $ hPutStrLn stderr $ "[mcp] unknown method=" <> unpack m
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

toolsCallMethod :: MCPRegistry -> Maybe UserCtx -> Request -> IO Response
toolsCallMethod r ctx req =
  case reqParams req of
    Nothing -> return $ errorResponse InvalidParams "Missing params"
    Just params -> case fromJSON params of
      Error _ -> return $ errorResponse InvalidParams "Invalid params"
      Success (ToolCallParams callName mArgs) -> do
        hPutStrLn stderr $ "[mcp] tool=" <> unpack callName
        case find (\t -> someToolName t == callName) (tools r) of
          Nothing -> do
            hPutStrLn stderr $ "[mcp] tool=" <> unpack callName <> " not_found"
            return $ errorResponse MethodNotFound "Tool not found"
          Just t -> do
            result <- callSomeTool t ctx (fromMaybe (object []) mArgs)
            case result of
              Left e -> do
                hPutStrLn stderr $ "[mcp] tool=" <> unpack callName <> " error=" <> e
                return $ errorResponse InternalError (pack e)
              Right tr -> do
                hPutStrLn stderr $ "[mcp] tool=" <> unpack callName <> " ok"
                return $ defaultResponse (reqId req) tr
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
    hPutStrLn stderr $ "[server] fetching JWKS from " <> unpack (issueUrl cfg)
    jwks <- fetchJWKS (issueUrl cfg)
    hPutStrLn stderr "[server] JWKS fetched ok"
    return $ authMiddleware jwks (audience cfg) (serve (Proxy :: Proxy API) (mcpHandlers r))
