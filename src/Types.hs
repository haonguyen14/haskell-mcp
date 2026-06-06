{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( RequestId,
    Request (..),
    McpMethod (..),
    JsonRpcError (..),
    Response (..),
    ResponseError (..),
    Notification (..),
    defaultRequest,
    defaultResponse,
  )
where

import Data.Aeson
  ( FromJSON (..),
    KeyValue ((.=)),
    Options (fieldLabelModifier),
    ToJSON (..),
    Value (..),
    defaultOptions,
    genericParseJSON,
    genericToJSON,
    object,
    withObject,
    (.:),
    (.:?),
  )
import Data.Char (toLower)
import Data.Text (Text)
import GHC.Generics (Generic)

data RequestId = IdNum Integer | IdStr Text
  deriving (Show, Generic)

instance ToJSON RequestId where
  toJSON (IdNum n) = toJSON n
  toJSON (IdStr s) = toJSON s

instance FromJSON RequestId where
  parseJSON (Number n) = pure $ IdNum (round n)
  parseJSON (String s) = pure $ IdStr s
  parseJSON _ = fail "RequestId must be a number or string"

data McpMethod
  = Initialize
  | ToolsList
  | ToolsCall
  | ResourcesList
  | ResourcesRead
  | PromptsList
  | PromptsGet
  | UnknownMethod Text
  deriving (Eq, Show)

instance ToJSON McpMethod where
  toJSON Initialize        = toJSON ("initialize" :: Text)
  toJSON ToolsList         = toJSON ("tools/list" :: Text)
  toJSON ToolsCall         = toJSON ("tools/call" :: Text)
  toJSON ResourcesList     = toJSON ("resources/list" :: Text)
  toJSON ResourcesRead     = toJSON ("resources/read" :: Text)
  toJSON PromptsList       = toJSON ("prompts/list" :: Text)
  toJSON PromptsGet        = toJSON ("prompts/get" :: Text)
  toJSON (UnknownMethod m) = toJSON m

instance FromJSON McpMethod where
  parseJSON (String "initialize")      = pure Initialize
  parseJSON (String "tools/list")      = pure ToolsList
  parseJSON (String "tools/call")      = pure ToolsCall
  parseJSON (String "resources/list")  = pure ResourcesList
  parseJSON (String "resources/read")  = pure ResourcesRead
  parseJSON (String "prompts/list")    = pure PromptsList
  parseJSON (String "prompts/get")     = pure PromptsGet
  parseJSON (String m)                 = pure $ UnknownMethod m
  parseJSON _                          = fail "method must be a string"

data Request = Request
  { reqJsonRpc :: Text,
    reqMethod :: McpMethod,
    reqId :: RequestId,
    reqParams :: Maybe Value
  }
  deriving (Show, Generic)

instance ToJSON Request where
  toJSON = genericToJSON jsonOptions

instance FromJSON Request where
  parseJSON = genericParseJSON jsonOptions

data Notification = Notification
  { notJsonRpc :: Text,
    notMethod :: Text,
    notParams :: Maybe Value
  }
  deriving (Show, Generic)

instance ToJSON Notification where
  toJSON = genericToJSON jsonOptions

instance FromJSON Notification where
  parseJSON = genericParseJSON jsonOptions

data Response = Response
  { resJsonRpc :: Text,
    resId :: RequestId,
    resResult :: Either ResponseError Value
  }
  deriving (Show, Generic)

data JsonRpcError
  = MethodNotFound
  | InvalidParams
  | InternalError
  | CustomError Integer
  deriving (Show, Eq)

instance ToJSON JsonRpcError where
  toJSON MethodNotFound   = toJSON (-32601 :: Integer)
  toJSON InvalidParams    = toJSON (-32602 :: Integer)
  toJSON InternalError    = toJSON (-32603 :: Integer)
  toJSON (CustomError n)  = toJSON n

instance FromJSON JsonRpcError where
  parseJSON (Number n) = pure $ case round n of
    -32601 -> MethodNotFound
    -32602 -> InvalidParams
    -32603 -> InternalError
    c      -> CustomError c
  parseJSON _ = fail "error code must be a number"

data ResponseError = ResponseError
  { errCode :: JsonRpcError,
    errMessage :: Text
  }
  deriving (Show, Generic)

instance ToJSON ResponseError where
  toJSON = genericToJSON jsonOptions

instance FromJSON ResponseError where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Response where
  toJSON r =
    object
      [ "jsonrpc" .= resJsonRpc r,
        "id" .= resId r,
        toResult (resResult r)
      ]
    where
      toResult (Left err) = "error" .= toJSON err
      toResult (Right val) = "result" .= toJSON val

instance FromJSON Response where
  parseJSON = withObject "Response" $ \v -> do
    jsonRpc <- v .: "jsonrpc"
    id' <- v .: "id"

    maybeErr <- v .:? "error"
    maybeResult <- v .:? "result"

    case (maybeErr, maybeResult) of
      (Just err, Nothing) -> return $ Response {resJsonRpc = jsonRpc, resId = id', resResult = Left err}
      (Nothing, Just val) -> return $ Response {resJsonRpc = jsonRpc, resId = id', resResult = Right val}
      _ -> fail "Response must contain either 'error' or 'result' field"

labelModifier :: [Char] -> [Char]
labelModifier = map toLower . drop 3

jsonOptions :: Options
jsonOptions = defaultOptions {fieldLabelModifier = labelModifier}

defaultRequest :: RequestId -> McpMethod -> Maybe Value -> Request
defaultRequest id' method params =
  Request
    { reqJsonRpc = "2.0",
      reqMethod = method,
      reqId = id',
      reqParams = params
    }

defaultResponse :: (ToJSON v) => RequestId -> v -> Response
defaultResponse id' result =
  Response
    { resJsonRpc = "2.0",
      resId = id',
      resResult = Right (toJSON result)
    }
