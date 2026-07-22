{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Tool
  ( ToolCapability (..),
    SomeTool (..),
    ToolMetadata (..),
    ToolContent (..),
    ToolResult (..),
    ToolCallParams (..),
    someToolMetadata,
    someToolName,
    callSomeTool,
  )
where

import Data.Aeson
  ( FromJSON (..),
    KeyValue ((.=)),
    Options (omitNothingFields),
    Result (..),
    ToJSON (..),
    Value,
    defaultOptions,
    fromJSON,
    genericParseJSON,
    genericToJSON,
    object,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Types (UserCtx)

data ToolMetadata = ToolMetadata
  { name :: Text,
    title :: Maybe Text,
    description :: Maybe Text,
    inputSchema :: Value
  }
  deriving (Show, Generic)

toolOptions :: Options
toolOptions = defaultOptions {omitNothingFields = True}

instance ToJSON ToolMetadata where
  toJSON = genericToJSON toolOptions

instance FromJSON ToolMetadata where
  parseJSON = genericParseJSON toolOptions

data ToolContent = ToolText Text
  deriving (Show)

instance ToJSON ToolContent where
  toJSON (ToolText t) = object ["type" .= ("text" :: Text), "text" .= t]

data ToolResult = ToolResult
  { content :: [ToolContent],
    isError :: Bool
  }
  deriving (Show, Generic)

instance ToJSON ToolResult where
  toJSON = genericToJSON defaultOptions

data ToolCallParams = ToolCallParams
  { name :: Text,
    arguments :: Maybe Value
  }
  deriving (Show, Generic)

instance FromJSON ToolCallParams where
  parseJSON = genericParseJSON defaultOptions

class (FromJSON (Input a)) => ToolCapability a where
  type Input a

  getMetadata :: a -> ToolMetadata
  runTool :: Maybe UserCtx -> a -> Input a -> IO ToolResult

data SomeTool where
  SomeTool :: (ToolCapability a) => a -> SomeTool

someToolMetadata :: SomeTool -> ToolMetadata
someToolMetadata (SomeTool t) = getMetadata t

someToolName :: SomeTool -> Text
someToolName (SomeTool t) = case getMetadata t of
  ToolMetadata {name = n} -> n

callSomeTool :: SomeTool -> Maybe UserCtx -> Value -> IO (Either String ToolResult)
callSomeTool (SomeTool t) ctx args = case fromJSON args of
  Error e -> return $ Left e
  Success input -> Right <$> runTool ctx t input
