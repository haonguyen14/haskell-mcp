{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Prompt
  ( PromptCapability (..),
    SomePrompt (..),
    PromptMetadata (..),
    PromptArgumentMetadata (..),
    PromptMessage (..),
    PromptContent (..),
    PromptGetParams (..),
    Role (..),
    somePromptMetadata,
    somePromptName,
    callSomePrompt,
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

data PromptArgumentMetadata = PromptArgumentMetadata
  { name :: Text,
    description :: Maybe Text,
    required :: Bool
  }
  deriving (Show, Generic)

data PromptMetadata = PromptMetadata
  { name :: Text,
    title :: Maybe Text,
    description :: Maybe Text,
    arguments :: Maybe [PromptArgumentMetadata]
  }
  deriving (Show, Generic)

promptOptions :: Options
promptOptions = defaultOptions {omitNothingFields = True}

instance ToJSON PromptArgumentMetadata where
  toJSON = genericToJSON promptOptions

instance ToJSON PromptMetadata where
  toJSON = genericToJSON promptOptions

data Role = User | Assistant deriving (Show)

instance ToJSON Role where
  toJSON User = toJSON ("user" :: Text)
  toJSON Assistant = toJSON ("assistant" :: Text)

data PromptContent = PromptText Text deriving (Show)

instance ToJSON PromptContent where
  toJSON (PromptText t) = object ["type" .= ("text" :: Text), "text" .= t]

data PromptMessage = PromptMessage
  { role :: Role,
    content :: PromptContent
  }
  deriving (Show, Generic)

instance ToJSON PromptMessage where
  toJSON = genericToJSON defaultOptions

data PromptGetParams = PromptGetParams
  { name :: Text,
    arguments :: Maybe Value
  }
  deriving (Show, Generic)

instance FromJSON PromptGetParams where
  parseJSON = genericParseJSON defaultOptions

class (FromJSON (Args a)) => PromptCapability a where
  type Args a

  getMetadata :: a -> PromptMetadata
  runPrompt :: a -> Args a -> PromptMessage

data SomePrompt where
  SomePrompt :: (PromptCapability a) => a -> SomePrompt

somePromptMetadata :: SomePrompt -> PromptMetadata
somePromptMetadata (SomePrompt p) = getMetadata p

somePromptName :: SomePrompt -> Text
somePromptName (SomePrompt p) = case getMetadata p of
  PromptMetadata {name = n} -> n

callSomePrompt :: SomePrompt -> Value -> Either String PromptMessage
callSomePrompt (SomePrompt p) args = case fromJSON args of
  Error e -> Left e
  Success input -> Right (runPrompt p input)
