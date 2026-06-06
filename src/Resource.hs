{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Resource
  ( ResourceCapability (..),
    SomeResource (..),
    ResourceMetadata (..),
    ResourceContent (..),
    ResourceReadResult (..),
    ResourceReadParams (..),
    someResourceMetadata,
    someResourceUri,
    readSomeResource,
  )
where

import Data.Aeson
  ( FromJSON (..),
    KeyValue ((.=)),
    Options (omitNothingFields),
    ToJSON (..),
    defaultOptions,
    genericParseJSON,
    genericToJSON,
    object,
  )
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)

data ResourceMetadata = ResourceMetadata
  { uri :: Text,
    name :: Text,
    title :: Maybe Text,
    description :: Maybe Text,
    mimeType :: Maybe Text
  }
  deriving (Show, Generic)

resourceOptions :: Options
resourceOptions = defaultOptions {omitNothingFields = True}

instance ToJSON ResourceMetadata where
  toJSON = genericToJSON resourceOptions

instance FromJSON ResourceMetadata where
  parseJSON = genericParseJSON resourceOptions

data ResourceContent
  = ResourceText Text (Maybe Text) Text
  | ResourceBlob Text (Maybe Text) Text
  deriving (Show)

instance ToJSON ResourceContent where
  toJSON (ResourceText u mt t) =
    object . catMaybes $
      [Just ("uri" .= u), ("mimeType" .=) <$> mt, Just ("text" .= t)]
  toJSON (ResourceBlob u mt b) =
    object . catMaybes $
      [Just ("uri" .= u), ("mimeType" .=) <$> mt, Just ("blob" .= b)]

data ResourceReadResult = ResourceReadResult
  { contents :: [ResourceContent]
  }
  deriving (Show, Generic)

instance ToJSON ResourceReadResult where
  toJSON = genericToJSON defaultOptions

data ResourceReadParams = ResourceReadParams
  { uri :: Text
  }
  deriving (Show, Generic)

instance FromJSON ResourceReadParams where
  parseJSON = genericParseJSON defaultOptions

class ResourceCapability a where
  getMetadata :: a -> ResourceMetadata
  readResource :: a -> IO ResourceReadResult

data SomeResource where
  SomeResource :: (ResourceCapability a) => a -> SomeResource

someResourceMetadata :: SomeResource -> ResourceMetadata
someResourceMetadata (SomeResource r) = getMetadata r

someResourceUri :: SomeResource -> Text
someResourceUri (SomeResource r) = case getMetadata r of
  ResourceMetadata {uri = u} -> u

readSomeResource :: SomeResource -> IO ResourceReadResult
readSomeResource (SomeResource r) = readResource r
