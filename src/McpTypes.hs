{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module McpTypes
  ( mcpClientInitialize,
    mcpServerInitialize,
    supportedVersion,
    ClientInitParams (..),
    ServerInfo (..),
    ServerCap (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    Options (fieldLabelModifier),
    ToJSON (..),
    defaultOptions,
    genericParseJSON,
    genericToJSON,
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
  )
import Data.Aeson.Key (fromText, toText)
import Data.Aeson.KeyMap (toList)
import Data.Char (toLower)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import Types (McpMethod (Initialize), Request, RequestId, Response, defaultRequest, defaultResponse)

supportedVersion :: Text
supportedVersion = "2025-11-25"

data ClientInitParams = ClientInitParams
  { protocolVersion :: Text
  }
  deriving (Show, Generic)

instance FromJSON ClientInitParams where
  parseJSON = genericParseJSON defaultOptions

data ServerCap = ServerCap {listChanged :: Maybe Bool, subscribe :: Maybe Bool}
  deriving (Show, Generic)

data ServerInfo = ServerInfo
  { serverName :: Text,
    serverTitle :: Text,
    serverVersion :: Text,
    serverDescription :: Text
  }
  deriving (Show, Generic)

data ServerInitialization = ServerInitialization
  { serverInitVersion :: Text,
    serverInitCaps :: [(Text, ServerCap)],
    serverInitInfo :: ServerInfo,
    serverInitInst :: Text
  }
  deriving (Show, Generic)

instance ToJSON ServerCap where
  toJSON cap =
    object . catMaybes $
      [ ("subscribe" .=) <$> (subscribe cap),
        ("listChanged" .=) <$> (listChanged cap)
      ]

instance FromJSON ServerCap where
  parseJSON =
    withObject
      "ServerCap"
      ( \v ->
          ServerCap
            <$> v .:? "listChanged"
            <*> v .:? "subscribe"
      )

serverInfoOptions :: Options
serverInfoOptions = defaultOptions {fieldLabelModifier = map toLower . drop 6}

instance ToJSON ServerInfo where
  toJSON = genericToJSON serverInfoOptions

instance FromJSON ServerInfo where
  parseJSON = genericParseJSON serverInfoOptions

instance ToJSON ServerInitialization where
  toJSON x =
    object
      [ "protocolVersion" .= serverInitVersion x,
        "serverInfo" .= serverInitInfo x,
        "instructions" .= serverInitInst x,
        "capabilities" .= object (fmap (\(name, val) -> fromText name .= val) (serverInitCaps x))
      ]

instance FromJSON ServerInitialization where
  parseJSON =
    withObject
      "ServerInfo"
      ( \v ->
          ServerInitialization
            <$> v .: "protocolVersion"
            <*> (v .: "capabilities" >>= parseCapabilities)
            <*> v .: "serverInfo"
            <*> v .: "instructions"
      )
    where
      parseCapabilities obj = traverse parseEntry (toList obj)
      parseEntry (key, val) = ((,) (toText key)) <$> parseJSON val

mcpClientInitialize :: RequestId -> Request
mcpClientInitialize id' = defaultRequest id' Initialize Nothing

mcpServerInitialize :: RequestId -> ServerInfo -> Text -> [(Text, ServerCap)] -> Response
mcpServerInitialize id' info inst caps =
  defaultResponse id' $
    ServerInitialization
      { serverInitVersion = supportedVersion,
        serverInitInfo = info,
        serverInitInst = inst,
        serverInitCaps = caps
      }
