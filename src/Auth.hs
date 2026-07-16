{-# LANGUAGE OverloadedStrings #-}

module Auth
  ( fetchJWKS,
    authMiddleware,
  )
where

import Control.Exception (throwIO)
import Control.Monad.Except (runExceptT)
import Crypto.JOSE.JWK (JWKSet)
import Crypto.JWT
  ( ClaimsSet,
    JWTError,
    StringOrURI,
    defaultJWTValidationSettings,
    decodeCompact,
    verifyClaims,
  )
import Data.Aeson (FromJSON (parseJSON), Result (..), Value (..), eitherDecode, fromJSON, withObject, (.:))
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (status401)
import Network.Wai

newtype ASMetadata = ASMetadata {jwksUri :: Text}

instance FromJSON ASMetadata where
  parseJSON = withObject "ASMetadata" $ \v ->
    ASMetadata <$> v .: "jwks_uri"

fetchJWKS :: Text -> IO JWKSet
fetchJWKS issuer = do
  manager <- newTlsManager
  uri <- discoverJwksUri manager issuer
  req <- HC.parseRequest (T.unpack uri)
  resp <- HC.httpLbs req manager
  case eitherDecode (HC.responseBody resp) of
    Left e -> throwIO (userError $ "Failed to parse JWKS: " <> e)
    Right raw -> case fromJSON (stripX5t raw) of
      Error e -> throwIO (userError $ "Failed to parse JWKS: " <> e)
      Success jwks -> return jwks

stripX5t :: Value -> Value
stripX5t (Object o) =
  case KM.lookup (fromText "keys") o of
    Just (Array keys) ->
      Object (KM.insert (fromText "keys") (Array (fmap dropX5t keys)) o)
    _ -> Object o
stripX5t v = v

dropX5t :: Value -> Value
dropX5t (Object o) = Object (KM.delete (fromText "x5t") o)
dropX5t v = v

discoverJwksUri :: HC.Manager -> Text -> IO Text
discoverJwksUri manager issuer = do
  let base = T.unpack (T.dropWhileEnd (== '/') issuer)
  req <- HC.parseRequest (base <> "/.well-known/oauth-authorization-server")
  resp <- HC.httpLbs req manager
  case eitherDecode (HC.responseBody resp) of
    Right meta -> return (jwksUri meta)
    Left _ -> do
      oidcReq <- HC.parseRequest (base <> "/.well-known/openid-configuration")
      oidcResp <- HC.httpLbs oidcReq manager
      case eitherDecode (HC.responseBody oidcResp) of
        Left e -> throwIO (userError $ "Failed to discover JWKS URI: " <> e)
        Right meta -> return (jwksUri meta)

authMiddleware :: JWKSet -> Text -> Middleware
authMiddleware jwks aud app req respond
  | isPublicPath (rawPathInfo req) = app req respond
  | otherwise =
      case extractBearer req of
        Nothing -> sendUnauthorized
        Just token -> do
          result <- validateToken jwks aud token
          case result of
            Left _ -> sendUnauthorized
            Right _ -> app req respond
  where
    sendUnauthorized =
      respond $
        responseLBS
          status401
          [ ( "WWW-Authenticate",
              "Bearer resource_metadata=\""
                <> encodeUtf8 aud
                <> "/.well-known/oauth-protected-resource\""
            )
          ]
          "Unauthorized"

isPublicPath :: BS.ByteString -> Bool
isPublicPath = BS.isPrefixOf "/.well-known/"

extractBearer :: Request -> Maybe BL.ByteString
extractBearer req = do
  header <- lookup "authorization" (requestHeaders req)
  BL.fromStrict <$> BS.stripPrefix "Bearer " header

validateToken :: JWKSet -> Text -> BL.ByteString -> IO (Either JWTError ClaimsSet)
validateToken jwks aud token = runExceptT $ do
  let audUri = fromString (T.unpack aud) :: StringOrURI
      settings = defaultJWTValidationSettings (== audUri)
  signedJwt <- decodeCompact token
  verifyClaims settings jwks signedJwt
