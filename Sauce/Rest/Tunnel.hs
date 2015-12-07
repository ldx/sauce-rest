{-# LANGUAGE OverloadedStrings #-}

module Tunnel (UserName,
               AccessKey,
               TunnelId,
               DomainName,
               ConnectionInfo,
               TunnelMetadata,
               Tunnel,
               getTunnel,
               deleteTunnel,
               getTunnelList) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson (decode, eitherDecode, FromJSON, parseJSON, Value(..), (.:))
import Data.ByteString.Char8 (pack, unpack)
import Data.Maybe (fromJust)
import Network.HTTP.Conduit as C
import Network.HTTP.Types.Status (statusCode, statusMessage)
import System.Environment (lookupEnv)
import Text.Printf (printf)

type UserName = String
type AccessKey = String
type TunnelId = String
type DomainName = String

data ConnectionInfo = ConnectionInfo { userName    :: UserName
                                     , accessKey   :: AccessKey
                                     , connManager :: C.Manager
                                     }

data TunnelMetadata = TunnelMetadata { hostname :: String
                                     , git_version :: String
                                     , platform :: String
                                     , release :: String
                                     , nofile_limit :: Int
                                     } deriving Show

instance FromJSON TunnelMetadata where
    parseJSON (Object v) = TunnelMetadata <$>
                           v .: "hostname" <*>
                           v .: "git_version" <*>
                           v .: "platform" <*>
                           v .: "release" <*>
                           v .: "nofile_limit"
    parseJSON _          = mzero

data Tunnel = Tunnel { status :: String
                     , direct_domains :: Maybe [DomainName]
                     , vm_version :: Maybe String
                     , last_connected :: Maybe Int
                     , shutdown_time :: Maybe Int
                     , ssh_port :: Maybe Int
                     , launch_time :: Maybe Int
                     , user_shutdown :: Maybe Int
                     , use_caching_proxy :: Maybe Int
                     , creation_time :: Int
                     , domain_names :: Maybe [DomainName]
                     , shared_tunnel :: Bool
                     , tunnel_identifier :: Maybe String
                     , host :: Maybe DomainName
                     , no_proxy_caching :: Maybe Bool
                     , owner :: String
                     , use_kgp :: Bool
                     , no_ssl_bump_domains :: Maybe [DomainName]
                     , id :: TunnelId
                     , metadata :: Maybe TunnelMetadata
                     } deriving Show

instance FromJSON Tunnel where
    parseJSON (Object v) = Tunnel <$>
                           v .: "status" <*>
                           v .: "direct_domains" <*>
                           v .: "vm_version" <*>
                           v .: "last_connected" <*>
                           v .: "shutdown_time" <*>
                           v .: "ssh_port" <*>
                           v .: "launch_time" <*>
                           v .: "user_shutdown" <*>
                           v .: "use_caching_proxy" <*>
                           v .: "creation_time" <*>
                           v .: "domain_names" <*>
                           v .: "shared_tunnel" <*>
                           v .: "tunnel_identifier" <*>
                           v .: "host" <*>
                           v .: "no_proxy_caching" <*>
                           v .: "owner" <*>
                           v .: "use_kgp" <*>
                           v .: "no_ssl_bump_domains" <*>
                           v .: "id" <*>
                           v .: "metadata"
    parseJSON _          = mzero

restUrl :: String
restUrl = "https://saucelabs.com/rest/v1"

createTunnelRestUrl :: ConnectionInfo -> Maybe TunnelId -> String
createTunnelRestUrl ci Nothing = printf "%s/%s/tunnels" restUrl (userName ci)
createTunnelRestUrl ci (Just x) = printf "%s/%s/tunnels/%s" restUrl (userName ci) x

createTunnelRestRequest :: ConnectionInfo -> Maybe TunnelId -> C.Request
createTunnelRestRequest ci tid =
    applyBasicAuth user key $ fromJust $ C.parseUrl url
    where url  = createTunnelRestUrl ci tid
          user = pack $ userName ci
          key  = pack $ accessKey ci

getTunnel :: ConnectionInfo -> TunnelId -> IO (Either String Tunnel)
getTunnel ci tid = do
    let req = createTunnelRestRequest ci (Just tid)
    let request = req { checkStatus = \_ _ _ -> Nothing }
    res <- C.httpLbs request (connManager ci)
    let rstatus = C.responseStatus res
    case statusCode rstatus of
        200 -> do
            let json = C.responseBody res
            let tunnel = eitherDecode json :: Either String Tunnel
            case tunnel of
                Right x  -> return $ Right x
                Left e -> return $ Left ("Invalid response " ++ e)
        _   ->
            return $ Left (show (statusCode rstatus) ++ " " ++ unpack (statusMessage rstatus))

getTunnelList :: ConnectionInfo -> IO (Either String [TunnelId])
getTunnelList ci = do
    let req = createTunnelRestRequest ci Nothing
    let request = req { checkStatus = \_ _ _ -> Nothing }
    res <- C.httpLbs request (connManager ci)
    let rstatus = C.responseStatus res
    case statusCode rstatus of
        200 -> do
            let json = C.responseBody res
            let tunnels = decode json :: Maybe [String]
            case tunnels of
                Just x  -> return $ Right x
                Nothing -> return $ Left ("Invalid response " ++ show json)
        _   ->
            return $ Left (show (statusCode rstatus) ++ " " ++ unpack (statusMessage rstatus))

deleteTunnel :: ConnectionInfo -> TunnelId -> IO (Either String ())
deleteTunnel ci tid = do
    let req = createTunnelRestRequest ci (Just tid)
    let request = req { checkStatus = \_ _ _ -> Nothing
                      , method = "DELETE"
                      }
    res <- C.httpLbs request (connManager ci)
    let rstatus = C.responseStatus res
    case statusCode rstatus of
        200 -> do
            let json = C.responseBody res
            print json
            return $ Right ()
        _   ->
            return $ Left (show (statusCode rstatus) ++ " " ++ unpack (statusMessage rstatus))

getUserName :: IO UserName
getUserName = do
    x <- lookupEnv "SAUCE_USERNAME"
    return $ fromJust x

getAccessKey :: IO AccessKey
getAccessKey = do
    x <- lookupEnv "SAUCE_ACCESS_KEY"
    return $ fromJust x

main :: IO ()
main = do
    user <- getUserName
    key <- getAccessKey
    man <- C.newManager C.tlsManagerSettings
    let ci = ConnectionInfo {userName=user, accessKey=key, connManager=man}
    tunnels <- getTunnelList ci
    case tunnels of
        Left x -> print x
        Right ts -> mapM_ (\x -> getTunnel ci x >>= print) ts
