{-# LANGUAGE OverloadedStrings #-}

module Tunnel (UserName,
               AccessKey,
               TunnelId,
               DomainName,
               ConnectionInfo,
               TunnelMetadata,
               Tunnel,
               mkConnectionInfo,
               getTunnel,
               deleteTunnel,
               getTunnelList) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson (decode, eitherDecode, encode, FromJSON, parseJSON, ToJSON, toJSON, Value(..), (.:), (.=), object)
import Data.ByteString.Char8 as BS (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as LBS (unpack)
import Data.Maybe (fromJust, fromMaybe)
import Network.HTTP.Conduit as C
import Network.HTTP.Types.Status (statusCode, statusMessage)
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
                                     }

instance FromJSON TunnelMetadata where
    parseJSON (Object v) = TunnelMetadata <$>
                           v .: "hostname" <*>
                           v .: "git_version" <*>
                           v .: "platform" <*>
                           v .: "release" <*>
                           v .: "nofile_limit"
    parseJSON _          = mzero

instance ToJSON TunnelMetadata where
    toJSON (TunnelMetadata hostname git_version platform release nofile_limit) =
        object [ "hostname" .= hostname
               , "git_version" .= git_version
               , "platform" .= platform
               , "release" .= release
               , "nofile_limit" .= nofile_limit
               ]

instance Show TunnelMetadata where
    show a = LBS.unpack $ encode a

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
                     }

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

instance ToJSON Tunnel where
    toJSON (Tunnel
            status
            direct_domains
            vm_version
            last_connected
            shutdown_time
            ssh_port
            launch_time
            user_shutdown
            use_caching_proxy
            creation_time
            domain_names
            shared_tunnel
            tunnel_identifier
            host
            no_proxy_caching
            owner
            use_kgp
            no_ssl_bump_domains
            id
            metadata) =
        object [ "status" .= status
               , "direct_domains" .= direct_domains
               , "vm_version" .= vm_version
               , "last_connected" .= last_connected
               , "shutdown_time" .= shutdown_time
               , "ssh_port" .= ssh_port
               , "launch_time" .= launch_time
               , "user_shutdown" .= user_shutdown
               , "use_caching_proxy" .= use_caching_proxy
               , "creation_time" .= creation_time
               , "domain_names" .= domain_names
               , "shared_tunnel" .= shared_tunnel
               , "tunnel_identifier" .= tunnel_identifier
               , "host" .= host
               , "no_proxy_caching" .= no_proxy_caching
               , "owner" .= owner
               , "use_kgp" .= use_kgp
               , "no_ssl_bump_domains" .= no_ssl_bump_domains
               , "id" .= id
               , "metadata" .= metadata
               ]

instance Show Tunnel where
    show a = LBS.unpack $ encode a

data TunnelDeleteReply = TunnelDeleteReply { jobs_running :: Int
                                           , result :: Bool
                                           , id_ :: String
                                           }

instance FromJSON TunnelDeleteReply where
    parseJSON (Object v) = TunnelDeleteReply <$>
                           v .: "jobs_running" <*>
                           v .: "result" <*>
                           v .: "id"
    parseJSON _          = mzero

instance ToJSON TunnelDeleteReply where
    toJSON (TunnelDeleteReply jobs_running result id_) =
        object [ "jobs_running" .= jobs_running
               , "result" .= result
               , "id" .= id_
               ]

instance Show TunnelDeleteReply where
    show a = LBS.unpack $ encode a

mkConnectionInfo :: UserName -> AccessKey -> IO ConnectionInfo
mkConnectionInfo user key = do
    man <- C.newManager C.tlsManagerSettings
    return $ ConnectionInfo user key man

restUrl :: String
restUrl = "https://saucelabs.com/rest/v1"

createTunnelRestUrl :: ConnectionInfo -> Maybe TunnelId -> String
createTunnelRestUrl ci Nothing = printf "%s/%s/tunnels" restUrl (userName ci)
createTunnelRestUrl ci (Just x) = printf "%s/%s/tunnels/%s" restUrl (userName ci) x

createTunnelRestRequest :: ConnectionInfo -> Maybe TunnelId -> C.Request
createTunnelRestRequest ci tid =
    applyBasicAuth user key $ fromJust $ C.parseUrl url
    where url  = createTunnelRestUrl ci tid
          user = BS.pack $ userName ci
          key  = BS.pack $ accessKey ci

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
            return $ Left (show (statusCode rstatus) ++ " " ++ BS.unpack (statusMessage rstatus))

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
            return $ Left (show (statusCode rstatus) ++ " " ++ BS.unpack (statusMessage rstatus))

deleteTunnel :: ConnectionInfo -> TunnelId -> IO (Either String TunnelDeleteReply)
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
            let reply = eitherDecode json :: Either String TunnelDeleteReply
            case reply of
                Right x -> return $ Right x
                Left e  -> return $ Left ("Invalid response " ++ e)
        _   ->
            return $ Left (show (statusCode rstatus) ++ " " ++ BS.unpack (statusMessage rstatus))
