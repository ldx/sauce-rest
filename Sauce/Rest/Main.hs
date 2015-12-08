{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Maybe (fromJust)
import System.Console.CmdArgs
import System.Environment (lookupEnv)
import Tunnel

data SauceRest = TunnelList { full      :: Bool
                            , username  :: String
                            , accesskey :: String
                            }
               | TunnelShow { id_       :: String
                            , username  :: String
                            , accesskey :: String
                            }
               | TunnelDelete { id_       :: String
                              , username  :: String
                              , accesskey :: String
                              }
               deriving (Data, Typeable, Show, Eq)

usernameFlags x = x &= help "Sauce Labs username; environment variable SAUCE_USERNAME can also be used" &= typ "USER"

accesskeyFlags x = x &= help "Sauce Labs access key; environment variable SAUCE_ACCESS_KEY can also be used" &= typ "KEY"

idFlags x = x &= typ "ID" &= argPos 0

tunnelList = TunnelList
    { full      = def &= help "Show tunnel details"
    , username  = usernameFlags "invalid list user"
    , accesskey = accesskeyFlags "invalid list key"
    } &= name "tunnel-list" &= help "List tunnels"

tunnelShow = TunnelShow
    { id_       = idFlags "invalid show id"
    , username  = usernameFlags "invalid show user"
    , accesskey = accesskeyFlags "invalid show key"
    } &= name "tunnel-show" &= help "Show information about a tunnel"

tunnelDelete = TunnelDelete
    { id_       = idFlags "invalid delete id"
    , username  = usernameFlags "invalid delete user"
    , accesskey = accesskeyFlags "invalid delete key"
    } &= name "tunnel-delete" &= help "Shut down a tunnel"

mode = cmdArgsMode $ modes [tunnelShow, tunnelList, tunnelDelete] &= help "Sauce REST API client" &= program "sauce-rest" &= summary "sauce-rest v0.1.0.0"

isInvalid :: String -> Bool
isInvalid x = "invalid " == take 8 x

getUserName :: SauceRest -> IO UserName
getUserName s = do
    case isInvalid user of
        False -> return user
        True  -> do
            x <- lookupEnv "SAUCE_USERNAME"
            return $ fromJust x
    where user = username s

getAccessKey :: SauceRest -> IO AccessKey
getAccessKey s = do
    case isInvalid key of
        False -> return key
        True  -> do
            x <- lookupEnv "SAUCE_ACCESS_KEY"
            return $ fromJust x
    where key = accesskey s

main = do
    opts <- cmdArgsRun mode
    user <- getUserName opts
    key <- getAccessKey opts
    ci <- mkConnectionInfo user key
    print opts
