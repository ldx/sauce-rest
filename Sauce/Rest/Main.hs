{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
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
    , username  = usernameFlags "list"
    , accesskey = accesskeyFlags "list"
    } &= name "tunnel-list" &= help "List tunnels"

tunnelShow = TunnelShow
    { id_       = idFlags "show"
    , username  = usernameFlags "show"
    , accesskey = accesskeyFlags "show"
    } &= name "tunnel-show" &= help "Show information about a tunnel"

tunnelDelete = TunnelDelete
    { id_       = idFlags "delete"
    , username  = usernameFlags "delete"
    , accesskey = accesskeyFlags "delete"
    } &= name "tunnel-delete" &= help "Shut down a tunnel"

mode = cmdArgsMode $ modes [tunnelShow, tunnelList, tunnelDelete] &= help "Sauce REST API client" &= program "sauce-rest" &= summary "sauce-rest v0.1.0.0"

main = do
    opts <- cmdArgsRun mode
    print opts
