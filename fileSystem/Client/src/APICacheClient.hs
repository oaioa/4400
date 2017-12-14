{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module APICacheClient where

import           Data.Proxy
import           Servant.API
import           Servant.Client
import           APICache

import API

restAPI :: Proxy APICache
restAPI = Proxy


cacheFile :: String -> ClientM boolean
checkCache :: String -> ClientM (Maybe File)

(cacheFile :<|> checkCache ) = client restAPI
