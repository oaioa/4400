{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module APICache where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           GHC.Generics
import           Servant

import Data.Time
import Data.Cache

import API

data CacheResponse = InDate | OutDate 
  deriving(Generic)
instance ToJSON CacheResponse
instance FromJSON CacheResponse

type FileCache = Cache String File

type APICache = "cacheFile" :> ReqBody '[JSON] File :> Get '[JSON] Bool
      :<|> "checkCache"            :> ReqBody '[JSON] String :> Get '[JSON] (Maybe File)

