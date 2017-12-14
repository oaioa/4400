{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib
    ( startApp
    ) where
import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import qualified Data.ByteString.Lazy         as L
import qualified Data.List                    as DL
import           Data.Maybe                   (catMaybes)
import           Data.Text                    (pack, unpack)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Database.MongoDB
import           GHC.Generics
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Servant
import qualified Servant.API                  as SC
import qualified Servant.Client               as SC
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger

import           API

import APICache
import Data.Cache as Cache

import System.Directory

startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp =  do
  putStrLn $ "Ccahe Server started"
  let settings = setPort 8080  defaultSettings
  c <-newCache Nothing :: IO FileCache
  --c <-newCache (Just $ TimeSpec 60 0 ) :: IO FileCache

  runSettings settings $ app c


app :: FileCache -> Application
app c = serve api $ server c

api :: Proxy APICache
api = Proxy


cacheFile :: FileCache -> File ->IO ()
cacheFile c f = do
                        Cache.insert c (filename f) (f)
                        putStrLn $ "inserted in cache "++(filename f)


checkCache :: FileCache -> String -> (Either String File)
checkCache c filename = do
  case Cache.lookup c filename of
        Just cached -> Right (cached)
        Nothing -> Left ("Error in the cache prossess "++filename)
  
server :: FileCache -> Server APICache
server c = cacheFile c
    :<|> checkCache c




