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
import           RestClient
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
import System.Directory

startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting use-haskell."
  forkIO $ taskScheduler 5
  let settings = setPort 8080 $ setLogger aplogger defaultSettings
  runSettings settings app

taskScheduler :: Int -> IO ()
taskScheduler delay = do
  warnLog $ "Fork alive"
  threadDelay $ delay * 1000000
  taskScheduler delay -- tail recursion

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = loadEnvironmentVariable
    :<|> getREADME
    :<|> storeMessage
    :<|> searchMessage
    :<|> performRESTCall
    :<|> getFile
  where
    getFile :: String -> Handler File
    getFile f = do
        let path = "dd/" ++ f
        exists <- liftIO $ doesFileExist path
        if exists
            then do
                contents <- liftIO $ readFile path
                liftIO $ putStr contents
                return $ File path contents
            else return $ File "0" "File do not exist"


    loadEnvironmentVariable :: Maybe String -> Handler ResponseData
    loadEnvironmentVariable ms = liftIO $ do
      warnLog $ "request to load environment variable: " ++ show ms
      case ms of
        Nothing -> do
          warnLog "No environment variable requested"
          return $ ResponseData "WAT? No environment variable requested."
        Just s  -> liftIO $ do
          e <- lookupEnv s -- lookupEnv is an IO function, so we must use the `<-` notation
          case e of
            Nothing -> do
              warnLog $ "Environment variable " ++ s ++ " is not set."
              return $ ResponseData $  "Environment variable " ++ s ++ " is not set."

            Just e' -> return $ ResponseData e'

    -- |  One can do File IO with the standard functions:
    --                 readFile  :: FilePath -> IO String
    --                 writeFile :: FilePath -> String -> IO ()
    -- we shall set the location of the README file to return using a command line argument
    -- So our first task is to get the README file location, then open it and return its contents to the client.
    getREADME, getREADME' :: Handler ResponseData -- fns with no input, second getREADME' is for demo below
    getREADME = liftIO $ do
      [rPath] <- getArgs         -- alternatively (rPath:xs) <- getArgs
      s       <- readFile rPath
      return $ ResponseData s

    -- here is an alternative implementation of getREADME, more in keeping the Haskell style
    -- takes a bit of practice, but very easy to read, if yuo understand the symbols, and very hard to get wrong.
    -- in english, read file idenfitied by the head of the argument list and return as a ResponseData structure
    getREADME' = liftIO $ ResponseData <$> (readFile . head =<< getArgs)

    storeMessage :: Message -> Handler Bool
    storeMessage msg@(Message key _) = liftIO $ do
      warnLog $ "Storing message under key " ++ key ++ "."

      -- upsert creates a new record if the identified record does not exist, or if
      -- it does exist, it updates the record with the passed document content
      -- As you can see, this takes a single line of code
      withMongoDbConnection $ upsert (select ["name" =: key] "MESSAGE_RECORD") $ toBSON msg

      return True  -- as this is a simple demo I'm not checking anything

    searchMessage :: Maybe String -> Handler [Message]
    searchMessage (Just key) = liftIO $ do
      warnLog $ "Searching for value for key: " ++ key

      -- catMaybes is a standard filtering library function that can take a set of Maybe type (Nothing or Just something)
      -- and return an array of the contained elements. There is no magic to catMaybes - its entire implementation is as
      -- follows:
      --            catMaybes :: [Maybe a] -> [a]
      --            catMaybes ls = [x | Just x <- ls]

      withMongoDbConnection $ do
        docs <- find (select ["name" =: key] "MESSAGE_RECORD") >>= drainCursor
        --warnLog $ "retrieved data: " ++ show docs
        return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Message) docs

      -- notice the structure of the first line of code: fn1 >>= fn2
      -- An alternative way to write this is:
      --        a <- fn1
      --        b <- fn2 a
      -- but for short IO function chains, it can be easier to use >>= to chain them.
      --
      -- In fact, the code above can be compressed further, although it is a question of style as to which is
      -- preferable:
      --
      --     withMongoDbConnection $ find (select ["name" =: key] "MESSAGE_RECORD") >>= drainCursor >>=
      --                                return . catMaybes . DL.map (\ b -> fromBSON b :: Maybe ResponseData)
      --
      -- The effect of the '.' is to chain a set of functions A . B . C into a single function X, so that when one calls
      -- X p, C is called on the parameter p first, followed by B on the result, followed by A of the result of
      -- that. This is exactly equivalent in effect to a function \p ->(A (B (C p))).

    searchMessage Nothing = liftIO $ do
      warnLog $ "No key for searching."
      return $ ([] :: [Message])

    -- searchable documentation server. The API to the service is accessible at http://hackage.haskell.org
    performRESTCall :: Maybe String -> Handler ResponseData
    performRESTCall (Just filt) = liftIO $ do
      warnLog $ "recieved request to perform REST call with param " ++ filt
      doRest $ DL.filter (DL.isInfixOf filt)

    -- | An implementation when no parameter is passed, no filtering so.
    performRESTCall Nothing = liftIO $ do
      warnLog $ "recieved request to perform REST call, but no param "
      doRest id

    -- | the performRESTCall is delegated to this function, with a filtering function passed as a parameter
    doRest :: ([String] -> [String]) -> IO ResponseData
    doRest flt = do
      -- first we perform the call to hackage.org, then we will extract the package names and filter
      -- to include only package names matching the 'filt' parameter, returning a comma separated string of those
      -- packages.
      res <- SC.runClientM getPackages =<< env   -- the actual REST call
      case res of
        Left err -> do
          warnLog $ "Rest call failed with error: " ++ show err
          return $ ResponseData $ "Rest call failed with error: " ++ show err
        Right pkgs -> do
          return $ ResponseData $ DL.intercalate ", " $                          -- reduce to comma separated string
                                  flt $                                          -- run the filtering function
                                  DL.map (unpack . RestClient.packageName) pkgs  -- extract name and convert to string
      where env = do
             manager <- newManager defaultManagerSettings
             return (SC.ClientEnv manager (SC.BaseUrl SC.Http "hackage.haskell.org" 80 ""))


-- What follows next is some helper function code that makes it easier to do warious things such as use
-- a mongoDB, post console log statements define environment variables for use in your programmes and so forth.
-- The code is not written particularly to be understood by novice Haskellers, but should be useable relatively easily
-- as set out above.

-- | error stuff
custom404Error msg = err404 { errBody = msg }


-- | Logging stuff
iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%q%z"

-- global loggin functions
debugLog, warnLog, errorLog :: String -> IO ()
debugLog = doLog debugM
warnLog  = doLog warningM
errorLog = doLog errorM
noticeLog = doLog noticeM

doLog f s = getProgName >>= \ p -> do
                t <- getCurrentTime
                f p $ (iso8601 t) ++ " " ++ s

withLogging act = withStdoutLogger $ \aplogger -> do

  lname  <- getProgName
  llevel <- logLevel
  updateGlobalLogger lname
                     (setLevel $ case llevel of
                                  "WARNING" -> WARNING
                                  "ERROR"   -> ERROR
                                  _         -> DEBUG)
  act aplogger


-- | Mongodb helpers...

-- | helper to open connection to mongo database and run action
-- generally run as follows:
--        withMongoDbConnection $ do ...
--
withMongoDbConnection :: Action IO a -> IO a
withMongoDbConnection act  = do
  ip <- mongoDbIp
  port <- mongoDbPort
  database <- mongoDbDatabase
  pipe <- connect (host ip)
  ret <- runResourceT $ liftIO $ access pipe master (pack database) act
  close pipe
  return ret

-- | helper method to ensure we force extraction of all results
-- note how it is defined recursively - meaning that draincursor' calls itself.
-- the purpose is to iterate through all documents returned if the connection is
-- returning the documents in batch mode, meaning in batches of retruned results with more
-- to come on each call. The function recurses until there are no results left, building an
-- array of returned [Document]
drainCursor :: Cursor -> Action IO [Document]
drainCursor cur = drainCursor' cur []
  where
    drainCursor' cur res  = do
      batch <- nextBatch cur
      if null batch
        then return res
        else drainCursor' cur (res ++ batch)

-- | Environment variable functions, that return the environment variable if set, or
-- default values if not set.

-- | The IP address of the mongoDB database that devnostics-rest uses to store and access data
mongoDbIp :: IO String
mongoDbIp = defEnv "MONGODB_IP" id "database" True

-- | The port number of the mongoDB database that devnostics-rest uses to store and access data
mongoDbPort :: IO Integer
mongoDbPort = defEnv "MONGODB_PORT" read 27017 False -- 27017 is the default mongodb port

-- | The name of the mongoDB database that devnostics-rest uses to store and access data
mongoDbDatabase :: IO String
mongoDbDatabase = defEnv "MONGODB_DATABASE" id "USEHASKELLDB" True

-- | Determines log reporting level. Set to "DEBUG", "WARNING" or "ERROR" as preferred. Loggin is
-- provided by the hslogger library.
logLevel :: IO String
logLevel = defEnv "LOG_LEVEL" id "DEBUG" True


-- | Helper function to simplify the setting of environment variables
-- function that looks up environment variable and returns the result of running funtion fn over it
-- or if the environment variable does not exist, returns the value def. The function will optionally log a
-- warning based on Boolean tag
defEnv :: Show a
              => String        -- Environment Variable name
              -> (String -> a)  -- function to process variable string (set as 'id' if not needed)
              -> a             -- default value to use if environment variable is not set
              -> Bool          -- True if we should warn if environment variable is not set
              -> IO a
defEnv env fn def doWarn = lookupEnv env >>= \ e -> case e of
      Just s  -> return $ fn s
      Nothing -> do
        when doWarn (doLog warningM $ "Environment variable: " ++ env ++
                                      " is not set. Defaulting to " ++ (show def))
        return def




