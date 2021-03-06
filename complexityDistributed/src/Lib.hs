
{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE TemplateHaskell #-}
--{-# CPP #-}

-- | use-haskell
-- The purpose of this project is to provide a baseline demonstration of the use of cloudhaskell in the context of the
-- code complexity measurement individual programming project. The cloud haskell platform provides an elegant set of
-- features that support the construction of a wide variety of multi-node distributed systems commuinication
-- architectures. A simple message passing abstraction forms the basis of all communication.
--
-- This project provides a command line switch for starting the application in master or worker mode. It is implemented
-- using the work-pushing pattern described in http://www.well-typed.com/blog/71/. Comments below describe how it
-- operates. A docker-compose.yml file is provided that supports the launching of a master and set of workers.

module Lib
    ( someFunc
    ) where

-- These imports are required for Cloud Haskell
import           Control.Distributed.Process
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Distributed.Process.Closure
import           Control.Distributed.Process.Node                   (initRemoteTable)
import           Control.Monad
import           Network.Transport.TCP                              (createTransport,
                                                                     defaultTCPParameters)
import           BashCommand

import           System.Environment                                 (getArgs)
import           System.Exit
import           Data.Char

import           System.Console.ANSI
redCode   = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Red]
resetCode = setSGRCode [Reset]

-- | worker function.
-- This is the function that is called to launch a worker. It loops forever, asking for work, reading its message queue
-- and sending the result of runnning numPrimeFactors on the message content (an integer).
worker :: ( ProcessId  -- The processid of the manager (where we send the results of our work)
         , ProcessId
         , String)
       -> Process ()
worker (manager, workQueue,url) = do
    us <- getSelfPid              -- get our process identifier
    let workerNumber = (digitToInt $ (show us)!!18)*10 + (digitToInt $ (show us)!!19)
    liftIO $ putStrLn $ "Starting worker: " ++ show us
    liftIO $ cloneGitRepo url workerNumber
    go us workerNumber
  where
    go :: ProcessId -> Int-> Process ()
    go us workerNumber= do

      send workQueue us -- Ask the queue for work. Note that we send out process id so that a message can be sent to us

      -- Wait for work to arrive. We will either be sent a message with an integer value to use as input for processing,
      -- or else we will be sent (). If there is work, do it, otherwise terminate
      receiveWait
        [ match $ \(n)  -> do
            liftIO $ putStrLn $ "[Node " ++ (show us) ++ "] given work: " ++ show n
            liftIO $ resetMaster workerNumber
            liftIO $ putStrLn $ "Reset !"
            resultRest <- liftIO $ resetToPrevCommit n workerNumber
            if (resultRest /=0 )
                    then do
                            complexity <-liftIO $  computeComplexity workerNumber
                            liftIO $ putStrLn $ "The commit "++(show n)++" complexity "++(show complexity)
                            liftIO $ putStrLn $ "[Node " ++ (show us) ++ "] finished work."
                            send manager complexity
                            go us workerNumber -- note the recursion this function is called again!
                    else do
                            send manager (0 :: Integer)
                            go us workerNumber -- note the recursion this function is called again!

        , match $ \ () -> do
            liftIO $ putStrLn $ "Terminating node: " ++ show us
            return ()
        ]

remotable ['worker] -- this makes the worker function executable on a remote node

manager :: String    -- The git repository
        -> [NodeId]   -- The set of cloud haskell nodes we will initalise as workers
        -> Process Integer
manager url workers = do
  us <- getSelfPid
  liftIO $ putStrLn $ "Starting manager: " ++ show us
  -- first, we create a thread that generates the work tasks in response to workers
  -- requesting work.
  output <- liftIO $ cloneGitRepo  url 1
  liftIO $ putStrLn $ output

  taskNumber <- liftIO $ totalCommits 1
  liftIO $ putStrLn $ ("Total Commits: " ++ (show taskNumber))
  
  workQueue <- spawnLocal $ do
    -- Return the next bit of work to be done
    forM_ [1 .. (taskNumber-1)] $ \m -> do
      pid <- expect   -- await a message from a free worker asking for work
      liftIO $ putStrLn $ "Receive from worker : "++(show pid)
      send pid m     -- send them work

    -- Once all the work is done tell the workers to terminate. We do this by sending every worker who sends a message
    -- to us a null content: () . We do this only after we have distributed all the work in the forM_ loop above. Note
    -- the indentiation - this is part of the workQueue do block.
    forever $ do
      pid <- expect
      send pid ()

  -- Next, start worker processes on the given cloud haskell nodes. These will start
  -- asking for work from the workQueue thread immediately.
  forM_ workers $ \nid -> spawn nid ($(mkClosure 'worker) (us, workQueue,url))
  liftIO $ putStrLn $ "[Manager] Workers spawned"
  -- wait for all the results from the workers and return the sum total. Look at the implementation, whcih is not simply
  -- summing integer values, but instead is expecting results from workers.
  finalFunction (fromIntegral (taskNumber-1))

-- note how this function works: initialised with n, the number range we started the program with, it calls itself
-- recursively, decrementing the integer passed until it finally returns the accumulated value in go:acc. Thus, it will
-- be called n times, consuming n messages from the message queue, corresponding to the n messages sent by workers to
-- the manager message queue.
finalFunction :: Int -> Process Integer
finalFunction = go 0
  where
    go :: Integer -> Int -> Process Integer
    go !acc 0 = return acc
    go !acc n = do
      m <- expect
      go (acc + m) (n - 1)

rtable :: RemoteTable
rtable = Lib.__remoteTable initRemoteTable

-- | This is the entrypoint for the program. We deal with program arguments and launch up the cloud haskell code from
someFunc = do


  args <- getArgs

  case args of
    ["manager", host, port, url] -> do
      putStrLn "Starting Node as Manager"
      backend <- initializeBackend host port rtable
      startMaster backend $ \workers -> do
        result <- manager url workers
        liftIO $ putStrLn $ "\n"++redCode++"[[ FINAL result : "++(show result)++" ]]"++resetCode
    ["worker", host, port] -> do
      putStrLn "Starting Node as Worker"
      backend <- initializeBackend host port rtable
      startSlave backend
    _ -> putStrLn "Bad parameters"


  -- create a cloudhaskell node, which must be initialised with a network transport
  -- Right transport <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  -- node <- newLocalNode transport initRemoteTable

  -- runProcess node $ do
  --   us <- getSelfNode
  --   _ <- spawnLocal $ sampleTask (1 :: Int, "using spawnLocal")
  --   pid <- spawn us $ $(mkClosure 'sampleTask) (1 :: Int, "using spawn")
  --   liftIO $ threadDelay 2000000
