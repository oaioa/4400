-- Main.hs, final code
module Main where

import Network.Socket
import System.IO
import System.Environment -- getArgs
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Control.Monad (liftM)
import Data.List
import System.Exit
ip =iNADDR_ANY

main :: IO ()
main = do
    return()
    args <- getArgs
    let port = head args
    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet (read port::PortNumber) iNADDR_ANY)   -- listen on TCP port 4242.
    listen sock 2                              -- set a max of 2 queued connections
    chan <- newChan
    _ <- forkIO $ fix $ \loop -> do
        (   _, _) <- readChan chan
        loop
    mainLoop sock chan 0-- pass it into the loop
    return()

type Msg = (Int, String)

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum= do
    conn <- accept sock
    let (conn2,addr2) = conn
    forkIO (runConn conn chan msgNum)
    forkFinally (runConn2 conn chan msgNum) (\_ -> do putStrLn "Killing...a";close sock;close conn2 )    
    mainLoop sock chan $! msgNum + 1


runConn2 :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn2 (sock, _) chan msgNum = do
    let broadcast msg = writeChan chan (msgNum, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    commLine <- dupChan chan

    -- fork off a thread for reading from the duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan commLine
        when (msgNum /= nextNum) $ hPutStrLn hdl line
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- fmap init (hGetLine hdl)
        case stripPrefix "HELO" line of
            Just restOfString -> hPutStrLn hdl("HELO ") -- ++restOfString++"\nIP:"++show ip++"\nPort:"++show port++"\nStudentID:17342676\n") >>loop
            Nothing -> return () 
        case stripPrefix "JOIN" line of
            Just restOfString -> hPutStrLn hdl("Joined !")>>loop
            Nothing -> return () 

        case line of
        -- If an exception is caught, send a message and break the loop
            "kill" -> hPutStrLn hdl "Bye!"
            "KILL_SERVICE" -> hPutStrLn hdl "Bye!"
            "stop" -> do hPutStrLn hdl "Bye!" 
            "quit" -> hPutStrLn hdl "Bye!"
            "exit" -> hPutStrLn hdl "Bye!"
        -- else, continue looping.
            _  -> broadcast (" wantAname !!: " ++ line) >>loop
    killThread reader                      -- kill after the loop ends
    broadcast ("<-- one left.") -- make a final broadcast
    hClose hdl       
   
runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan msgNum = do
    let broadcast msg = writeChan chan (msgNum, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    --  case stripPrefix "HELO" name of
    --        Just restOfString -> hPutStrLn hdl("HELO "++restOfString++"\nIP:"++show ip++"\nPort:"++show port++"\nStudentID:17342676\n")
    --      Nothing -> do
    --		   
    commLine <- dupChan chan

    -- fork off a thread for reading from the duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan commLine
        when (msgNum /= nextNum) $ hPutStrLn hdl line
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- fmap init (hGetLine hdl)
        case stripPrefix "HELO" line of
            Just restOfString -> hPutStrLn hdl("HELO ") -- ++restOfString++"\nIP:"++show ip++"\nPort:"++show port++"\nStudentID:17342676\n") >>loop
            Nothing -> return () 
        case stripPrefix "JOIN" line of
            Just restOfString -> hPutStrLn hdl("Joined !")>>loop
            Nothing -> return () 

        case line of
        -- If an exception is caught, send a message and break the loop
            "kill" -> hPutStrLn hdl "Bye!"
            "KILL_SERVICE" -> hPutStrLn hdl "Bye!"
            "stop" -> do hPutStrLn hdl "Bye!" 
            "quit" -> hPutStrLn hdl "Bye!"
            "exit" -> hPutStrLn hdl "Bye!"
        -- else, continue looping.
            _  -> broadcast (" wantAname !!: " ++ line) >>loop
    killThread reader                      -- kill after the loop ends
    broadcast ("<-- one left.") -- make a final broadcast
    hClose hdl                             -- close the handle
