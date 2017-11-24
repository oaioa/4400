-- Main.hs, final code
module Main where

import Network.Socket
import System.IO
import System.Environment -- getArgs
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Control.Monad (liftM,replicateM)
import Data.List
import System.Exit
ip =iNADDR_ANY


data User = User {name :: String,idU::Int,hdl::Handle,channel::Chan Msg}

nUser :: String  ->Int ->Handle-> IO User
nUser name idU hdl = do
    channel <- newChan
    return User { name = name,idU=idU,hdl=hdl,channel=channel}

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
    mainLoop sock chan $! msgNum + 1


   
runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan msgNum = do
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
        case words line of
            ["KILL_SERVICE"] -> do
                hClose hdl
            ["JOIN_CHATROOM:", roomName] -> do
                remain <- replicateM 3 $ hGetLine hdl
                case map words remain of
                    [["CLIENT_IP:", _], [ "PORT:", _], ["CLIENT_NAME:", name]] -> do
                                print("JOIN ok")
                                thisUser <- nUser name msgNum hdl
                                runChat thisUser
                                loop
                    _ -> do
                        print("wrong JOIN")
                        hPutStr hdl "ERROR SYNTAX JOIN\n" 
                        loop
            ["HELO",remain] -> do
                print("HELO \n")
                hPutStrLn hdl("HELO " ++remain++
                        "\nIP:"++"10.62.0.63"++
                        "\nPort:"++"not implemented"++
                        "\nStudentID:17342676\n")
                loop

            _ ->   hPutStr hdl "JOIN to begin\n" >> loop

    killThread reader                      -- kill after the loop ends
    broadcast ("<-- one left.") -- make a final broadcast


runChat :: User -> IO ()
runChat nUser = do
    print (name nUser ++ " alone.")
    
    hPutStr (hdl nUser) $
        "JOINED_CHATROOM:0000" 
        ++ "\nSERVER_IP:10.62.0.63" ++ 
        "\nPORT:4849" ++
         "\nROOM_REF:3" ++
         "\nJOIN_ID: " ++ (show $ idU nUser) ++ "\n"

    -- fork off a thread for reading messages from client channel
    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan $ channel nUser
        when ((idU nUser) /= nextNum) $ hPutStrLn (hdl nUser) line
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- fmap init (hGetLine (hdl nUser))
        case words line of
            ["JOIN_CHATROOM:", roomName] -> do
                remain <- replicateM 3 $ hGetLine (hdl nUser)
                case map words remain of
                    [["CLIENT_IP:", _], [ "PORT:", _], ["CLIENT_NAME:", name]] -> do
                                print("JOIN ok")
                                runChat nUser-- !! same chat have to creat anothe one) 
                                loop
                    _ -> do
                        hPutStr (hdl nUser) "Try again.\n" >> loop
            _ -> do
            	return() >> loop

