-- Main.hs, final code
module Main where

import Network.Socket
import System.IO
import System.Environment -- getArgs
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Control.Monad (liftM,replicateM)
-- import Control.Concurrent.STM.TVar --Shared memory locations that support atomic memory transactions
import Data.Map as Map
import Data.List
import Data.Hashable (hash)
import System.Exit
ip =iNADDR_ANY


data User = User {nameUser :: String,idU::Int,hdlU::Handle,channel::Chan Msg}

data Room = Room {nameRoom :: String, idC :: Int,users :: MVar (Map Int User)}

nUser :: String  ->Int ->Handle-> IO User
nUser name idU hdl = do
    channel <- newChan
    print("user created "++ name)
    return User { nameUser = name,idU=idU,hdlU=hdl,channel=channel}


newRoom :: String -> User -> IO Room
newRoom name user = do
    let mapUsers = Map.insert (idU user) user Map.empty
    mapUsersMutable <- newMVar mapUsers
    print("room created "++name)
    return Room {nameRoom = name, idC = (hash name), users = mapUsersMutable}

joinRoom :: User -> String -> MVar (Map Int Room) -> IO ()
joinRoom user nameR rooms = do
    roomMap <- readMVar rooms
    let room = Map.lookup (hash nameR) roomMap
    case room of
        Nothing -> do
            print("room do not exist "++nameR)
            hPutStrLn (hdlU user) ("It is coming")
            newRoom <- newRoom nameR user
            let newMap = Map.insert (idC newRoom) newRoom roomMap
            print("room map updated")
            putMVar rooms newMap
            print("MVar room updated")
            hPutStr (hdlU user) $
                "JOINED_CHATROOM: " ++(nameRoom newRoom)++
                "\nSERVER_IP: 0.0.0.0" ++ 
                "\nPORT: 0" ++
                "\nROOM_REF: " ++(show $ idC newRoom)++
                "\nJOIN_ID: " ++ (show $ idU user) ++ "\n"
        Just room -> do
            print("room do exist "++nameR)
            userMap <- readMVar (users room)
            let newMap = Map.insert (idU user) user userMap
            putMVar (users room) newMap
            hPutStr (hdlU user) $
                "JOINED_CHATROOM: " ++(nameRoom room)++
                "\nSERVER_IP: 0.0.0.0" ++ 
                "\nPORT: 0" ++
                "\nROOM_REF: " ++(show $ idC room)++
                "\nJOIN_ID: " ++ (show $ idU user) ++ "\n"

main :: IO ()
main = do
    args <- getArgs
    let port = head args
    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet (read port::PortNumber) iNADDR_ANY)   -- listen on TCP port 4242.
    listen sock 2                              -- set a max of 2 queued connections
    chan <- newChan
    
    --because of memory leak 
    _ <- forkIO $ fix $ \loop -> do
        (_,_) <- readChan chan
        loop

    mainLoop sock chan 0-- pass it into the loop
    return()

type Msg = (Int, String)

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum= do
    conn <- accept sock
    print("connection accepted : "++(show msgNum))
    rooms <- newMVar Map.empty
    forkIO (runConn conn chan msgNum rooms)
    mainLoop sock chan $! msgNum + 1


   
runConn :: (Socket, SockAddr) -> Chan Msg -> Int ->MVar (Map Int Room) ->IO ()
runConn (sock, _) chan msgNum rooms = do
    let broadcast msg = writeChan chan (msgNum, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
   
    commLine <- dupChan chan
    
    -- fork off a thread for reading from the duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan commLine
        print("reader !!!")
        when (msgNum /= nextNum) $  hPutStrLn hdl line
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- fmap init (hGetLine hdl)
        case words line of
            ["KILL_SERVICE"] -> do
                hClose hdl
            ["JOIN_CHATROOM:", roomName] -> do
                remain <- replicateM 3 $ hGetLine hdl
                case fmap words remain of
                    [["CLIENT_IP:", _], [ "PORT:", _], ["CLIENT_NAME:", name]] -> do
                                print("JOIN ok")
                                thisUser <- nUser name msgNum hdl
                                hPutStrLn (hdlU thisUser) "ready to join!"
                                joinRoom thisUser roomName rooms
                                runChat thisUser rooms
                                loop
                    _ -> do
                        print("wrong JOIN")
                        loop
            ["HELO",remain] -> do
                print("HELO \n")
                hPutStrLn hdl("HELO " ++remain++
                        "\nIP:"++"10.62.0.63"++
                        "\nPort:"++"not implemented"++
                        "\nStudentID:17342676\n")
                loop

            _ -> do 
                print(line++" from "++show msgNum)
                loop
    killThread reader                      -- kill after the loop ends
    broadcast ("<-- one left.") -- make a final broadcast


runChat :: User ->MVar (Map Int Room)-> IO ()
runChat user rooms = do
    print (nameUser user ++ " alone.")
    
    -- fork off a thread for reading messages from client channel
    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan $ channel user
        when ((idU user) /= nextNum) $ hPutStrLn (hdlU user) line
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- fmap init (hGetLine (hdlU user))
        case words line of
            ["JOIN_CHATROOM:", roomName] -> do
                remain <- replicateM 3 $ hGetLine (hdlU user)
                case fmap words remain of
                    [["CLIENT_IP:", _], [ "PORT:", _], ["CLIENT_NAME:", name]] -> do
                                print("JOIN ok")
                                joinRoom user roomName rooms 
                                loop
                    _ -> do
                        print("wrong join for user  "++ (show $ idU user)) >> loop
            _ -> do
            	return() >> loop

