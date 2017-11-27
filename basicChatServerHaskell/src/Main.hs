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


type Msg = (Int, String)
data User = User {nameUser :: String,idU::Int,hdlU::Handle,channel::Chan Msg}

data Room = Room {nameRoom :: String, idC :: Int,users :: MVar (Map Int User)}

nUser :: String  ->Int ->Handle->Chan Msg -> IO User
nUser name idU hdl chan= do
    chanUser <- newChan
    print("user created "++ name)
    return User { nameUser = name,idU=idU,hdlU=hdl,channel=chanUser}


newRoom :: String -> User -> IO Room
newRoom name user = do
    let mapUsers = Map.insert (idU user) user Map.empty
    mapUsersMutable <- newMVar mapUsers
    print("room created "++name)
    return Room {nameRoom = name, idC = (hash name::Int), users = mapUsersMutable}

joinRoom :: User -> String -> MVar (Map Int Room) -> IO ()
joinRoom user nameR rooms = do
    roomMap <- readMVar rooms
    let room = Map.lookup (hash nameR::Int) roomMap
    case room of
        Nothing -> do
            print("JOIN!! : room do not exist "++nameR)
            newRoom <- newRoom nameR user
            let newMap = Map.insert (idC newRoom) newRoom roomMap
            takeMVar rooms --take before put otherwise it is blocking because full
            putMVar rooms newMap

            let newMap = Map.insert (idU user) user Map.empty
            putMVar (users newRoom) newMap

            hPutStr (hdlU user) $
                "JOINED_CHATROOM: " ++(nameRoom newRoom)++
                "\nSERVER_IP: 0.0.0.0" ++ 
                "\nPORT: 0" ++
                "\nROOM_REF: " ++(show $ idC newRoom)++
                "\nJOIN_ID: " ++ (show $ idU user) ++ "\n"
            messageUser user ("HELLO I am "++(nameUser user)) (hash nameR::Int) rooms
        Just room -> do
            print("JOIN : room do exist "++nameR)
            userMap <- readMVar (users room)
            let newMap = Map.insert (idU user) user userMap
            takeMVar (users room)
            putMVar (users room) newMap
            userMap <- readMVar (users room)
            print("Users :  ")
            print $ fmap (\u -> nameUser u) (Map.elems userMap)

            hPutStr (hdlU user) $
                "JOINED_CHATROOM: " ++(nameRoom room)++
                "\nSERVER_IP: 0.0.0.0" ++ 
                "\nPORT: 0" ++
                "\nROOM_REF: " ++(show $ idC room)++
                "\nJOIN_ID: " ++ (show $ idU user) ++ "\n"
            messageUser user ("HELLO I am "++(nameUser user)) (idC room) rooms
            
leaveRoom :: User -> Int -> MVar (Map Int Room) -> IO()
leaveRoom user idRoom rooms = do
    roomMap <- readMVar rooms
    let room = Map.lookup idRoom roomMap
    case room of
        Nothing -> do
            print("LEAVE : room do not exist ")
        Just room -> do
            print("LEAVE : room do exist ")
            let nameroom = nameRoom room 
            -- map of users not used for now
            hPutStr (hdlU user) $
                "LEFT_CHATROOM: " ++(show idRoom)++
                "\nJOIN_ID: " ++ (show $ idU user) ++ "\n"
            messageUser user ("I am leaving : "++(nameUser user)) (idC room) rooms
            userMap <- readMVar (users room)
            let newuserMap = Map.delete (idU user) userMap
            takeMVar (users room)  
            putMVar (users room) newuserMap

messageUser::User -> String ->Int -> MVar (Map Int Room)  -> IO ()
messageUser user message idRoom rooms=do
    roomsMap <- readMVar rooms
    let room = Map.lookup idRoom roomsMap
    case room of
        Nothing -> do
            print("SEND :room do not exist "++(show idRoom))
        Just room -> do
            print("SEND : room do exist "++(show idRoom))
            userMap <- readMVar (users room)
            let line = "CHAT: " ++ (show idRoom) ++ 
                            "\nCLIENT_NAME: " ++ (nameUser user) ++ 
                            "\nMESSAGE: " ++ message ++ "\n\n"

            mapM (\u -> writeChan (channel u )(idU user,line ) ) (Map.elems userMap) 
            mapM (\u -> print $ nameUser u ) (Map.elems userMap)
            return()

main :: IO ()
main = do
    print("Welcome")
    args <- getArgs
    let port = head args
    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet (read port::PortNumber) iNADDR_ANY)   -- listen on TCP port 4242.
    listen sock 2                              -- set a max of 2 queued connections
    chan <- newChan
    
    rooms <- newMVar Map.empty

    --because of memory leak 
    _ <- forkIO $ fix $ \loop -> do
        (_,_) <- readChan chan
        loop

    mainLoop sock chan 0 rooms-- pass it into the loop
    return()


mainLoop :: Socket -> Chan Msg -> Int -> MVar (Map Int Room) -> IO ()
mainLoop sock chan msgNum rooms = do
    conn <- accept sock
    print("|| connection accepted : "++(show msgNum))
    forkIO (runConn conn chan msgNum rooms)
    mainLoop sock chan ( msgNum + 1) rooms


   
runConn :: (Socket, SockAddr) -> Chan Msg -> Int ->MVar (Map Int Room) ->IO ()
runConn (sock, _) chan msgNum rooms = do
    let broadcast msg = writeChan chan (msgNum, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
   
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- hGetLine hdl
        print("LINE conn: " ++ line)
        case words line of
            ["KILL_SERVICE"] -> do
                return()
            ["JOIN_CHATROOM:", roomName] -> do
                remain <- replicateM 3 $ hGetLine hdl
                case fmap words remain of
                    [["CLIENT_IP:", _], [ "PORT:", _], ["CLIENT_NAME:", name]] -> do
                                print("JOIN:  ok")
                                thisUser <- nUser name msgNum hdl chan
                                joinRoom thisUser roomName rooms
                                runChat thisUser rooms
                               -- loop GET OUT of this loop
                    _ -> do
                        print("JOIN: wrong")
                        loop
            ["HELO",remain] -> do
                print("HELO \n")
                hPutStrLn hdl("HELO " ++remain++
                        "\nIP:"++"10.62.0.63"++
                        "\nPort:"++"not implemented"++
                        "\nStudentID:17342676\n")
                loop

            _ -> do 
                loop
    --killThread reader                      -- kill after the loop ends

runChat :: User ->MVar (Map Int Room)-> IO ()
runChat user rooms = do
    -- fork off a thread for reading messages from client channel
    commLine <- dupChan (channel user)
    -- fork off a thread for reading from the duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan commLine
        print("|| chat reader !!!")
        when(((idU user) /= nextNum)) $  hPutStrLn (hdlU user) line
        loop
   
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <-  (hGetLine (hdlU user))
        print("LINE chat : "++line)
        case words line of
            ["KILL_SERVICE"] -> do return()
            ["kill"] -> do 
                    print("kill !")
                    return()
            ["JOIN_CHATROOM:", roomName] -> do
                remain <- replicateM 3 $ hGetLine (hdlU user)
                case fmap words remain of
                    [["CLIENT_IP:", _], [ "PORT:", _], ["CLIENT_NAME:", name]] -> do
                                print("JOIN ok")
                                joinRoom user roomName rooms 
                                loop
                    _ -> do
                                print("JOIN : wrong for user  "++ (show $ idU user)) >> loop
            ["LEAVE_CHATROOM:", roomRef] -> do
                remain <- replicateM 2 $ hGetLine (hdlU user)
                case fmap words remain of
                    [["JOIN_ID:", cId], ["CLIENT_NAME:", name]] -> do
                                print("LEAVE ok")
                                leaveRoom user (read roomRef::Int) rooms
                                loop
                    _ -> do
                                print("wrong leave for user  "++ (show $ idU user)) >> loop
            ["DISCONNECT:", _] -> do
                remain <- replicateM 2 $ hGetLine (hdlU user)
                case fmap words remain of
                    [["PORT:", _], ["CLIENT_NAME:", name]] -> do
                            print("DISCONNECT ok")
                            -- !! leave all the rooms to be implemented
                    _ -> do
                            print("wrong disconnect for user  "++ (show $ idU user)) >> loop   
            ["CHAT:", roomRef] -> do
                remain <- replicateM 4 $ hGetLine (hdlU user)
                case fmap words remain of
                    [["JOIN_ID:", cId], ["CLIENT_NAME:", name], ("MESSAGE:":msg), []] -> do
                                print("CHAT : ok " ++ (show msg))
                                messageUser user (unwords msg) (read roomRef::Int) rooms
                                loop
                    _ -> do
                                print("wrong chat for user  "++ (show $ idU user)) >> loop

        
            _ -> do
                return() >> loop
    killThread reader
    hClose (hdlU user)
