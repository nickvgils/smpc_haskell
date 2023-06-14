{-# LANGUAGE TypeApplications #-}


module Connect where

import Network.Socket
import Network.Socket.ByteString (recv, send)
import qualified Data.ByteString.Lazy as BL
import Data.Binary
import Data.Int
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)

type Msg = String

createConnections :: Int -> Int -> IO [(Chan Msg, Chan Msg)]
createConnections pid m = do
    channels <- channelLoop m
    print (pid)
    print (4242 + pid)

    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet (fromIntegral (4242 + pid)) 0)   -- listen on TCP port 4242 + pid.
    listen sock 2                              -- set a max of 2 queued connections
    serverLoop channels sock pid pid

    clientLoop channels pid (m-1-pid)

    return channels


serverLoop :: [(Chan Msg, Chan Msg)] -> Socket -> Int -> Int -> IO ()
serverLoop _ _ _ 0 = return ()
serverLoop chan sock pid n = do
    putStrLn "klaar voor connectie"
    (conn, _) <- accept sock     -- accept a connection and handle it
    putStrLn "connected!"
    msg <- recv conn 1024
    -- print 122220
    _ <- forkIO (runConn sock (chan !! (fromIntegral (decode (BL.fromStrict msg) :: Int16) :: Int)) )
    serverLoop chan sock pid (n-1)

clientLoop :: [(Chan Msg, Chan Msg)] -> Int -> Int -> IO ()
clientLoop _ _ 0 = return ()
clientLoop chan pid n = do
    let peer_pid = pid + n
    addrInfo <- getAddrInfo Nothing (Just "127.0.0.1") (Just $ show (4242 + peer_pid))
    print addrInfo
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    res <- try @IOException $ connect sock (addrAddress serverAddr)
    case res of
      Right () -> do  -- connection successful
        _ <- send sock $ BL.toStrict $ encode (fromIntegral pid :: Int16)
        _ <- forkIO (runConn sock (chan !! peer_pid) )
        clientLoop chan pid (n-1)
      Left _ -> do  -- exception
        threadDelay 3000000
        clientLoop chan pid n



runConn :: Socket -> (Chan Msg, Chan Msg) -> IO ()
runConn sock (inChan,outChan) = do
    return ()


channelLoop :: Int -> IO [(Chan Msg, Chan Msg)]
channelLoop 0 = return []
channelLoop n = do
    inChan <- newChan
    outChan <- newChan
    rest <- channelLoop (n-1)
    return ((inChan, outChan):rest)

