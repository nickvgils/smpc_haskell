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

createConnections :: Int16 -> Int16 -> IO [(Chan Msg, Chan Msg)]
createConnections pid m = do
    channels <- channelLoop pid m
    print (pid)
    print (4242 + pid)

    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet (fromIntegral (4242 + pid)) 0)   -- listen on TCP port 4242 + pid.
    listen sock 2                              -- set a max of 2 queued connections
    serverLoop channels sock pid pid

    clientLoop channels pid (m-1-pid)


    return channels


serverLoop :: [(Chan Msg, Chan Msg)] -> Socket -> Int16 -> Int16 -> IO ()
serverLoop _ _ _ 0 = return ()
serverLoop chan sock pid n = do
    putStrLn "klaar voor connectie"
    (conn, _) <- accept sock     -- accept a connection and handle it
    putStrLn "connected!"
    msg <- recv conn 1024
    -- print 122220
    print (decode (BL.fromStrict msg) :: Int16)
    serverLoop chan sock pid (n-1)

clientLoop :: [(Chan Msg, Chan Msg)] -> Int16 -> Int16 -> IO ()
clientLoop _ _ 0 = return ()
clientLoop chan pid n = do
    addrInfo <- getAddrInfo Nothing (Just "127.0.0.1") (Just $ show (4242 + pid + n))
    print addrInfo
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    res <- try @IOException $ connect sock (addrAddress serverAddr)
    case res of
      Right () -> do  -- connection succesful
        _ <- send sock (BL.toStrict (encode pid))
        clientLoop chan pid (n-1)
      Left _ -> do  -- exception
        threadDelay 3000000
        clientLoop chan pid n



runConn :: Socket -> Chan Msg ->  Chan Msg -> IO ()
runConn sock inChan outChan = do
    return ()
    -- hdl <- socketToHandle sock ReadWriteMode
    -- hSetBuffering hdl NoBuffering

    -- hPutStrLn hdl "Hi, what's your name?"
    -- name <- fmap init (hGetLine hdl)
    -- writeChan outChan ("--> " ++ name ++ " entered chat.")
    -- hPutStrLn hdl ("Welcome, " ++ name ++ "!")

    -- -- commLine <- dupChan chan

    -- -- -- fork off a thread for reading from the duplicated channel
    -- -- reader <- forkIO $ fix $ \loop -> do
    -- --     (nextNum, line) <- readChan commLine
    -- --     when (msgNum /= nextNum) $ hPutStrLn hdl line
    -- --     loop

    -- handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
    --     line <- fmap init (hGetLine hdl)
    --     case line of
    --          -- If an exception is caught, send a message and break the loop
    --          "quit" -> hPutStrLn hdl "Bye!"
    --          -- else, continue looping.
    --          _      -> writeChan outChan (name ++ ": " ++ line) >> loop

    -- -- killThread reader                      -- kill after the loop ends
    -- -- broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
    -- hClose hdl                             -- close the handle



channelLoop :: Int16 -> Int16 -> IO [(Chan Msg, Chan Msg)]
channelLoop _ 0 = return []
channelLoop pid n = do
    inChan <- newChan
    outChan <- newChan
    rest <- channelLoop pid (n-1)
    return ((inChan, outChan):rest)


-- connect :: IO ()
-- connect 0 = return ()
-- connect n = do
--   -- connect to all parties > self.pid
--   sock <- socket AF_INET Stream 0
--   setSocketOption sock ReuseAddr 1
--   bind sock (SockAddrInet 11265 0)
--   listen sock 2
--   -- chan <- newChan
--   -- _ <- forkIO $ fix $ \loop -> do
--   --   (_, _) <- readChan chan
--   --   loop
--   mainLoop sock chan 0

-- type Msg = (Int, String)

-- mainLoop :: Socket -> Chan Msg -> Int -> IO ()
-- mainLoop sock chan msgNum = do
--   conn <- accept sock
--   forkIO (runConn conn chan msgNum)
--   mainLoop sock chan $! msgNum + 1
