module Connect where

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)


createConnections :: Int -> Int -> IO ()
createConnections pid m = do
    print (pid)
    print (m-1-pid)
    print (fromIntegral (4242 + pid))
    clientLoop pid pid

    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet (fromIntegral (4242 + pid)) 0)   -- listen on TCP port 4242 + pid.
    listen sock 2                              -- set a max of 2 queued connections
    serverLoop sock pid (m-1-pid)
    return ()


clientLoop :: Int -> Int -> IO ()
clientLoop _ 0 = return ()
clientLoop pid n = do
    addrInfo <- getAddrInfo Nothing (Just "127.0.0.1") (Just $ show (4242 + pid - n))
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    connect sock (addrAddress serverAddr)
    clientLoop pid (n-1)


serverLoop :: Socket -> Int -> Int -> IO ()
serverLoop _ _ 0 = return ()
serverLoop sock pid n = do
    putStrLn "klaar voor connectie"
    (conn, _) <- accept sock     -- accept a connection and handle it
    putStrLn "connected!"
    handleSock <- socketToHandle conn ReadWriteMode
    hPutStrLn handleSock "Hey, client!"
    hClose handleSock
    serverLoop sock pid (n-1)

-- start :: Int -> IO ()
-- start pid = do
--   putStrLn $ show pid
--   name <- getLine
--   return ()



-- type Msg = String
-- runConn :: Socket -> Chan Msg -> IO ()
-- runConn sock chan = do
--     let broadcast msg = writeChan chan msg
--     hdl <- socketToHandle sock ReadWriteMode
--     hSetBuffering hdl NoBuffering

--     hPutStrLn hdl "Hi, what's your name?"
--     name <- fmap init (hGetLine hdl)
--     broadcast ("--> " ++ name ++ " entered chat.")
--     hPutStrLn hdl ("Welcome, " ++ name ++ "!")

--     commLine <- dupChan chan

--     -- fork off a thread for reading from the duplicated channel
--     reader <- forkIO $ fix $ \loop -> do
--         (nextNum, line) <- readChan commLine
--         when (msgNum /= nextNum) $ hPutStrLn hdl line
--         loop

--     handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
--         line <- fmap init (hGetLine hdl)
--         case line of
--              -- If an exception is caught, send a message and break the loop
--              "quit" -> hPutStrLn hdl "Bye!"
--              -- else, continue looping.
--              _      -> broadcast (name ++ ": " ++ line) >> loop

--     killThread reader                      -- kill after the loop ends
--     broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
--     hClose hdl                             -- close the handle



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

-- runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
-- runConn (sock, _) chan msgNum = do
--     let broadcast msg = writeChan chan (msgNum, msg)
--     hdl <- socketToHandle sock ReadWriteMode
--     hSetBuffering hdl NoBuffering

--     hPutStrLn hdl "Hi, what's your name?"
--     name <- fmap init (hGetLine hdl)
--     broadcast ("--> " ++ name ++ " entered chat.")
--     hPutStrLn hdl ("Welcome, " ++ name ++ "!")

--     commLine <- dupChan chan

--     -- fork off a thread for reading from the duplicated channel
--     reader <- forkIO $ fix $ \loop -> do
--         (nextNum, line) <- readChan commLine
--         when (msgNum /= nextNum) $ hPutStrLn hdl line
--         loop

--     handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
--         line <- fmap init (hGetLine hdl)
--         case line of
--              -- If an exception is caught, send a message and break the loop
--              "quit" -> hPutStrLn hdl "Bye!"
--              -- else, continue looping.
--              _      -> broadcast (name ++ ": " ++ line) >> loop

--     killThread reader                      -- kill after the loop ends
--     broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
--     hClose hdl                             -- close the handle