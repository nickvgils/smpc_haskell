module Main (main) where

import Lib
import Parser
import Connect
import System.Process
import System.IO
import System.Environment
-- import Connect

main :: IO ()
main = do
  Sample m pid <- Parser.options
  createProcessLoop (m-1) m pid
  -- print pid
  channels <- Connect.createConnections pid m
  -- start sampleeee
  -- print pid
  print (length channels)
  getLine
  return ()


createProcessLoop :: Int -> Int -> Int -> IO ()
createProcessLoop n m pid
  | n == 0 || pid > 0 = return ()
  | otherwise = do
      exPath <- getExecutablePath
      r <- createProcess (shell $ "start " ++ exPath ++ " --i " ++ show (m-n) ++ " --m " ++ show m)
      createProcessLoop (n-1) m pid