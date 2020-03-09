module Main where

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 iNADDR_ANY)
  listen sock 2
  chan <- newChan
  _ <- forkIO $ fix $ \loop -> do
    (x,y) <- readChan chan
    print $ show y
    loop
  broadcast chan (0, "starting new game...")
  _ <- forkIO $ gameController chan 0
  mainLoop sock chan 0

type Msg = (Int, String)

gameController:: Chan Msg -> Int -> IO ()

gameController chan nPlayers = do
  print $ "total players: " <> show nPlayers
  fix $ \loop -> do
    (x, y) <- readChan chan
    when ((x == (-1)) && (y == "JOINED")) $ gameController chan (nPlayers+1)
    when ((x == (-1)) && (y == "LEFT")) $ gameController chan (nPlayers-1)
  return ()

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
  conn <- accept sock
  forkIO (runConn conn chan msgNum)
  mainLoop sock chan $! msgNum + 1

broadcast :: Chan Msg -> Msg -> IO ()
broadcast chan (n, m) = writeChan chan (n, m)

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan msgNum = do
    -- let broadcast msg = writeChan chan (msgNum, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    hPutStrLn hdl "Hi, what's your name?"
    name <- fmap init (hGetLine hdl)
    broadcast chan (msgNum, "--> " ++ name ++ " entered game.")
    broadcast chan (-1, "JOINED")
    hPutStrLn hdl ("Welcome, " ++ name ++ "!")

    commLine <- dupChan chan

    -- fork off a thread for reading from the duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan commLine
        when ((msgNum /= nextNum) && ((-1) /= nextNum)) $ hPutStrLn hdl line
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- fmap init (hGetLine hdl)
        case line of
             -- If an exception is caught, send a message and break the loop
             "quit" -> hPutStrLn hdl "Bye!"
             -- else, continue looping.
             _      -> broadcast chan (msgNum, name ++ ": " ++ line) >> loop

    killThread reader                      -- kill after the loop ends
    broadcast chan (msgNum, "<-- " ++ name ++ " left.") -- make a final broadcast
    broadcast chan (-1, "LEFT") -- make a final broadcast
    hClose hdl                             -- close the handle