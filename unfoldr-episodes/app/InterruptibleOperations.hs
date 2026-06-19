module Main where 

import Control.Concurrent hiding (withMVar)
import Control.Exception
import Control.Monad
import System.Environment

import Debug

{-------------------------------------------------------------------------------
  Interruptibility of takeMVar

  Key take-away: never mask exceptions indefinitely.
-------------------------------------------------------------------------------}

example0_threadA :: MVar Int -> IO ()
example0_threadA v = do 
    i <- takeMVar v 
    printLogMsg i 

example0_threadB :: MVar Int -> IO ()
example0_threadB v = do 
    threadDelay 6_000_000
    putMVar v 1 

example0 :: IO ()
example0 = do 
    v <- newEmptyMVar
    _ <- forkDebug "a" $ example0_threadA v 
    _ <- forkDebug "b" $ example0_threadB v 
    threadDelay 8_000_000

-- takeMVar is interruptible here because it is currently blocking, 
example1 :: IO ()
example1 = do
    v <- newEmptyMVar
    a <- forkDebug "a" $ example0_threadA v
    _ <- forkDebug "b" $ example0_threadB v
    threadDelay 4_000_000 >> killThread a 
    threadDelay 8_000_000


main :: IO ()
main = do 
    -- withTimer example0 
    withTimer example1 
