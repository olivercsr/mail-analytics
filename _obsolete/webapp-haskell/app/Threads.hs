module Threads where

import Control.Concurrent

task :: IO ()
task = do
  -- putStrLn "start task"
  threadDelay 5000000
  -- putStrLn "end task"

runthreads :: IO ()
runthreads = do
  _ <- mapM forkIO [t | _ <- [1..100000],
                        let t = task]
  threadDelay 6000000
-- runthreads = forkIO task
