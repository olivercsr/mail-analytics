module Main where

import Server (runweb)
import Threads (runthreads)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  -- runweb 
  runthreads

