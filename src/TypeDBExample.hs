{-# LANGUAGE OverloadedStrings #-}
module Main where
import TypeDBClient

import Control.Concurrent (threadDelay, forkIO, killThread)

main :: IO ()
main = do 
    -- putStrLn "starting server"
    -- id' <- forkIO runServer
    -- threadDelay 500
    -- runClient
    -- threadDelay 500
    -- killThread id'
    --actualClient
    --runWith tryTx defaultGrakn
    --res <- runWith (createKeyspace (Keyspace "Test")) defaultGrakn
    --print res
    
    res <- runWith (withSession (Keyspace "test")
                        tryTx) defaultTypeDB
    print res
    return ()
