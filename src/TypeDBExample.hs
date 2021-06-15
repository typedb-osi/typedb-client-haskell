{-# LANGUAGE OverloadedStrings #-}
module Main where
import Types
import TypeDBClient
import TypeDBTransaction
import Control.Monad.Catch

--import Control.Concurrent (threadDelay, forkIO, killThread)

main :: IO ()
main = do 
    res <- catch 
              ((do 
                  recreateTest
                  res <- withSession (Keyspace ksTest)
                            (runTxDefault testTx testCallback) 
                  return $ Right res
              ) `runWith` defaultTypeDB ) 
              (\case 
                (TypeDBError e) -> return $ Left e
                (CompileError t q) -> return $ Left $ CompileError t q)
    
    print res
    return ()

    where 
        ksTest = "test"
        recreateTest :: TypeDBM IO () 
        recreateTest = do 
                    keyspaces <- getKeyspaces
                    if ksTest `elem` keyspaces
                       then deleteKeyspace $ Keyspace ksTest
                       else return ()
                    createKeyspace $ Keyspace ksTest
                    withSession (Keyspace ksTest) $ do
                        return ()


