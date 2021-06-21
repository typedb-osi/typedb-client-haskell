{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Main where
import Types
import TypeDBClient
import TypeDBQuery
import Transaction
import TypeDBTransaction
import Control.Monad.Catch

--import Control.Concurrent (threadDelay, forkIO, killThread)

main :: IO ()
main = do 
    res <- catch 
              ((do 
                  recreateTestEnv
                  _ <- withSession (Keyspace ksTest)
                            (runTxDefault createSchemaTx emptyCallback)
                  return $ Right ()
              ) `runWith` defaultTypeDB ) 
              (\case 
                a -> (return $ Left (a::SomeException)))
    
    print res
    return ()
    
    where 
        ksTest = "test"
        recreateTestEnv :: TypeDBM IO () 
        recreateTestEnv = do 
                    keyspaces <- getKeyspaces
                    if ksTest `elem` keyspaces
                       then deleteKeyspace $ Keyspace ksTest
                       else return ()
                    createKeyspace $ Keyspace ksTest
                    withSession (Keyspace ksTest) $ do
                        return ()

createTestSchema :: Query DEFINE
createTestSchema = define
                 $ employment `sub` relation
                 $ relates_ employer
                 $ relates_ employee

                 $ person `sub` entity 
                 $ owns "full-name"
                 $ owns "nickname"
                 $ plays_ (employment `rp` employee)
                 $ plays_ (employment `rp` employer)
                 $ε
    where
        [person,employer,employee,employment] 
          = map Lbl 
          ["person","employer","employee","employment"]
    

createSchemaTx :: TypeDBTX 
createSchemaTx = do
    openTx Transaction_TypeREAD Nothing networkLatency
    query defaultQueryOpts createTestSchema
    commit

emptyCallback :: Callback Transaction_Server Transaction_Client
emptyCallback clientCall meta receive send done = do
    print "callback"
    done
    return ()

testCallback :: Callback Transaction_Server Transaction_Client
testCallback clientCall meta receive send done = do
    res <- receive :: IO (Either GRPCIOError (Maybe Transaction_Server))
    putStrLn $ "received: " ++ show res
    case res of
      (Right (Just (Transaction_Server (Just (Transaction_ServerServerRes _))))) ->
          return ()
      _ -> testCallback clientCall meta receive send done
