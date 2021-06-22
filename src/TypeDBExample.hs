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

main :: IO ()
main = do 
    res <- catch 
              ((do 
                  recreateTestEnv
                  _ <- withSchemaSession (Keyspace ksTest)
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
                    withDataSession (Keyspace ksTest) $ \session-> do
                        return ()

createTestSchema :: Query DEFINE
createTestSchema = define
                 $ Lbl "full-name" `sub` attribute
                 $ value string
                 $ Lbl "nickname" `sub` attribute
                 $ value string

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
    openTx Transaction_TypeWRITE Nothing networkLatency
    query defaultQueryOpts createTestSchema
    commit

emptyCallback :: Callback Transaction_Server Transaction_Client
emptyCallback result clientCall meta receive send done = do
    print $ "callback; result: " ++ show result
    done
    return ()

testCallback :: Callback Transaction_Server Transaction_Client
testCallback result clientCall meta receive send done = do
    putStrLn $ "result: " ++ show result
    res <- receive :: IO (Either GRPCIOError (Maybe Transaction_Server))
    putStrLn $ "received: " ++ show res
    case res of
      (Right (Just (Transaction_Server (Just (Transaction_ServerServerRes _))))) ->
          return ()
      _ -> testCallback result clientCall meta receive send done
