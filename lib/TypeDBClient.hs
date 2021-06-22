{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
{-# LANGUAGE DerivingStrategies #-} 
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PartialTypeSignatures #-}
module TypeDBClient where
import Network.GRPC.HighLevel.Generated
import Proto3.Suite.Types
import Options
import CoreDatabase
import Session
import Transaction
import CoreService
import Data.Text (Text, pack)
import Data.Text.Lazy (toStrict, fromStrict)
import Data.Text.Encoding
import Data.UUID
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import UnliftIO.Async
import UnliftIO.Concurrent 
import Control.Monad.Conc.Class hiding (throw, catch, throwTo, threadDelay)
import Control.Monad.Catch
import Control.Exception.Base hiding (throwTo, catch,try)
import Control.Monad.Trans.Reader
import Network.GRPC.LowLevel.Op
import Network.GRPC.LowLevel.Call
import GHC.Exts
import GHC.Int
import Types


defaultConfig :: ClientConfig
defaultConfig = ClientConfig { clientServerHost = "localhost"
                             , clientServerPort = 1729
                             , clientArgs = []
                             , clientSSLConfig = Nothing
                             , clientAuthority = Nothing
                             }


defaultTypeDB :: TypeDBConfig 
defaultTypeDB = TypeDBConfig defaultConfig 3

data SessionType = DataSession
                 | SchemaSession
            deriving (Show,Eq)

openSessionOfType :: (MonadIO m, MonadConc m) => Keyspace -> SessionType -> TypeDBM m TypeDBSession
openSessionOfType keyspace sessiontype = openSession_ keyspace sessiontype defaultSessionOpts

openSessionDefault :: (MonadIO m, MonadConc m) => Keyspace -> TypeDBM m TypeDBSession
openSessionDefault keyspace = openSession_ keyspace DataSession defaultSessionOpts

defaultSessionOpts :: Maybe Options
defaultSessionOpts = Just $ Options { optionsInferOpt = Nothing
                               , optionsTraceInferenceOpt = Nothing
                               , optionsExplainOpt = Nothing
                               , optionsParallelOpt = Nothing
                               , optionsPrefetchSizeOpt = Nothing
                               , optionsPrefetchOpt = Nothing
                               , optionsSessionIdleTimeoutOpt = Nothing
                               , optionsSchemaLockAcquireTimeoutOpt = Nothing
                               , optionsReadAnyReplicaOpt = Nothing } 

openSession_ :: (MonadIO m, MonadConc m) => Keyspace -> SessionType -> Maybe Options -> TypeDBM m TypeDBSession
openSession_ keyspace sessiontype opts = 
    typeDBNormalRequest
        typeDBSessionOpen
        (Session_Open_Req db type' opts)
        (\sessOpenRes ->
            TypeDBSession $ session_Open_ResSessionId sessOpenRes)
    where
        db    = fromStrict $ getKeyspace keyspace
        type' = Enumerated $ Right $ 
                    (case sessiontype of 
                      DataSession -> Session_TypeDATA
                      SchemaSession -> Session_TypeSCHEMA)

pulseSession :: (MonadIO m, MonadConc m) => TypeDBSession -> TypeDBM m ()
pulseSession (TypeDBSession session) =
    typeDBNormalRequest
        typeDBSessionPulse
        (Session_Pulse_Req $ {-toASCIIBytes-} session)
        (const ())

closeSession :: (MonadIO m, MonadConc m) => TypeDBSession -> TypeDBM m ()
closeSession (TypeDBSession session) = 
    typeDBNormalRequest
        typeDBSessionClose
        (Session_Close_Req $ {-toASCIIBytes-} session)
        (const ())
                              {-

data RuntimeState = RS { rt_threads :: Map FullyQualifiedName ThreadId
                       , rt_port_cell_mapping 
                                    :: Map FullyQualifiedName CellID
                       , rt_cells   :: Map CellID [TVarID]
                       , rt_tvars   :: Map TVarID (TVar SomeType)
                       , rt_prim    :: PRIM
                       }
                                 -}


keyspaceExists :: (MonadIO m, MonadConc m) => Keyspace -> TypeDBM m Bool
keyspaceExists(Keyspace keyspace) =
    typeDBNormalRequest
        typeDBDatabasesContains 
        (CoreDatabaseManager_Contains_Req $ fromStrict keyspace) 
        coreDatabaseManager_Contains_ResContains

createKeyspace :: (MonadIO m, MonadConc m) => Keyspace -> TypeDBM m ()
createKeyspace (Keyspace keyspace) =
    typeDBNormalRequest
        typeDBDatabasesCreate
        (CoreDatabaseManager_Create_Req $ fromStrict keyspace)
        (const ())

deleteKeyspace :: (MonadIO m, MonadConc m) => Keyspace -> TypeDBM m ()
deleteKeyspace (Keyspace keyspace) =
    typeDBNormalRequest
        typeDBDatabaseDelete
        (CoreDatabase_Delete_Req $ fromStrict keyspace)
        (const ())

getKeyspaces :: (MonadIO m, MonadConc m) => TypeDBM m [Text]
getKeyspaces =
    typeDBNormalRequest
        typeDBDatabasesAll
        CoreDatabaseManager_All_Req
        (map toStrict . toList . coreDatabaseManager_All_ResNames)


networkLatency :: GHC.Int.Int32
networkLatency = 15000


                 
performTx :: (MonadIO m, MonadConc m) => [Transaction_Req] ->  Callback Transaction_Server Transaction_Client -> TypeDBM m (StatusCode, StatusDetails)
performTx tx func = do
    liftIO $ print "performing tx"
    typeDBBidiRequest
                typeDBTransaction
                (\clientCall meta receive send done -> do 
                    -- TODO: send_res, res may be error
                    print tx
                    print "sending request"
                    res <- send $ Transaction_Client $ fromList 
                            $ tx
                    --res <- unsafeFromRight <$> receive ::IO (Maybe Transaction_Server)
                    
                    func res clientCall meta receive send done
                )



type Selector req res = TypeDB ClientRequest ClientResult
                -> ClientRequest 'Normal req res
                -> IO (ClientResult 'Normal res)

typeDBNormalRequest :: MonadIO m => 
    Selector req res -> req -> (res -> a) -> TypeDBM m a
typeDBNormalRequest select req convert = TypeDBM $ do
    (TypeDBConfig config timeoutSeconds') <- ask
    liftIO $ withGRPCClient config $ \client -> do
        typeDBFunction <- select <$> typeDBClient client
        res <- typeDBFunction (ClientNormalRequest req timeoutSeconds' [])
        case res of
            ClientNormalResponse 
              result 
              _meta1 _meta2 _status _details  -> return $ convert result
            ClientErrorResponse err -> throw $ TypeDBError $ pack $ show err

typeDBNormalRequestErr :: (MonadIO m, Show ex) => 
    Selector req res -> req -> (res -> Either ex a) -> TypeDBM m a
typeDBNormalRequestErr select req convert = TypeDBM $ do
    (TypeDBConfig config timeoutSeconds') <- ask
    liftIO $ withGRPCClient config $ \client -> do
        typeDBFunction <- select <$> typeDBClient client
        res <- typeDBFunction (ClientNormalRequest req timeoutSeconds' [])
        case res of
            ClientNormalResponse 
              result 
              _meta1 _meta2 _status _details  -> 
                  case convert result of 
                    (Left  err) -> throw $ TypeDBError $ pack $ show err
                    (Right res) -> return res
            ClientErrorResponse err -> throw $ TypeDBError $ pack $ show err

typeDBNormalRequestE :: MonadIO m => 
    Selector req res -> req -> (res -> a) -> TypeDBM m (Either TypeDBError a)
typeDBNormalRequestE select req convert = TypeDBM $ do
    (TypeDBConfig config timeoutSeconds') <- ask
    liftIO $ withGRPCClient config $ \client -> do
        typeDBFunction <- select <$> typeDBClient client
        res <- typeDBFunction (ClientNormalRequest req timeoutSeconds' [])
        case res of
            ClientNormalResponse 
              result 
              _meta1 _meta2 _status _details  -> return $ Right $ convert result
            ClientErrorResponse err -> return $ Left $ TypeDBError $ pack $ show err

type BidiSelector req res = TypeDB ClientRequest ClientResult
                                -> ClientRequest 'BiDiStreaming req res
                                -> IO (ClientResult 'BiDiStreaming res)
type HandlerFunction req res = ClientCall -> MetadataMap
                        -> StreamRecv res
                        -> StreamSend req
                        -> WritesDone -> IO ()
                        
typeDBBidiRequest :: (MonadIO m, MonadConc m) => 
    BidiSelector req res -> HandlerFunction req res -> TypeDBM m (StatusCode, StatusDetails)
typeDBBidiRequest select handler = TypeDBM $ do
    (TypeDBConfig config timeoutSeconds') <- ask
    liftIO $ withGRPCClient config $ \client -> do
        typeDBFunction <- select <$> typeDBClient client
        resE <- try $ typeDBFunction 
                    (ClientBiDiRequest timeoutSeconds' [] handler) :: IO (Either SomeException (ClientResult 'BiDiStreaming _))
        case resE of
          (Right (ClientBiDiResponse _meta _status _detail))
                                    -> return $ (_status, _detail)
          (Right (ClientErrorResponse err)) -> throw $ TypeDBError $ pack $ show err
          (Left a) -> throw $ TypeDBError $ pack $ show a

unsafetypeDBBidiRequestE :: (MonadIO m, MonadConc m) => 
    BidiSelector req res -> HandlerFunction req res -> TypeDBM m (Either TypeDBError (StatusCode, StatusDetails))
unsafetypeDBBidiRequestE select handler = TypeDBM $ do
    (TypeDBConfig config timeoutSeconds') <- ask
    liftIO $ withGRPCClient config $ \client -> do
        typeDBFunction <- select <$> typeDBClient client
        res <- typeDBFunction (ClientBiDiRequest timeoutSeconds' [] handler)
        case res of
            ClientBiDiResponse _meta _status _detail
                                    -> return $ Right $ (_status, _detail)
            ClientErrorResponse err -> return $ Left $ TypeDBError $ pack $ show err




-- safer combinator-style
runWithErr :: (MonadIO m, MonadConc m) 
        => TypeDBM m a -> TypeDBConfig 
        -> (TypeDBError -> m a) -> m a
runWithErr g config errHand = (runReaderT (fromTypeDB g) config)
                                `catch` errHand

runWith :: (MonadIO m, MonadConc m, MonadCatch m) 
        => TypeDBM m a -> TypeDBConfig 
        -> m a
runWith g config = (runReaderT (fromTypeDB g) config)

data SessionTimeout = SessionTimeout
    deriving Show
instance Exception SessionTimeout


withSchemaSession :: (MonadUnliftIO m, MonadIO m, MonadConc m) 
            => Keyspace -> (TypeDBSession -> TypeDBM m a) -> TypeDBM m a
withSchemaSession keyspace m = withSession keyspace SchemaSession m

withDataSession :: (MonadUnliftIO m, MonadIO m, MonadConc m) 
            => Keyspace -> (TypeDBSession -> TypeDBM m a) -> TypeDBM m a
withDataSession keyspace m = withSession keyspace DataSession m


withSession :: (MonadUnliftIO m, MonadIO m, MonadConc m) 
            => Keyspace -> SessionType -> (TypeDBSession -> TypeDBM m a) -> TypeDBM m a
withSession keyspace sessiontype m = do
    session <- openSessionOfType keyspace sessiontype
    liftIO $ putStrLn $ "opened session: " <> (show session)

    pulseSessionThread <- forkIO $ 
                            catch (sendPulses session)
                                  (\(a::SessionTimeout) -> return ())
    res <- m session
    -- waitEitherCancel :: MonadUnliftIO m => Async a 
    --                       -> Async b 
    --                       -> m (Either a b) 
    -- so the pattern match is safe because sendPulses never finishes first
    {-res <- waitEitherCancel 
                     pulseSessionThread
                     action 
    -}
    throwTo pulseSessionThread SessionTimeout
    closeSession session
    liftIO $ putStrLn $ "closed session " <> (show session)
    liftIO $ putStrLn ""
    return res
    {-case res of
      Left _ -> throwM $ TypeDBError "connection to the server severed; session unclosed"
      Right res' -> do
        closeSession session
        liftIO $ print $ "closed session " <> (getTypeDBSession session)
        return res'
    -}
    where 
        sendPulses :: (MonadIO m, MonadConc m) => TypeDBSession -> TypeDBM m ()
        sendPulses !session = do 
            -- send pulse every 5 seconds after creation
            liftIO $ threadDelay (5*10^6) 
            liftIO $ print "pulsing"
            pulseSession session
            sendPulses session

ask' :: (Monad m) => TypeDBM m TypeDBConfig
ask' = TypeDBM $ do
            config <- ask
            return config

