{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- | Generated by Haskell protocol buffer compiler. DO NOT EDIT!
module CoreService where
import qualified Prelude as Hs
import qualified Proto3.Suite.Class as HsProtobuf
import qualified Proto3.Suite.DotProto as HsProtobuf
import qualified Proto3.Suite.JSONPB as HsJSONPB
import Proto3.Suite.JSONPB ((.=), (.:))
import qualified Proto3.Suite.Types as HsProtobuf
import qualified Proto3.Wire as HsProtobuf
import qualified Control.Applicative as Hs
import Control.Applicative ((<*>), (<|>), (<$>))
import qualified Control.DeepSeq as Hs
import qualified Control.Monad as Hs
import qualified Data.ByteString as Hs
import qualified Data.Coerce as Hs
import qualified Data.Int as Hs (Int16, Int32, Int64)
import qualified Data.List.NonEmpty as Hs (NonEmpty(..))
import qualified Data.Map as Hs (Map, mapKeysMonotonic)
import qualified Data.Proxy as Proxy
import qualified Data.String as Hs (fromString)
import qualified Data.Text.Lazy as Hs (Text)
import qualified Data.Vector as Hs (Vector)
import qualified Data.Word as Hs (Word16, Word32, Word64)
import qualified GHC.Enum as Hs
import qualified GHC.Generics as Hs
import qualified Unsafe.Coerce as Hs
import Network.GRPC.HighLevel.Generated as HsGRPC
import Network.GRPC.HighLevel.Client as HsGRPC
import Network.GRPC.HighLevel.Server as HsGRPC hiding (serverLoop)
import Network.GRPC.HighLevel.Server.Unregistered as HsGRPC
       (serverLoop)
import qualified CoreDatabase
import qualified Session
import qualified Transaction
 
data TypeDB request response = TypeDB{typeDBDatabasesContains ::
                                      request 'HsGRPC.Normal
                                        CoreDatabase.CoreDatabaseManager_Contains_Req
                                        CoreDatabase.CoreDatabaseManager_Contains_Res
                                        ->
                                        Hs.IO
                                          (response 'HsGRPC.Normal
                                             CoreDatabase.CoreDatabaseManager_Contains_Res),
                                      typeDBDatabasesCreate ::
                                      request 'HsGRPC.Normal
                                        CoreDatabase.CoreDatabaseManager_Create_Req
                                        CoreDatabase.CoreDatabaseManager_Create_Res
                                        ->
                                        Hs.IO
                                          (response 'HsGRPC.Normal
                                             CoreDatabase.CoreDatabaseManager_Create_Res),
                                      typeDBDatabasesAll ::
                                      request 'HsGRPC.Normal
                                        CoreDatabase.CoreDatabaseManager_All_Req
                                        CoreDatabase.CoreDatabaseManager_All_Res
                                        ->
                                        Hs.IO
                                          (response 'HsGRPC.Normal
                                             CoreDatabase.CoreDatabaseManager_All_Res),
                                      typeDBDatabaseSchema ::
                                      request 'HsGRPC.Normal CoreDatabase.CoreDatabase_Schema_Req
                                        CoreDatabase.CoreDatabase_Schema_Res
                                        ->
                                        Hs.IO
                                          (response 'HsGRPC.Normal
                                             CoreDatabase.CoreDatabase_Schema_Res),
                                      typeDBDatabaseDelete ::
                                      request 'HsGRPC.Normal CoreDatabase.CoreDatabase_Delete_Req
                                        CoreDatabase.CoreDatabase_Delete_Res
                                        ->
                                        Hs.IO
                                          (response 'HsGRPC.Normal
                                             CoreDatabase.CoreDatabase_Delete_Res),
                                      typeDBSessionOpen ::
                                      request 'HsGRPC.Normal Session.Session_Open_Req
                                        Session.Session_Open_Res
                                        -> Hs.IO (response 'HsGRPC.Normal Session.Session_Open_Res),
                                      typeDBSessionClose ::
                                      request 'HsGRPC.Normal Session.Session_Close_Req
                                        Session.Session_Close_Res
                                        ->
                                        Hs.IO (response 'HsGRPC.Normal Session.Session_Close_Res),
                                      typeDBSessionPulse ::
                                      request 'HsGRPC.Normal Session.Session_Pulse_Req
                                        Session.Session_Pulse_Res
                                        ->
                                        Hs.IO (response 'HsGRPC.Normal Session.Session_Pulse_Res),
                                      typeDBTransaction ::
                                      request 'HsGRPC.BiDiStreaming Transaction.Transaction_Client
                                        Transaction.Transaction_Server
                                        ->
                                        Hs.IO
                                          (response 'HsGRPC.BiDiStreaming
                                             Transaction.Transaction_Server)}
                             deriving Hs.Generic
 
typeDBServer ::
               TypeDB HsGRPC.ServerRequest HsGRPC.ServerResponse ->
                 HsGRPC.ServiceOptions -> Hs.IO ()
typeDBServer
  TypeDB{typeDBDatabasesContains = typeDBDatabasesContains,
         typeDBDatabasesCreate = typeDBDatabasesCreate,
         typeDBDatabasesAll = typeDBDatabasesAll,
         typeDBDatabaseSchema = typeDBDatabaseSchema,
         typeDBDatabaseDelete = typeDBDatabaseDelete,
         typeDBSessionOpen = typeDBSessionOpen,
         typeDBSessionClose = typeDBSessionClose,
         typeDBSessionPulse = typeDBSessionPulse,
         typeDBTransaction = typeDBTransaction}
  (ServiceOptions serverHost serverPort useCompression
     userAgentPrefix userAgentSuffix initialMetadata sslConfig logger
     serverMaxReceiveMessageLength)
  = (HsGRPC.serverLoop
       HsGRPC.defaultOptions{HsGRPC.optNormalHandlers =
                               [(HsGRPC.UnaryHandler
                                   (HsGRPC.MethodName "/typedb.protocol.TypeDB/databases_contains")
                                   (HsGRPC.convertGeneratedServerHandler typeDBDatabasesContains)),
                                (HsGRPC.UnaryHandler
                                   (HsGRPC.MethodName "/typedb.protocol.TypeDB/databases_create")
                                   (HsGRPC.convertGeneratedServerHandler typeDBDatabasesCreate)),
                                (HsGRPC.UnaryHandler
                                   (HsGRPC.MethodName "/typedb.protocol.TypeDB/databases_all")
                                   (HsGRPC.convertGeneratedServerHandler typeDBDatabasesAll)),
                                (HsGRPC.UnaryHandler
                                   (HsGRPC.MethodName "/typedb.protocol.TypeDB/database_schema")
                                   (HsGRPC.convertGeneratedServerHandler typeDBDatabaseSchema)),
                                (HsGRPC.UnaryHandler
                                   (HsGRPC.MethodName "/typedb.protocol.TypeDB/database_delete")
                                   (HsGRPC.convertGeneratedServerHandler typeDBDatabaseDelete)),
                                (HsGRPC.UnaryHandler
                                   (HsGRPC.MethodName "/typedb.protocol.TypeDB/session_open")
                                   (HsGRPC.convertGeneratedServerHandler typeDBSessionOpen)),
                                (HsGRPC.UnaryHandler
                                   (HsGRPC.MethodName "/typedb.protocol.TypeDB/session_close")
                                   (HsGRPC.convertGeneratedServerHandler typeDBSessionClose)),
                                (HsGRPC.UnaryHandler
                                   (HsGRPC.MethodName "/typedb.protocol.TypeDB/session_pulse")
                                   (HsGRPC.convertGeneratedServerHandler typeDBSessionPulse))],
                             HsGRPC.optClientStreamHandlers = [],
                             HsGRPC.optServerStreamHandlers = [],
                             HsGRPC.optBiDiStreamHandlers =
                               [(HsGRPC.BiDiStreamHandler
                                   (HsGRPC.MethodName "/typedb.protocol.TypeDB/transaction")
                                   (HsGRPC.convertGeneratedServerRWHandler typeDBTransaction))],
                             optServerHost = serverHost, optServerPort = serverPort,
                             optUseCompression = useCompression,
                             optUserAgentPrefix = userAgentPrefix,
                             optUserAgentSuffix = userAgentSuffix,
                             optInitialMetadata = initialMetadata, optSSLConfig = sslConfig,
                             optLogger = logger,
                             optMaxReceiveMessageLength = serverMaxReceiveMessageLength})
 
typeDBClient ::
               HsGRPC.Client ->
                 Hs.IO (TypeDB HsGRPC.ClientRequest HsGRPC.ClientResult)
typeDBClient client
  = (Hs.pure TypeDB) <*>
      ((Hs.pure (HsGRPC.clientRequest client)) <*>
         (HsGRPC.clientRegisterMethod client
            (HsGRPC.MethodName "/typedb.protocol.TypeDB/databases_contains")))
      <*>
      ((Hs.pure (HsGRPC.clientRequest client)) <*>
         (HsGRPC.clientRegisterMethod client
            (HsGRPC.MethodName "/typedb.protocol.TypeDB/databases_create")))
      <*>
      ((Hs.pure (HsGRPC.clientRequest client)) <*>
         (HsGRPC.clientRegisterMethod client
            (HsGRPC.MethodName "/typedb.protocol.TypeDB/databases_all")))
      <*>
      ((Hs.pure (HsGRPC.clientRequest client)) <*>
         (HsGRPC.clientRegisterMethod client
            (HsGRPC.MethodName "/typedb.protocol.TypeDB/database_schema")))
      <*>
      ((Hs.pure (HsGRPC.clientRequest client)) <*>
         (HsGRPC.clientRegisterMethod client
            (HsGRPC.MethodName "/typedb.protocol.TypeDB/database_delete")))
      <*>
      ((Hs.pure (HsGRPC.clientRequest client)) <*>
         (HsGRPC.clientRegisterMethod client
            (HsGRPC.MethodName "/typedb.protocol.TypeDB/session_open")))
      <*>
      ((Hs.pure (HsGRPC.clientRequest client)) <*>
         (HsGRPC.clientRegisterMethod client
            (HsGRPC.MethodName "/typedb.protocol.TypeDB/session_close")))
      <*>
      ((Hs.pure (HsGRPC.clientRequest client)) <*>
         (HsGRPC.clientRegisterMethod client
            (HsGRPC.MethodName "/typedb.protocol.TypeDB/session_pulse")))
      <*>
      ((Hs.pure (HsGRPC.clientRequest client)) <*>
         (HsGRPC.clientRegisterMethod client
            (HsGRPC.MethodName "/typedb.protocol.TypeDB/transaction")))