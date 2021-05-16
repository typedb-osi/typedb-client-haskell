
    {-
tryTx :: GraknM IO (Either GraknError ())
tryTx =  do
    void $ graknBidiRequestE
        graknCoreTransaction
        (\clientCall meta receive send done -> do 
            res <- send $ Transaction_Client $ singleton
                            $ Transaction_Req 
                                "reqID" 
                                (fromList []) 
                                $ Just $ Transaction_ReqReqOpenReq 
                              $ Transaction_Open_Req 
                                    "randomTxID"
                                    (Enumerated $ Right Transaction_TypeREAD)
                                    Nothing
                                    networkLatency
            print "tx should be open"
            print res
            
           
          --  res <- send $ Transaction_Client $ singleton
          --                  $ Transaction_Req 
          --                      "reqID" 
          --                      (fromList []) 
          --                      $ Just $ Transaction_ReqReqCommitReq 
          --                    $ Transaction_Commit_Req 
          --  print "tx should be committed"
          --  print res
        )-}
                {-
    return $ threadDelay 1000 
    return $ print "open fired"
    void $ graknBidiRequestE
        graknTransaction
        (\clientCall meta receive send done -> do 
            res <- send $ Transaction_Req (fromList [])
              $ Just $ Transaction_ReqReqCommitReq 
              $ Transaction_Commit_Req 
            print "should be committed"
            print res
            )
    --return $ GraknM $ (liftIO @(ReaderT GraknConfig _) $ print "commit fired")
    return $ Right ()-}
    

    {-
data API_ConecptManager = GetThingType { cm_thing_type_label :: Text }
                        | GetThing { cm_thing_id :: Text }
                        | PutEntityType { cm_put_entity_type_label :: Text }
                        | PutAttributeType { cm_put_attribute_type_label :: Text }
                        | PutRelationType { cm_put_relation_type_label :: Text }-} 


data TypeDBTx   = Open { txO_sessionID :: BS.ByteString
                      , txO_type :: Transaction_Type
                      , txO_options :: Maybe Options
                      , txO_latency :: Int32 }
               | Stream
               | Commit
               | Rollback
               | QueryManager { txQ_options :: Maybe Options
                              , txQ_qm :: Maybe Query.QueryManager_ReqReq }
               | ConceptManager { txCM_cm :: Maybe Concept.ConceptManager_ReqReq }
               | LogicManager { txL_lm :: Maybe Logic.LogicManager_ReqReq }
               | GetRule { ruleName :: Text, txR_rr :: Maybe Logic.Rule_ReqReq }
               | GetType { typeName :: Text, scope :: Text, txT_rr :: Maybe Concept.Type_ReqReq }
               | GetThing { thingId :: Text, txTh_rr :: Maybe Concept.Thing_ReqReq }

    {-
toTransaction :: GraknTx -> Transaction_Req
toTransaction a = Transaction_Req "randomTxID" (fromList [])
                    $ Just $ toTx a
    where toTx :: GraknTx -> Transaction_ReqReq
          toTx (Open sess t o l) = Transaction_ReqReqOpenReq $ Transaction_Open_Req sess (Enumerated $ Right t) o l
          toTx (Stream)          = Transaction_ReqReqStreamReq $ Transaction_Stream_Req
          toTx (Commit)          = Transaction_ReqReqCommitReq $ Transaction_Commit_Req
          toTx (Rollback)        = Transaction_ReqReqRollbackReq $ Transaction_Rollback_Req
          toTx (QueryManager o q)= Transaction_ReqReqQueryManagerReq $ Query.QueryManager_Req o q 
          toTx (ConceptManager m)= Transaction_ReqReqConceptManagerReq $ Concept.ConceptManager_Req m
          toTx (LogicManager lm) = Transaction_ReqReqLogicManagerReq $ Logic.LogicManager_Req  lm
          toTx (GetRule name req)    = Transaction_ReqReqRuleReq $ Logic.Rule_Req (fromStrict name) req
          toTx (GetType name scope req)    = Transaction_ReqReqTypeReq $ Concept.Type_Req (fromStrict name) (fromStrict scope) req
          toTx (GetThing thingId req)    = Transaction_ReqReqThingReq $ Concept.Thing_Req (fromString $ unpack thingId) req

-}

    {-
class MonadIO m => GraknAdmin m where
    dbContainsKeyspace :: Text -> m Bool
    dbCreateKeyspace   :: Text -> m ()
    dbGetKeyspaces     :: Text -> m ()
    dbDeleteKeyspaces  :: Text -> m ()
    openSession :: ClientConfig -> Keyspace
                    -> m a 
                    -> Either (m Error) (m ())
    closeSession :: m a -> m ()

class (Monad m) => GraknClient m where
    run :: Query result -> m a -> m q
    compute :: Computation result -> m a -> m result

-}
