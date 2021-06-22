{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module TypeDBTransaction (module GRPC, module TypeDBTransaction) where
import Prelude 
import Network.GRPC.HighLevel.Generated as GRPC
import Proto3.Suite.Types
import Options
import qualified Query
import qualified Logic
import qualified Concept
import Transaction
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.UUID.V4
import Data.UUID hiding (fromString)
--import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Internal.Lazy
import Types
import TypeDBClient (performTx)
import GHC.Exts (fromList, fromString)
import GHC.Int (Int32)

import Polysemy
import Polysemy.State
import Polysemy.Error
import Polysemy.Internal (send)
import System.Random
import Control.Monad.IO.Class
import Control.Monad.Conc.Class hiding (catch,throw) 
import qualified Control.Monad.Conc.Class
import Control.Exception (Exception(..))
import TypeDBQuery

data Sth a
type RandomTxIdGen = StdGen

data TX m a where
    Commit :: TX m ()                                             -- done 
    Rollback :: TX m ()                                           -- done
    Open :: Transaction_Type -> Maybe Options -> Int32 -> TX m () -- done
    Stream :: TX m ()                                             -- done
    QueryManager :: Maybe Options -> Maybe Query.QueryManager_ReqReq -> TX m () -- done
    ConceptManager :: Maybe Concept.ConceptManager_ReqReq -> TX m () -- done
    LogicManager :: Maybe Logic.LogicManager_ReqReq -> TX m ()    -- done
    Rule :: RuleLabel -> Maybe Logic.Rule_ReqReq -> TX m ()           -- done
    TX_Type :: TypeLabel -> TypeScope -> Maybe Concept.Type_ReqReq -> TX m () -- done
    TX_Thing :: ThingID -> Maybe Concept.Thing_ReqReq -> TX m ()  -- done


--makeSem ''TX
  
commit :: Sem (TX ': a) ()
commit = send Commit

getMore :: Member (TX) a => Sem a ()
getMore = send Stream


openTx :: Member (TX) a => Transaction_Type -> Maybe Options -> Int32 -> Sem a ()
openTx t o i = send (Open t o i)

rollback :: Member (TX) a => Sem a ()
rollback = send Rollback

deleteThing :: Member (TX) a => ThingID -> Sem a ()
deleteThing t = send $ TX_Thing t $ Just 
              $ Concept.Thing_ReqReqThingDeleteReq
              $ Concept.Thing_Delete_Req



thingHas :: Member (TX) a => ThingID  -> [ThingType] -> KeysOnly -> Sem a ()
thingHas t thingTypes keysOnly = send $ TX_Thing t $ Just
           $ Concept.Thing_ReqReqThingGetHasReq
           $ Concept.Thing_GetHas_Req 
                (fromList $ map toConceptThingType thingTypes) 
                (keysOnly==KeysOnly)

toConceptThingType :: ThingType -> Concept.Type
toConceptThingType (ThingType 
                        (TypeLabel tl) 
                        (TypeScope ts) 
                        te
                        vt
                        isRoot)
    = Concept.Type (toInternalLazyText  tl) 
                   (toInternalLazyText ts) 
                   (Enumerated $ Right te) 
                   (Enumerated $ Right vt) 
                   (isRoot==RootType)





toConceptThing :: Thing -> Concept.Thing
toConceptThing (Thing (ThingID tid)
                      thingType'
                      thingVal
                      isInferred)
    = Concept.Thing (encodeUtf8 tid)
                    (toConceptThingType <$> thingType')
                    (toConceptThingVal <$> thingVal)
                    (isInferred==IsInferred)

thingSet :: Member (TX) a => ThingID -> Thing -> Sem a ()
thingSet tid thing = send $ TX_Thing tid $ Just
           $ Concept.Thing_ReqReqThingSetHasReq
           $ Concept.Thing_SetHas_Req 
                (Just $ toConceptThing thing)

thingUnset :: Member (TX) a => ThingID -> Thing -> Sem a ()
thingUnset tid thing = send $ TX_Thing tid $ Just
           $ Concept.Thing_ReqReqThingUnsetHasReq
           $ Concept.Thing_UnsetHas_Req 
                (Just $ toConceptThing thing)

thingGetRelations :: Member (TX) a => ThingID -> [ThingType] -> Sem a ()
thingGetRelations tid things = send $ TX_Thing tid $ Just
           $ Concept.Thing_ReqReqThingGetRelationsReq
           $ Concept.Thing_GetRelations_Req
                (fromList $ map toConceptThingType things)

thingGetPlaying :: Member (TX) a => ThingID -> Sem a ()
thingGetPlaying tid = send $ TX_Thing tid $ Just
           $ Concept.Thing_ReqReqThingGetPlayingReq
           $ Concept.Thing_GetPlaying_Req 


addPlayer :: Member (TX) a => ThingID -> Maybe ThingType -> Maybe Thing -> Sem a ()
addPlayer tid thingType' thing = send $ TX_Thing tid $ Just
            $ Concept.Thing_ReqReqRelationAddPlayerReq
            $ Concept.Relation_AddPlayer_Req 
                (toConceptThingType <$> thingType')
                (toConceptThing <$> thing)

removePlayer :: Member (TX) a => ThingID -> Maybe ThingType -> Maybe Thing -> Sem a ()
removePlayer tid thingType' thing = send $ TX_Thing tid $ Just
            $ Concept.Thing_ReqReqRelationRemovePlayerReq
            $ Concept.Relation_RemovePlayer_Req 
                (toConceptThingType <$> thingType')
                (toConceptThing <$> thing)


getPlayers :: Member (TX) a => ThingID -> [ThingType] -> Sem a ()
getPlayers tid roleTypes = send $ TX_Thing tid $ Just
           $ Concept.Thing_ReqReqRelationGetPlayersReq
           $ Concept.Relation_GetPlayers_Req
                    (fromList $ map toConceptThingType roleTypes)

getPlayersByRoleType :: Member (TX) a => ThingID -> Sem a ()
getPlayersByRoleType tid = send $ TX_Thing tid $ Just
           $ Concept.Thing_ReqReqRelationGetPlayersByRoleTypeReq
           $ Concept.Relation_GetPlayersByRoleType_Req

getRelating :: Member (TX) a => ThingID -> Sem a ()
getRelating tid = send $ TX_Thing tid $ Just
           $ Concept.Thing_ReqReqRelationGetRelatingReq
           $ Concept.Relation_GetRelating_Req

type AttributeOwnersFilter = ThingType

getAttributeOwners :: Member (TX) a => ThingID -> Maybe AttributeOwnersFilter -> Sem a ()
getAttributeOwners tid filter' = send $ TX_Thing tid $ Just
           $ Concept.Thing_ReqReqAttributeGetOwnersReq
           $ Concept.Attribute_GetOwners_Req
           $ Concept.Attribute_GetOwners_ReqFilterThingType 
                    <$> toConceptThingType <$> filter'

deleteType :: Member (TX) a => TypeLabel -> TypeScope -> Sem a ()
deleteType tl ts = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqTypeDeleteReq
           $ Concept.Type_Delete_Req


setTypeLabel :: Member (TX) a => TypeLabel -> TypeScope -> TypeLabel -> Sem a ()
setTypeLabel tl ts newLabel = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqTypeSetLabelReq
           $ Concept.Type_SetLabel_Req 
                (toConceptTypeLabel newLabel)

toConceptTypeLabel :: TypeLabel -> Data.Text.Internal.Lazy.Text
toConceptTypeLabel = toInternalLazyText . fromTypeLabel


isAbstract :: Member (TX) a => TypeLabel -> TypeScope -> Sem a ()
isAbstract tl ts = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqTypeIsAbstractReq
           $ Concept.Type_IsAbstract_Req

getSupertype :: Member (TX) a => TypeLabel -> TypeScope -> Sem a ()
getSupertype tl ts = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqTypeGetSupertypesReq
           $ Concept.Type_GetSupertypes_Req

getSupertypes :: Member (TX) a => TypeLabel -> TypeScope -> Sem a ()
getSupertypes tl ts = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqTypeGetSupertypeReq
           $ Concept.Type_GetSupertype_Req

setSupertype :: Member (TX) a => TypeLabel -> TypeScope -> Maybe ThingType -> Sem a ()
setSupertype tl ts mt = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqTypeSetSupertypeReq
           $ Concept.Type_SetSupertype_Req
                (toConceptThingType <$> mt)


getRelationTypes :: Member (TX) a => TypeLabel -> TypeScope -> Sem a ()
getRelationTypes tl ts = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqRoleTypeGetRelationTypesReq
           $ Concept.RoleType_GetRelationTypes_Req

getRoleTypePlayers :: Member (TX) a => TypeLabel -> TypeScope -> Sem a ()
getRoleTypePlayers tl ts = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqRoleTypeGetPlayersReq
           $ Concept.RoleType_GetPlayers_Req

getInstances :: Member (TX) a => TypeLabel -> TypeScope -> Sem a ()
getInstances tl ts = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqThingTypeGetInstancesReq
           $ Concept.ThingType_GetInstances_Req

setAbstract :: Member (TX) a => TypeLabel -> TypeScope -> Sem a ()
setAbstract tl ts = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqThingTypeSetAbstractReq
           $ Concept.ThingType_SetAbstract_Req

unsetAbstract :: Member (TX) a => TypeLabel -> TypeScope -> Sem a ()
unsetAbstract tl ts = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqThingTypeUnsetAbstractReq
           $ Concept.ThingType_UnsetAbstract_Req

getOwns :: Member (TX) a => TypeLabel -> TypeScope -> Maybe AttributeValueType -> KeysOnly -> Sem a ()
getOwns tl ts avt keysOnly = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqThingTypeGetOwnsReq
           $ Concept.ThingType_GetOwns_Req
                ((Concept.ThingType_GetOwns_ReqFilterValueType . Enumerated . Right . toConceptAttributeValueType) <$> avt)
                (keysOnly == KeysOnly)


toConceptAttributeValueType :: AttributeValueType -> Concept.AttributeType_ValueType
toConceptAttributeValueType AVT_Object   = Concept.AttributeType_ValueTypeOBJECT
toConceptAttributeValueType AVT_Boolean  = Concept.AttributeType_ValueTypeBOOLEAN
toConceptAttributeValueType AVT_Long     = Concept.AttributeType_ValueTypeLONG
toConceptAttributeValueType AVT_Double   = Concept.AttributeType_ValueTypeDOUBLE
toConceptAttributeValueType AVT_String   = Concept.AttributeType_ValueTypeSTRING
toConceptAttributeValueType AVT_DateTime = Concept.AttributeType_ValueTypeDATETIME

toConceptAttributeValue :: AttributeValue -> Concept.Attribute_Value
toConceptAttributeValue (AV_String t)   = Concept.Attribute_Value $ Just $ Concept.Attribute_ValueValueString $ toInternalLazyText t
toConceptAttributeValue (AV_Boolean b)  = Concept.Attribute_Value $ Just $ Concept.Attribute_ValueValueBoolean b
toConceptAttributeValue (AV_Long l)     = Concept.Attribute_Value $ Just $ Concept.Attribute_ValueValueLong l
toConceptAttributeValue (AV_Double d)   = Concept.Attribute_Value $ Just $ Concept.Attribute_ValueValueDouble d
toConceptAttributeValue (AV_DateTime d) = Concept.Attribute_Value $ Just $ Concept.Attribute_ValueValueDateTime d


setOwns :: Member (TX) a => TypeLabel -> TypeScope -> Maybe ThingType -> Maybe ThingType -> KeysOnly -> Sem a ()
setOwns tl ts avt t keysOnly = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqThingTypeSetOwnsReq
           $ Concept.ThingType_SetOwns_Req
                (toConceptThingType <$> avt)
                (Concept.ThingType_SetOwns_ReqOverriddenOverriddenType 
                    . toConceptThingType <$> t) 
                (keysOnly == KeysOnly)

unsetOwns :: Member (TX) a => TypeLabel -> TypeScope -> Maybe ThingType -> Sem a ()
unsetOwns tl ts tt  = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqThingTypeUnsetOwnsReq
           $ Concept.ThingType_UnsetOwns_Req
                (toConceptThingType <$> tt)


getPlays :: Member (TX) a => TypeLabel -> TypeScope -> Sem a ()
getPlays tl ts = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqThingTypeGetPlaysReq
           $ Concept.ThingType_GetPlays_Req


setPlays :: Member (TX) a => TypeLabel -> TypeScope -> Maybe ThingType -> Maybe ThingType -> Sem a ()
setPlays tl ts mt mo = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqThingTypeSetPlaysReq
           $ Concept.ThingType_SetPlays_Req
                (toConceptThingType <$> mt)
                ((Concept.ThingType_SetPlays_ReqOverriddenOverriddenRole 
                    . toConceptThingType) <$> mo)


unsetPlays :: Member (TX) a => TypeLabel -> TypeScope -> Maybe ThingType -> Sem a ()
unsetPlays tl ts mt = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqThingTypeUnsetPlaysReq
           $ Concept.ThingType_UnsetPlays_Req
                (toConceptThingType <$> mt)


createEntityType :: Member (TX) a => TypeLabel -> TypeScope -> Sem a ()
createEntityType tl ts = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqEntityTypeCreateReq
           $ Concept.EntityType_Create_Req



createRelationType :: Member (TX) a => TypeLabel -> TypeScope -> Sem a ()
createRelationType tl ts = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqRelationTypeCreateReq
           $ Concept.RelationType_Create_Req


getRelatesFor :: Member (TX) a => TypeLabel -> TypeScope -> RoleLabel -> Sem a ()
getRelatesFor tl ts rl = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqRelationTypeGetRelatesForRoleLabelReq
           $ Concept.RelationType_GetRelatesForRoleLabel_Req
                (toInternalLazyText . fromRoleLabel $ rl)

getRelates :: Member (TX) a => TypeLabel -> TypeScope -> Sem a ()
getRelates tl ts = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqRelationTypeGetRelatesReq
           $ Concept.RelationType_GetRelates_Req


setRelates :: Member (TX) a => TypeLabel -> TypeScope -> RelationLabel -> Maybe RelationLabel -> Sem a ()
setRelates tl ts rl mrl = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqRelationTypeSetRelatesReq
           $ Concept.RelationType_SetRelates_Req
                (toInternalLazyText . fromRelationLabel $ rl)
                (Concept.RelationType_SetRelates_ReqOverriddenOverriddenLabel 
                   . toInternalLazyText . fromRelationLabel <$> mrl)

unsetRelates :: Member (TX) a => TypeLabel -> TypeScope -> RelationLabel -> Sem a ()
unsetRelates tl ts rl = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqRelationTypeUnsetRelatesReq
           $ Concept.RelationType_UnsetRelates_Req
                (toInternalLazyText . fromRelationLabel $ rl)


putAttributeType :: Member (TX) a => TypeLabel -> TypeScope -> Maybe AttributeValue -> Sem a ()
putAttributeType tl ts mav = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqAttributeTypePutReq
           $ Concept.AttributeType_Put_Req
                (toConceptAttributeValue <$> mav)

getAttributeType :: Member (TX) a => TypeLabel -> TypeScope -> Maybe AttributeValue -> Sem a ()
getAttributeType tl ts mav = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqAttributeTypeGetReq
           $ Concept.AttributeType_Get_Req
                (toConceptAttributeValue <$> mav)
                
getAttributeTypeRegex :: Member (TX) a => TypeLabel -> TypeScope -> Sem a ()
getAttributeTypeRegex tl ts = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqAttributeTypeGetRegexReq
           $ Concept.AttributeType_GetRegex_Req


setAttributeTypeRegex :: Member (TX) a => TypeLabel -> TypeScope -> Sem a ()
setAttributeTypeRegex tl ts = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqAttributeTypeGetRegexReq
           $ Concept.AttributeType_GetRegex_Req


getAttributeTypeOwners :: Member (TX) a => TypeLabel -> TypeScope -> KeysOnly -> Sem a ()
getAttributeTypeOwners tl ts keysOnly = send $ TX_Type tl ts $ Just
           $ Concept.Type_ReqReqAttributeTypeGetOwnersReq
           $ Concept.AttributeType_GetOwners_Req
                (keysOnly == KeysOnly)


getThingType :: Member (TX) a => TypeLabel -> Sem a ()
getThingType tl = send $ ConceptManager $ Just 
            $ Concept.ConceptManager_ReqReqGetThingTypeReq
            $ Concept.ConceptManager_GetThingType_Req
                (toConceptTypeLabel tl)


getThing :: Member (TX) a => ThingID -> Sem a ()
getThing tid = send $ ConceptManager $ Just 
            $ Concept.ConceptManager_ReqReqGetThingReq
            $ Concept.ConceptManager_GetThing_Req
                (toConceptThingID tid)

toConceptThingID :: ThingID -> BS.ByteString
toConceptThingID = encodeUtf8 . fromThingID


conceptPutEntityType :: Member (TX) a => TypeLabel -> Sem a ()
conceptPutEntityType tid = send $ ConceptManager $ Just 
            $ Concept.ConceptManager_ReqReqPutEntityTypeReq
            $ Concept.ConceptManager_PutEntityType_Req
                (toConceptTypeLabel tid)

conceptPutAttributeType :: Member (TX) a => TypeLabel -> AttributeValueType -> Sem a ()
conceptPutAttributeType tid avt = send $ ConceptManager $ Just 
            $ Concept.ConceptManager_ReqReqPutAttributeTypeReq
            $ Concept.ConceptManager_PutAttributeType_Req
                (toConceptTypeLabel tid)
                (Enumerated . Right $ toConceptAttributeValueType avt)

conceptPutRelationType :: Member (TX) a => TypeLabel -> Sem a ()
conceptPutRelationType tid = send $ ConceptManager $ Just 
            $ Concept.ConceptManager_ReqReqPutRelationTypeReq
            $ Concept.ConceptManager_PutRelationType_Req
                (toConceptTypeLabel tid)




toLogicRuleLabel :: RuleLabel -> Data.Text.Internal.Lazy.Text
toLogicRuleLabel = toInternalLazyText . fromRuleLabel

deleteRule :: Member (TX) a => RuleLabel -> Sem a ()
deleteRule rl = send $ Rule rl $ Just
            $ Logic.Rule_ReqReqRuleDeleteReq
            $ Logic.Rule_Delete_Req

setRuleLabel :: Member (TX) a => RuleLabel -> RuleLabel -> Sem a ()
setRuleLabel rl lbl = send $ Rule rl $ Just
            $ Logic.Rule_ReqReqRuleSetLabelReq
            $ Logic.Rule_SetLabel_Req
                (toLogicRuleLabel lbl)

getRule :: Member (TX) a => RuleLabel -> Sem a ()
getRule lbl = send $ LogicManager $ Just
            $ Logic.LogicManager_ReqReqGetRuleReq
            $ Logic.LogicManager_GetRule_Req
                (toLogicRuleLabel lbl)

putRule :: Member (TX) a => RuleLabel -> When -> Then -> Sem a ()
putRule lbl w t = send $ LogicManager $ Just
            $ Logic.LogicManager_ReqReqPutRuleReq
            $ Logic.LogicManager_PutRule_Req
                (toLogicRuleLabel lbl)
                (toLogicWhen w)
                (toLogicThen t)

toLogicWhen :: When -> Data.Text.Internal.Lazy.Text
toLogicWhen = toInternalLazyText . fromWhen

toLogicThen :: Then -> Data.Text.Internal.Lazy.Text
toLogicThen = toInternalLazyText . fromThen

getRules :: Member (TX) a => Sem a ()
getRules = send $ LogicManager $ Just
            $ Logic.LogicManager_ReqReqGetRulesReq
            $ Logic.LogicManager_GetRules_Req


query :: (Member (Error QueryError) b) => Maybe Options -> Query a -> Sem (TX ': b) ()
query opts q = case (getQueryType q) of
            [QT_DEFINE] -> sendQuery
                                $ Query.QueryManager_ReqReqDefineReq
                                $ Query.QueryManager_Define_Req 
                                $ cq
            [QT_UNDEFINE] -> sendQuery
                                $ Query.QueryManager_ReqReqUndefineReq
                                $ Query.QueryManager_Undefine_Req
                                $ cq
            [QT_GET] -> sendQuery
                                $ Query.QueryManager_ReqReqMatchReq
                                $ Query.QueryManager_Match_Req
                                $ cq
            [QT_AGGREGATE] -> sendQuery
                                $ Query.QueryManager_ReqReqMatchAggregateReq
                                $ Query.QueryManager_MatchAggregate_Req
                                $ cq
            [QT_GET,QT_GROUP] -> sendQuery
                                $ Query.QueryManager_ReqReqMatchGroupReq
                                $ Query.QueryManager_MatchGroup_Req
                                $ cq
            [QT_GET,QT_GROUP,QT_AGGREGATE] -> sendQuery
                                $ Query.QueryManager_ReqReqMatchGroupAggregateReq
                                $ Query.QueryManager_MatchGroupAggregate_Req
                                $ cq
            [QT_INSERT] -> sendQuery
                                $ Query.QueryManager_ReqReqInsertReq
                                $ Query.QueryManager_Insert_Req
                                $ cq
            [QT_DELETE] -> sendQuery
                                $ Query.QueryManager_ReqReqDeleteReq
                                $ Query.QueryManager_Delete_Req
                                $ cq
            [QT_UPDATE] -> sendQuery
                                $ Query.QueryManager_ReqReqUpdateReq
                                $ Query.QueryManager_Update_Req
                                $ cq
            [QT_EXPLAIN] -> throw 
                                $ UnimplementedException
                                $ Unimplemented
                                "the explain query is not supported yet"
                                {-sendQuery
                                $ Query.QueryManager_ReqReqExplainReq
                                $ Query.QueryManager_Explain_Req
                                $ 0 -- TODO cq-}
            a -> throw 
                    $ QueryTypeUndefinedException
                    $ QueryTypeUndefined a cq_norm


    where
        sendQuery = send . QueryManager opts . Just
        cq = toInternalLazyText $ cq_norm
        cq_norm = compileQuery q


----------------------------

fromThingID' :: ThingID -> BS.ByteString
fromThingID' = encodeUtf8 . fromThingID

data CompilerState = CS { program :: [Opts -> Transaction_Req] }


data QueryError = QueryTypeUndefinedException QueryTypeUndefined
                | UnimplementedException Unimplemented
    deriving (Show)

instance Exception QueryError

data Unimplemented = Unimplemented { unimplementedText :: Text }
    deriving (Show)


data QueryTypeUndefined = QueryTypeUndefined { qt :: [QueryType]
                                             , queryText :: Text }
                deriving (Show)


compileTxFor :: TypeDBTX -> TypeDBSession -> IO (Either QueryError [Opts -> Transaction_Req])
compileTxFor req session_id = do -- ((reverse . program . fst) <$>)
              ioDischarged <- ( runM @IO
                              . runError @QueryError
                              . runState (CS [])
                              . interpret'
                              $ req) 
              return $ (reverse . program . fst) <$> ioDischarged
                  {-  ioDischarged <- (reinterpret interpret' req)
                return $ reverse . program . snd 
                              <$> run (runError (runState (CS []) ioDischarged))
                -}
  where
    interpret' :: Members '[State CompilerState, Error QueryError, Embed IO] a 
               => Sem (TX ': a) () -> Sem a ()
    interpret' = interpret $ \case
        (Open t o i)             -> withRandom (openTx_ t o i session_id)
        (Commit)                 -> withRandom (commitTx_)
        (Rollback)               -> withRandom (rollbackTx_)
        (TX_Thing a req')        -> withRandom (thingTx_ a req')
        (TX_Type lbl scope req') -> withRandom (typeTx_ lbl scope req')
        (ConceptManager req')    -> withRandom (conceptTx_ req')
        (Stream)                 -> withRandom (moreOrDoneStreamTx_)
        (Rule lbl req')          -> withRandom (ruleTx_ lbl req')
        (LogicManager req')      -> withRandom (logicManagerTx_ req')
        (QueryManager opts req') -> withRandom (queryManagerTx_ opts req')
    

runTxDefault :: (MonadIO m, MonadConc m) => TypeDBTX -> Callback Transaction_Server Transaction_Client -> TypeDBSession -> TypeDBM m (StatusCode, StatusDetails)
runTxDefault tx callback session_id = runTxWith tx [] callback session_id



runTxWithE :: (MonadIO m, MonadConc m) => TypeDBTX -> Opts -> Callback Transaction_Server Transaction_Client -> TypeDBSession -> TypeDBM m (Either QueryError (StatusCode, StatusDetails))
runTxWithE tx opts callback session_id = do
    compiled <- liftIO $ tx `compileTxFor` session_id
    case compiled of
      Left err -> return $ Left err
      Right comp -> Right <$> performTx (map ($ opts) comp) callback

runTxWith :: (MonadIO m, MonadConc m) => TypeDBTX -> Opts -> Callback Transaction_Server Transaction_Client -> TypeDBSession -> TypeDBM m (StatusCode, StatusDetails)
runTxWith tx opts callback session_id = do
    compiled <- liftIO $ tx `compileTxFor` session_id
    case compiled of
      Left err -> Control.Monad.Conc.Class.throw err
      Right comp -> do 
        performTx (map ($ opts) comp) callback


withRandom :: Members '[State CompilerState, Embed IO]  a => (UUID -> Opts -> Transaction_Req) -> Sem a ()
withRandom f = do
    uuid <- liftIO nextRandom
    modify (\s -> s { program = (f uuid):program s })


--type TypeDBTX' b = Members '[TX, Error QueryError] b => Sem b ()
type TypeDBTX = 
    forall m. Members '[State CompilerState, Error QueryError, Embed IO] m 
      => Sem (TX ': m) ()

defaultQueryOpts :: Maybe Options
defaultQueryOpts = Nothing

    {-
testTx :: TypeDBTX 
testTx = do
    openTx Transaction_TypeREAD Nothing networkLatency
    getThing (ThingID "sometid")
    deleteThing (ThingID "something")
    query defaultQueryOpts $
        ( match 
        $ (Var "x") `isa` (Lbl "person") $ε)
        `TypeDBQuery.get` [Var "x"]
    commit
-}

data CommitResult = CommitResult

receiveCommitRes :: Maybe Transaction_Server -> Maybe CommitResult
receiveCommitRes Nothing = Nothing
receiveCommitRes (Just (Transaction_Server Nothing)) = Nothing
receiveCommitRes (Just (Transaction_Server 
                 (Just (Transaction_ServerServerRes
                 (Transaction_Res _ Nothing))))) = Nothing
receiveCommitRes (Just (Transaction_Server 
                 (Just (Transaction_ServerServerRes
                 (Transaction_Res _ 
                 (Just (Transaction_ResResCommitRes
                 (Transaction_Commit_Res)))))))) = Just CommitResult
receiveCommitRes _ = Nothing

    {-
testCallback :: Callback Transaction_Server Transaction_Client
testCallback clientCall meta receive send done = do
    res <- receive :: IO (Either GRPCIOError (Maybe Transaction_Server))
    putStrLn $ "received: " ++ show res
    case res of
      (Right (Just (Transaction_Server (Just (Transaction_ServerServerRes _))))) ->
          return ()
      _ -> testCallback clientCall meta receive send done-}
    {-
    performTx $ map ($opts)  
                  $ [ openTx Transaction_TypeREAD Nothing networkLatency
                    , commitTx ]
    where opts = []
-}

toTx :: (UUID -> Opts -> Transaction_ReqReq -> Transaction_Req)
toTx txid opts a = Transaction_Req (toASCIIBytes txid) (fromList opts) (Just a)

moreOrDoneStreamTx_ :: UUID -> Opts -> Transaction_Req
moreOrDoneStreamTx_ txid opts = toTx txid opts $ Transaction_ReqReqStreamReq Transaction_Stream_Req

commitTx_ :: UUID -> Opts -> Transaction_Req
commitTx_ txid opts = toTx txid opts $ Transaction_ReqReqCommitReq Transaction_Commit_Req

rollbackTx_ :: UUID -> Opts -> Transaction_Req
rollbackTx_ txid opts = toTx txid opts $ Transaction_ReqReqRollbackReq Transaction_Rollback_Req

thingTx_ :: ThingID -> Maybe Concept.Thing_ReqReq ->  UUID -> Opts -> Transaction_Req
thingTx_ tid mthingReq txid opts = toTx txid opts $ Transaction_ReqReqThingReq $ Concept.Thing_Req (fromThingID' tid) mthingReq

logicManagerTx_ :: Maybe Logic.LogicManager_ReqReq -> UUID -> Opts -> Transaction_Req
logicManagerTx_ logicManagerReq txid opts = toTx txid opts 
        $ Transaction_ReqReqLogicManagerReq 
        $ Logic.LogicManager_Req logicManagerReq

conceptTx_ :: Maybe Concept.ConceptManager_ReqReq -> UUID -> Opts -> Transaction_Req
conceptTx_ conceptReq txid opts = toTx txid opts $ Transaction_ReqReqConceptManagerReq $ Concept.ConceptManager_Req conceptReq

ruleTx_ :: RuleLabel -> Maybe Logic.Rule_ReqReq -> UUID -> Opts -> Transaction_Req
ruleTx_ ruleLabel ruleReq txid opts = toTx txid opts 
    $ Transaction_ReqReqRuleReq $ Logic.Rule_Req 
        (toLogicRuleLabel ruleLabel)
        ruleReq

queryManagerTx_ :: Maybe Options -> Maybe Query.QueryManager_ReqReq -> UUID -> Opts -> Transaction_Req
queryManagerTx_ mopts mQReq txid opts = toTx txid opts 
        $ Transaction_ReqReqQueryManagerReq
        $ Query.QueryManager_Req 
            (mopts)
            (mQReq)

typeTx_ :: TypeLabel -> TypeScope -> Maybe Concept.Type_ReqReq -> UUID -> Opts -> Transaction_Req 
typeTx_ label scope mTypeReq txid opts = toTx txid opts $ Transaction_ReqReqTypeReq 
                  $ Concept.Type_Req 
                        (toInternalLazyText $ fromTypeLabel label)
                        (toInternalLazyText $ fromTypeScope scope)
                        mTypeReq
                

openTx_ :: Transaction_Type
        -> Maybe Options -> Int32 -> TypeDBSession -> UUID -> Opts -> Transaction_Req
openTx_ txType opts lat (TypeDBSession session_id) txid txopts = toTx txid txopts $ Transaction_ReqReqOpenReq 
       $ Transaction_Open_Req 
            (session_id)
            (Enumerated $ Right txType)
            opts
            lat

toConceptThingVal :: AttributeValue -> Concept.Attribute_Value
toConceptThingVal (AV_String t) 
  = Concept.Attribute_Value $ Just $ Concept.Attribute_ValueValueString (toInternalLazyText t)
toConceptThingVal (AV_Boolean b) 
  = Concept.Attribute_Value $ Just $ Concept.Attribute_ValueValueBoolean b
toConceptThingVal (AV_Long l)
  = Concept.Attribute_Value $ Just $ Concept.Attribute_ValueValueLong l
toConceptThingVal (AV_Double d) 
  = Concept.Attribute_Value $ Just $ Concept.Attribute_ValueValueDouble d
toConceptThingVal (AV_DateTime dt)
  = Concept.Attribute_Value $ Just $ Concept.Attribute_ValueValueDateTime dt



toInternalLazyText :: Text -> Data.Text.Internal.Lazy.Text
toInternalLazyText t = fromString $ unpack t
