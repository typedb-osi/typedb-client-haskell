{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module TypeDBTransaction where
import Prelude hiding (getStdGen)
import Network.GRPC.HighLevel.Generated
import Proto3.Suite.Types
import Options
import CoreDatabase
import Session
import qualified Query
import qualified Logic
import qualified Concept
import Transaction
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
--import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Internal.Lazy
import Types
import TypeDBClient (performTx, networkLatency)
import GHC.Exts (fromList, fromString)
import GHC.Int (Int32, Int64)

import Control.Monad.Freer
import Control.Monad.Freer.State
import System.Random
import Control.Monad.IO.Class
import Control.Monad.Conc.Class hiding (throw, catch) 

data Sth a
type RandomTxIdGen = StdGen

data TX a where
    Commit :: TX ()                                             -- done 
    Rollback :: TX ()                                           -- done
    Open :: Transaction_Type -> Maybe Options -> Int32 -> TX () -- done
    Stream :: TX ()                                             -- done
    QueryManager :: Maybe Options -> Maybe Query.QueryManager_ReqReq -> TX ()
    ConceptManager :: Maybe Concept.ConceptManager_ReqReq -> TX () -- done
    LogicManager :: Maybe Logic.LogicManager_ReqReq -> TX ()    -- done
    Rule :: RuleLabel -> Maybe Logic.Rule_ReqReq -> TX ()           -- done
    Type :: TypeLabel -> TypeScope -> Maybe Concept.Type_ReqReq -> TX () -- done
    TX_Thing :: ThingID -> Maybe Concept.Thing_ReqReq -> TX ()  -- done

commit :: Member TX a => Eff a ()
commit = send Commit

getMore :: Member TX a => Eff a ()
getMore = send Stream


openTx :: Member TX a => Transaction_Type -> Maybe Options -> Int32 -> Eff a ()
openTx t o i = send (Open t o i)

rollback :: Member TX a => Eff a ()
rollback = send Rollback

deleteThing :: Member TX a => ThingID -> Eff a ()
deleteThing t = send $ TX_Thing t $ Just 
              $ Concept.Thing_ReqReqThingDeleteReq
              $ Concept.Thing_Delete_Req



thingHas :: Member TX a => ThingID  -> [ThingType] -> KeysOnly -> Eff a ()
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
toConceptThing (Thing (ThingID id)
                      thingType
                      thingVal
                      isInferred)
    = Concept.Thing (encodeUtf8 id)
                    (toConceptThingType <$> thingType)
                    (toConceptThingVal <$> thingVal)
                    (isInferred==IsInferred)

thingSet :: Member TX a => ThingID -> Thing -> Eff a ()
thingSet tid thing = send $ TX_Thing tid $ Just
           $ Concept.Thing_ReqReqThingSetHasReq
           $ Concept.Thing_SetHas_Req 
                (Just $ toConceptThing thing)

thingUnset :: Member TX a => ThingID -> Thing -> Eff a ()
thingUnset tid thing = send $ TX_Thing tid $ Just
           $ Concept.Thing_ReqReqThingUnsetHasReq
           $ Concept.Thing_UnsetHas_Req 
                (Just $ toConceptThing thing)

thingGetRelations :: Member TX a => ThingID -> [ThingType] -> Eff a ()
thingGetRelations tid things = send $ TX_Thing tid $ Just
           $ Concept.Thing_ReqReqThingGetRelationsReq
           $ Concept.Thing_GetRelations_Req
                (fromList $ map toConceptThingType things)

thingGetPlaying :: Member TX a => ThingID -> Eff a ()
thingGetPlaying tid = send $ TX_Thing tid $ Just
           $ Concept.Thing_ReqReqThingGetPlayingReq
           $ Concept.Thing_GetPlaying_Req 


addPlayer :: Member TX a => ThingID -> Maybe ThingType -> Maybe Thing -> Eff a ()
addPlayer tid thingType thing = send $ TX_Thing tid $ Just
            $ Concept.Thing_ReqReqRelationAddPlayerReq
            $ Concept.Relation_AddPlayer_Req 
                (toConceptThingType <$> thingType)
                (toConceptThing <$> thing)

removePlayer :: Member TX a => ThingID -> Maybe ThingType -> Maybe Thing -> Eff a ()
removePlayer tid thingType thing = send $ TX_Thing tid $ Just
            $ Concept.Thing_ReqReqRelationRemovePlayerReq
            $ Concept.Relation_RemovePlayer_Req 
                (toConceptThingType <$> thingType)
                (toConceptThing <$> thing)


getPlayers :: Member TX a => ThingID -> [ThingType] -> Eff a ()
getPlayers tid roleTypes = send $ TX_Thing tid $ Just
           $ Concept.Thing_ReqReqRelationGetPlayersReq
           $ Concept.Relation_GetPlayers_Req
                    (fromList $ map toConceptThingType roleTypes)

getPlayersByRoleType :: Member TX a => ThingID -> Eff a ()
getPlayersByRoleType tid = send $ TX_Thing tid $ Just
           $ Concept.Thing_ReqReqRelationGetPlayersByRoleTypeReq
           $ Concept.Relation_GetPlayersByRoleType_Req

getRelating :: Member TX a => ThingID -> Eff a ()
getRelating tid = send $ TX_Thing tid $ Just
           $ Concept.Thing_ReqReqRelationGetRelatingReq
           $ Concept.Relation_GetRelating_Req

type AttributeOwnersFilter = ThingType

getAttributeOwners :: Member TX a => ThingID -> Maybe AttributeOwnersFilter -> Eff a ()
getAttributeOwners tid filter = send $ TX_Thing tid $ Just
           $ Concept.Thing_ReqReqAttributeGetOwnersReq
           $ Concept.Attribute_GetOwners_Req
           $ Concept.Attribute_GetOwners_ReqFilterThingType 
                    <$> toConceptThingType <$> filter

deleteType :: Member TX a => TypeLabel -> TypeScope -> Eff a ()
deleteType tl ts = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqTypeDeleteReq
           $ Concept.Type_Delete_Req


setTypeLabel :: Member TX a => TypeLabel -> TypeScope -> TypeLabel -> Eff a ()
setTypeLabel tl ts newLabel = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqTypeSetLabelReq
           $ Concept.Type_SetLabel_Req 
                (toConceptTypeLabel newLabel)

toConceptTypeLabel :: TypeLabel -> Data.Text.Internal.Lazy.Text
toConceptTypeLabel = toInternalLazyText . fromTypeLabel


isAbstract :: Member TX a => TypeLabel -> TypeScope -> Eff a ()
isAbstract tl ts = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqTypeIsAbstractReq
           $ Concept.Type_IsAbstract_Req

getSupertype :: Member TX a => TypeLabel -> TypeScope -> Eff a ()
getSupertype tl ts = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqTypeGetSupertypesReq
           $ Concept.Type_GetSupertypes_Req

getSupertypes :: Member TX a => TypeLabel -> TypeScope -> Eff a ()
getSupertypes tl ts = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqTypeGetSupertypeReq
           $ Concept.Type_GetSupertype_Req

setSupertype :: Member TX a => TypeLabel -> TypeScope -> Maybe ThingType -> Eff a ()
setSupertype tl ts mt = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqTypeSetSupertypeReq
           $ Concept.Type_SetSupertype_Req
                (toConceptThingType <$> mt)


getRelationTypes :: Member TX a => TypeLabel -> TypeScope -> Eff a ()
getRelationTypes tl ts = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqRoleTypeGetRelationTypesReq
           $ Concept.RoleType_GetRelationTypes_Req

getRoleTypePlayers :: Member TX a => TypeLabel -> TypeScope -> Eff a ()
getRoleTypePlayers tl ts = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqRoleTypeGetPlayersReq
           $ Concept.RoleType_GetPlayers_Req

getInstances :: Member TX a => TypeLabel -> TypeScope -> Eff a ()
getInstances tl ts = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqThingTypeGetInstancesReq
           $ Concept.ThingType_GetInstances_Req

setAbstract :: Member TX a => TypeLabel -> TypeScope -> Eff a ()
setAbstract tl ts = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqThingTypeSetAbstractReq
           $ Concept.ThingType_SetAbstract_Req

unsetAbstract :: Member TX a => TypeLabel -> TypeScope -> Eff a ()
unsetAbstract tl ts = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqThingTypeUnsetAbstractReq
           $ Concept.ThingType_UnsetAbstract_Req

getOwns :: Member TX a => TypeLabel -> TypeScope -> Maybe AttributeValueType -> KeysOnly -> Eff a ()
getOwns tl ts avt keysOnly = send $ Type tl ts $ Just
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


setOwns :: Member TX a => TypeLabel -> TypeScope -> Maybe ThingType -> Maybe ThingType -> KeysOnly -> Eff a ()
setOwns tl ts avt t keysOnly = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqThingTypeSetOwnsReq
           $ Concept.ThingType_SetOwns_Req
                (toConceptThingType <$> avt)
                (Concept.ThingType_SetOwns_ReqOverriddenOverriddenType 
                    . toConceptThingType <$> t) 
                (keysOnly == KeysOnly)

unsetOwns :: Member TX a => TypeLabel -> TypeScope -> Maybe ThingType -> Eff a ()
unsetOwns tl ts tt  = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqThingTypeUnsetOwnsReq
           $ Concept.ThingType_UnsetOwns_Req
                (toConceptThingType <$> tt)


getPlays :: Member TX a => TypeLabel -> TypeScope -> Eff a ()
getPlays tl ts = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqThingTypeGetPlaysReq
           $ Concept.ThingType_GetPlays_Req


setPlays :: Member TX a => TypeLabel -> TypeScope -> Maybe ThingType -> Maybe ThingType -> Eff a ()
setPlays tl ts mt mo = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqThingTypeSetPlaysReq
           $ Concept.ThingType_SetPlays_Req
                (toConceptThingType <$> mt)
                ((Concept.ThingType_SetPlays_ReqOverriddenOverriddenRole 
                    . toConceptThingType) <$> mo)


unsetPlays :: Member TX a => TypeLabel -> TypeScope -> Maybe ThingType -> Eff a ()
unsetPlays tl ts mt = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqThingTypeUnsetPlaysReq
           $ Concept.ThingType_UnsetPlays_Req
                (toConceptThingType <$> mt)


createEntityType :: Member TX a => TypeLabel -> TypeScope -> Eff a ()
createEntityType tl ts = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqEntityTypeCreateReq
           $ Concept.EntityType_Create_Req



createRelationType :: Member TX a => TypeLabel -> TypeScope -> Eff a ()
createRelationType tl ts = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqRelationTypeCreateReq
           $ Concept.RelationType_Create_Req


getRelatesFor :: Member TX a => TypeLabel -> TypeScope -> RoleLabel -> Eff a ()
getRelatesFor tl ts rl = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqRelationTypeGetRelatesForRoleLabelReq
           $ Concept.RelationType_GetRelatesForRoleLabel_Req
                (toInternalLazyText . fromRoleLabel $ rl)

getRelates :: Member TX a => TypeLabel -> TypeScope -> Eff a ()
getRelates tl ts = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqRelationTypeGetRelatesReq
           $ Concept.RelationType_GetRelates_Req


setRelates :: Member TX a => TypeLabel -> TypeScope -> RelationLabel -> Maybe RelationLabel -> Eff a ()
setRelates tl ts rl mrl = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqRelationTypeSetRelatesReq
           $ Concept.RelationType_SetRelates_Req
                (toInternalLazyText . fromRelationLabel $ rl)
                (Concept.RelationType_SetRelates_ReqOverriddenOverriddenLabel 
                   . toInternalLazyText . fromRelationLabel <$> mrl)

unsetRelates :: Member TX a => TypeLabel -> TypeScope -> RelationLabel -> Eff a ()
unsetRelates tl ts rl = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqRelationTypeUnsetRelatesReq
           $ Concept.RelationType_UnsetRelates_Req
                (toInternalLazyText . fromRelationLabel $ rl)


putAttributeType :: Member TX a => TypeLabel -> TypeScope -> Maybe AttributeValue -> Eff a ()
putAttributeType tl ts mav = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqAttributeTypePutReq
           $ Concept.AttributeType_Put_Req
                (toConceptAttributeValue <$> mav)

getAttributeType :: Member TX a => TypeLabel -> TypeScope -> Maybe AttributeValue -> Eff a ()
getAttributeType tl ts mav = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqAttributeTypeGetReq
           $ Concept.AttributeType_Get_Req
                (toConceptAttributeValue <$> mav)
                
getAttributeTypeRegex :: Member TX a => TypeLabel -> TypeScope -> Eff a ()
getAttributeTypeRegex tl ts = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqAttributeTypeGetRegexReq
           $ Concept.AttributeType_GetRegex_Req


setAttributeTypeRegex :: Member TX a => TypeLabel -> TypeScope -> Eff a ()
setAttributeTypeRegex tl ts = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqAttributeTypeGetRegexReq
           $ Concept.AttributeType_GetRegex_Req


getAttributeTypeOwners :: Member TX a => TypeLabel -> TypeScope -> KeysOnly -> Eff a ()
getAttributeTypeOwners tl ts keysOnly = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqAttributeTypeGetOwnersReq
           $ Concept.AttributeType_GetOwners_Req
                (keysOnly == KeysOnly)


getThingType :: Member TX a => TypeLabel -> Eff a ()
getThingType tl = send $ ConceptManager $ Just 
            $ Concept.ConceptManager_ReqReqGetThingTypeReq
            $ Concept.ConceptManager_GetThingType_Req
                (toConceptTypeLabel tl)


getThing :: Member TX a => ThingID -> Eff a ()
getThing tid = send $ ConceptManager $ Just 
            $ Concept.ConceptManager_ReqReqGetThingReq
            $ Concept.ConceptManager_GetThing_Req
                (toConceptThingID tid)

toConceptThingID :: ThingID -> BS.ByteString
toConceptThingID = encodeUtf8 . fromThingID


conceptPutEntityType :: Member TX a => TypeLabel -> Eff a ()
conceptPutEntityType tid = send $ ConceptManager $ Just 
            $ Concept.ConceptManager_ReqReqPutEntityTypeReq
            $ Concept.ConceptManager_PutEntityType_Req
                (toConceptTypeLabel tid)

conceptPutAttributeType :: Member TX a => TypeLabel -> AttributeValueType -> Eff a ()
conceptPutAttributeType tid avt = send $ ConceptManager $ Just 
            $ Concept.ConceptManager_ReqReqPutAttributeTypeReq
            $ Concept.ConceptManager_PutAttributeType_Req
                (toConceptTypeLabel tid)
                (Enumerated . Right $ toConceptAttributeValueType avt)

conceptPutRelationType :: Member TX a => TypeLabel -> Eff a ()
conceptPutRelationType tid = send $ ConceptManager $ Just 
            $ Concept.ConceptManager_ReqReqPutRelationTypeReq
            $ Concept.ConceptManager_PutRelationType_Req
                (toConceptTypeLabel tid)




toLogicRuleLabel :: RuleLabel -> Data.Text.Internal.Lazy.Text
toLogicRuleLabel = toInternalLazyText . fromRuleLabel

deleteRule :: Member TX a => RuleLabel -> Eff a ()
deleteRule rl = send $ Rule rl $ Just
            $ Logic.Rule_ReqReqRuleDeleteReq
            $ Logic.Rule_Delete_Req

setRuleLabel :: Member TX a => RuleLabel -> RuleLabel -> Eff a ()
setRuleLabel rl lbl = send $ Rule rl $ Just
            $ Logic.Rule_ReqReqRuleSetLabelReq
            $ Logic.Rule_SetLabel_Req
                (toLogicRuleLabel lbl)

getRule :: Member TX a => RuleLabel -> Eff a ()
getRule lbl = send $ LogicManager $ Just
            $ Logic.LogicManager_ReqReqGetRuleReq
            $ Logic.LogicManager_GetRule_Req
                (toLogicRuleLabel lbl)

putRule :: Member TX a => RuleLabel -> When -> Then -> Eff a ()
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

getRules :: Member TX a => Eff a ()
getRules = send $ LogicManager $ Just
            $ Logic.LogicManager_ReqReqGetRulesReq
            $ Logic.LogicManager_GetRules_Req





{--- 

data QueryManager_ReqReq = QueryManager_ReqReqDefineReq Query.QueryManager_Define_Req
                         | QueryManager_ReqReqUndefineReq Query.QueryManager_Undefine_Req
                         | QueryManager_ReqReqMatchReq Query.QueryManager_Match_Req
                         | QueryManager_ReqReqMatchAggregateReq Query.QueryManager_MatchAggregate_Req
                         | QueryManager_ReqReqMatchGroupReq Query.QueryManager_MatchGroup_Req
                         | QueryManager_ReqReqMatchGroupAggregateReq Query.QueryManager_MatchGroupAggregate_Req
                         | QueryManager_ReqReqInsertReq Query.QueryManager_Insert_Req
                         | QueryManager_ReqReqDeleteReq Query.QueryManager_Delete_Req
                         | QueryManager_ReqReqUpdateReq Query.QueryManager_Update_Req
                         | QueryManager_ReqReqExplainReq Query.QueryManager_Explain_Req
                         deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic, Hs.NFData)

todo:


data Transaction_ReqReq =
                        | Transaction_ReqReqQueryManagerReq Query.QueryManager_Req
                       

data Thing = Thing{thingIid :: Hs.ByteString,
                   thingType :: Hs.Maybe Concept.Type,
                   thingValue :: Hs.Maybe Concept.Attribute_Value,
                   thingInferred :: Hs.Bool}
           deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic, Hs.NFData)

data Type = Type{typeLabel :: Hs.Text, typeScope :: Hs.Text,
                 typeEncoding :: HsProtobuf.Enumerated Concept.Type_Encoding,
                 typeValueType ::
                 HsProtobuf.Enumerated Concept.AttributeType_ValueType,
                 typeRoot :: Hs.Bool}
          deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic, Hs.NFData)

data AttributeType_ValueType = AttributeType_ValueTypeOBJECT
                             | AttributeType_ValueTypeBOOLEAN
                             | AttributeType_ValueTypeLONG
                             | AttributeType_ValueTypeDOUBLE
                             | AttributeType_ValueTypeSTRING
                             | AttributeType_ValueTypeDATETIME
                             deriving (Hs.Show, Hs.Eq, Hs.Generic, Hs.NFData)

data Type_Encoding = Type_EncodingTHING_TYPE
                   | Type_EncodingENTITY_TYPE
                   | Type_EncodingRELATION_TYPE
                   | Type_EncodingATTRIBUTE_TYPE
                   | Type_EncodingROLE_TYPE
                   deriving (Hs.Show, Hs.Eq, Hs.Generic, Hs.NFData)

data ConceptManager_ReqReq = ConceptManager_ReqReqGetThingTypeReq Concept.ConceptManager_GetThingType_Req
                           | ConceptManager_ReqReqGetThingReq Concept.ConceptManager_GetThing_Req
                           | ConceptManager_ReqReqPutEntityTypeReq Concept.ConceptManager_PutEntityType_Req
                           | ConceptManager_ReqReqPutAttributeTypeReq Concept.ConceptManager_PutAttributeType_Req
                           | ConceptManager_ReqReqPutRelationTypeReq Concept.ConceptManager_PutRelationType_Req
                           deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic, Hs.NFData)
-}



fromThingID' :: ThingID -> BS.ByteString
fromThingID' = encodeUtf8 . fromThingID

data CompilerState = CS { randomGen :: RandomTxIdGen
                        , program :: [Opts -> Transaction_Req] }

compileTx :: RandomTxIdGen -> Eff '[TX] a -> [Opts -> Transaction_Req]
compileTx gen req = reverse . program . snd 
                  $ run (runState (CS gen []) (reinterpret interpret' req))
  where
    interpret' :: TX v -> Eff '[State CompilerState] v
    interpret' (Open t o i) = withRandom (openTx_ t o i)
    interpret' (Commit) = withRandom (commitTx_)
    interpret' (Rollback) = withRandom (rollbackTx_)
    interpret' (TX_Thing a req) = withRandom (thingTx_ a req)
    interpret' (Type lbl scope req) = withRandom (typeTx_ lbl scope req)
    interpret' (ConceptManager req) = withRandom (conceptTx_ req)
    interpret' (Stream) = withRandom (moreOrDoneStreamTx_)
    interpret' (Rule lbl req) = withRandom (ruleTx_ lbl req)
    interpret' (LogicManager req) = withRandom (logicManagerTx_ req)
    interpret' (QueryManager opts req) = withRandom (queryManagerTx_ opts req)
    


runTxDefault :: (MonadIO m, MonadConc m) => TypeDBTX -> Callback Transaction_Server Transaction_Client -> TypeDBM m (StatusCode, StatusDetails)
runTxDefault tx callback = runTxWith tx [] callback



runTxWith :: (MonadIO m, MonadConc m) => TypeDBTX -> Opts -> Callback Transaction_Server Transaction_Client -> TypeDBM m (StatusCode, StatusDetails)
runTxWith tx opts callback = do
    gen <- liftIO $ newStdGen 
    let compiled = compileTx gen tx
    performTx (map ($opts) compiled) callback


withRandom :: (String -> Opts -> Transaction_Req) -> Eff '[State CompilerState] ()
withRandom a = do
    g <- gets randomGen
    let r = randomRs ('a','z') g
        (_:: Int,g') = random g  
    modify (\s -> s { randomGen = g', program = (a r):program s })


type TypeDBTX = Eff '[TX] ()

testTx :: TypeDBTX
testTx = do
    openTx Transaction_TypeREAD Nothing networkLatency
    getThing (ThingID "sometid")
    deleteThing (ThingID "something")
    commit

testCallback :: Callback Transaction_Server Transaction_Client
testCallback clientCall meta receive send done = do
    res <- receive :: IO (Either GRPCIOError (Maybe Transaction_Server))
    case res of
      (Right (Just (Transaction_Server (Just (Transaction_ServerServerRes _))))) ->
          return ()
      _ -> testCallback clientCall meta receive send done
    {-
    performTx $ map ($opts)  
                  $ [ openTx Transaction_TypeREAD Nothing networkLatency
                    , commitTx ]
    where opts = []
-}

toTx :: (String -> Opts -> Transaction_ReqReq -> Transaction_Req)
toTx txid opts a = Transaction_Req (BS.pack txid) (fromList opts) (Just a)

moreOrDoneStreamTx_ :: String -> Opts -> Transaction_Req
moreOrDoneStreamTx_ txid opts = toTx txid opts $ Transaction_ReqReqStreamReq Transaction_Stream_Req

commitTx_ :: String -> Opts -> Transaction_Req
commitTx_ txid opts = toTx txid opts $ Transaction_ReqReqCommitReq Transaction_Commit_Req

rollbackTx_ :: String -> Opts -> Transaction_Req
rollbackTx_ txid opts = toTx txid opts $ Transaction_ReqReqRollbackReq Transaction_Rollback_Req

type Metadata = String -> Opts
thingTx_ :: ThingID -> Maybe Concept.Thing_ReqReq ->  String -> Opts -> Transaction_Req
thingTx_ tid mthingReq txid opts = toTx txid opts $ Transaction_ReqReqThingReq $ Concept.Thing_Req (fromThingID' tid) mthingReq

logicManagerTx_ :: Maybe Logic.LogicManager_ReqReq -> String -> Opts -> Transaction_Req
logicManagerTx_ logicManagerReq txid opts = toTx txid opts 
        $ Transaction_ReqReqLogicManagerReq 
        $ Logic.LogicManager_Req logicManagerReq

conceptTx_ :: Maybe Concept.ConceptManager_ReqReq -> String -> Opts -> Transaction_Req
conceptTx_ conceptReq txid opts = toTx txid opts $ Transaction_ReqReqConceptManagerReq $ Concept.ConceptManager_Req conceptReq

ruleTx_ :: RuleLabel -> Maybe Logic.Rule_ReqReq -> String -> Opts -> Transaction_Req
ruleTx_ ruleLabel ruleReq txid opts = toTx txid opts 
    $ Transaction_ReqReqRuleReq $ Logic.Rule_Req 
        (toLogicRuleLabel ruleLabel)
        ruleReq

queryManagerTx_ :: Maybe Options -> Maybe Query.QueryManager_ReqReq -> String -> Opts -> Transaction_Req
queryManagerTx_ mopts mQReq txid opts = toTx txid opts 
        $ Transaction_ReqReqQueryManagerReq
        $ Query.QueryManager_Req 
            (mopts)
            (mQReq)

typeTx_ :: TypeLabel -> TypeScope -> Maybe Concept.Type_ReqReq -> String -> Opts -> Transaction_Req 
typeTx_ label scope mTypeReq txid opts = toTx txid opts $ Transaction_ReqReqTypeReq 
                  $ Concept.Type_Req 
                        (toInternalLazyText $ fromTypeLabel label)
                        (toInternalLazyText $ fromTypeScope scope)
                        mTypeReq
                

openTx_ :: Transaction_Type
        -> Maybe Options -> Int32 -> String -> Opts -> Transaction_Req
openTx_ txType opts lat txid txopts = toTx txid txopts $ Transaction_ReqReqOpenReq 
       $ Transaction_Open_Req 
            (BS.pack txid)
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

