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
    Stream :: Sth a -> TX a
    QueryManager :: Sth a -> TX a
    ConceptManager :: Sth a ->TX a
    LogicManager :: Sth a -> TX a
    Rule :: Sth a -> TX a
    Type :: TypeLabel -> TypeScope -> Maybe Concept.Type_ReqReq -> TX ()
    TX_Thing :: ThingID -> Maybe Concept.Thing_ReqReq -> TX ()  -- done

commit :: Member TX a => Eff a ()
commit = send Commit

openTx :: Member TX a => Transaction_Type -> Maybe Options -> Int32 -> Eff a ()
openTx t o i = send (Open t o i)

rollback :: Member TX a => Eff a ()
rollback = send Rollback

-- normal get thing request; NOT A GET QUERY!!
-- used to get the thing after a query e.g.
getThing :: Member TX a => ThingID -> Eff a ()
getThing t = send (TX_Thing t Nothing)

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
                (toInternalLazyText $ fromTypeLabel newLabel)


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


setOwns :: Member TX a => TypeLabel -> TypeScope -> Maybe ThingType -> Maybe ThingType -> KeysOnly -> Eff a ()
setOwns tl ts avt t keysOnly = send $ Type tl ts $ Just
           $ Concept.Type_ReqReqThingTypeSetOwnsReq
           $ Concept.ThingType_SetOwns_Req
                (toConceptThingType <$> avt)
                (Concept.ThingType_SetOwns_ReqOverriddenOverriddenType 
                    . toConceptThingType <$> t) 
                (keysOnly == KeysOnly)

{---
todo:

data Type_ReqReq = 
                 | Type_ReqReqThingTypeUnsetOwnsReq Concept.ThingType_UnsetOwns_Req
                 | Type_ReqReqThingTypeGetPlaysReq Concept.ThingType_GetPlays_Req
                 | Type_ReqReqThingTypeSetPlaysReq Concept.ThingType_SetPlays_Req
                 | Type_ReqReqThingTypeUnsetPlaysReq Concept.ThingType_UnsetPlays_Req
                 | Type_ReqReqEntityTypeCreateReq Concept.EntityType_Create_Req
                 | Type_ReqReqRelationTypeCreateReq Concept.RelationType_Create_Req
                 | Type_ReqReqRelationTypeGetRelatesForRoleLabelReq Concept.RelationType_GetRelatesForRoleLabel_Req
                 | Type_ReqReqRelationTypeGetRelatesReq Concept.RelationType_GetRelates_Req
                 | Type_ReqReqRelationTypeSetRelatesReq Concept.RelationType_SetRelates_Req
                 | Type_ReqReqRelationTypeUnsetRelatesReq Concept.RelationType_UnsetRelates_Req
                 | Type_ReqReqAttributeTypePutReq Concept.AttributeType_Put_Req
                 | Type_ReqReqAttributeTypeGetReq Concept.AttributeType_Get_Req
                 | Type_ReqReqAttributeTypeGetRegexReq Concept.AttributeType_GetRegex_Req
                 | Type_ReqReqAttributeTypeSetRegexReq Concept.AttributeType_SetRegex_Req
                 | Type_ReqReqAttributeTypeGetOwnersReq Concept.AttributeType_GetOwners_Req
                 deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic, Hs.NFData)




data Transaction_ReqReq =
                        | Transaction_ReqReqStreamReq Transaction.Transaction_Stream_Req
                        
                        
                        | Transaction_ReqReqQueryManagerReq Query.QueryManager_Req
                        | Transaction_ReqReqConceptManagerReq Concept.ConceptManager_Req
                        | Transaction_ReqReqLogicManagerReq Logic.LogicManager_Req
                        | Transaction_ReqReqRuleReq Logic.Rule_Req
                        | Transaction_ReqReqTypeReq Concept.Type_Req
                        

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

commitTx_ :: String -> Opts -> Transaction_Req
commitTx_ txid opts = toTx txid opts $ Transaction_ReqReqCommitReq Transaction_Commit_Req

rollbackTx_ :: String -> Opts -> Transaction_Req
rollbackTx_ txid opts = toTx txid opts $ Transaction_ReqReqRollbackReq Transaction_Rollback_Req

type Metadata = String -> Opts
thingTx_ :: ThingID -> Maybe Concept.Thing_ReqReq ->  String -> Opts -> Transaction_Req
thingTx_ tid mthingReq txid opts = toTx txid opts $ Transaction_ReqReqThingReq $ Concept.Thing_Req (fromThingID' tid) mthingReq


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

