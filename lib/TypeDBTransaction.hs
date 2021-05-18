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
import GHC.Exts (fromList)
import GHC.Int (Int32)

import Control.Monad.Freer
import Control.Monad.Freer.State
import System.Random
import Control.Monad.IO.Class
import Control.Monad.Conc.Class hiding (throw, catch) 

data Sth a
type RandomTxIdGen = StdGen

data TX a where
    Commit :: TX ()
    Rollback :: TX ()
    Open :: Transaction_Type -> Maybe Options -> Int32 -> TX ()
    Stream :: Sth a -> TX a
    QueryManager :: Sth a -> TX a
    ConceptManager :: Sth a ->TX a
    LogicManager :: Sth a -> TX a
    Rule :: Sth a -> TX a
    Type :: Sth a -> TX a
    Thing :: ThingID -> Maybe Concept.Thing_ReqReq -> TX ()

commit :: Member TX a => Eff a ()
commit = send Commit

openTx :: Member TX a => Transaction_Type -> Maybe Options -> Int32 -> Eff a ()
openTx t o i = send (Open t o i)

rollback :: Member TX a => Eff a ()
rollback = send Rollback

-- normal get thing request; NOT A GET QUERY!!
-- used to get the thing after a query e.g.
getThing :: Member TX a => ThingID -> Eff a ()
getThing t = send (Thing t Nothing)

deleteThing :: Member TX a => ThingID -> Eff a ()
deleteThing t = send $ Thing t $ Just 
              $ Concept.Thing_ReqReqThingDeleteReq
              $ Concept.Thing_Delete_Req
    

type Opts = [(Data.Text.Internal.Lazy.Text
             ,Data.Text.Internal.Lazy.Text)]

newtype ThingID = TID { fromThingID :: Text }

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
    interpret' (Thing a Nothing) = withRandom (thingTx_ a Nothing)

runTxDefault :: (MonadIO m, MonadConc m) => TypeDBTX -> TypeDBM m (StatusCode, StatusDetails)
runTxDefault = flip runTxWith []

runTxWith :: (MonadIO m, MonadConc m) => TypeDBTX -> Opts -> TypeDBM m (StatusCode, StatusDetails)
runTxWith tx opts = do
    gen <- liftIO $ newStdGen 
    let compiled = compileTx gen tx
    performTx $ map ($opts) compiled 


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
    getThing (TID "sometid")
    deleteThing (TID "something")
    commit

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

openTx_ :: Transaction_Type
        -> Maybe Options -> Int32 -> String -> Opts -> Transaction_Req
openTx_ txType opts lat txid txopts = toTx txid txopts $ Transaction_ReqReqOpenReq 
       $ Transaction_Open_Req 
            (BS.pack txid)
            (Enumerated $ Right txType)
            opts
            lat

