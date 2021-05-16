{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module TypeDBTransaction where
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
import Data.Text.Lazy (toStrict, fromStrict)
import Types
import TypeDBClient (performTx, networkLatency)
import GHC.Exts (fromList)

tryTx :: TypeDBM IO  (StatusCode, StatusDetails)
tryTx = performTx $ map ($opts)  
                  $ [ openTx Transaction_TypeREAD Nothing networkLatency
                    , commitTx ]
    where opts = []


toTx opts a = Transaction_Req "reqId" (fromList opts) (Just a)
commitTx opts = toTx opts $ Transaction_ReqReqCommitReq Transaction_Commit_Req
rollbackTx opts = toTx opts $ Transaction_ReqReqRollbackReq Transaction_Rollback_Req
openTx txType opts lat txopts = toTx txopts $ Transaction_ReqReqOpenReq 
       $ Transaction_Open_Req 
            "randomTxID"
            (Enumerated $ Right txType)
            opts
            lat
