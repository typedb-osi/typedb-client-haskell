{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
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
--import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Internal.Lazy
import Types
import TypeDBClient (performTx, networkLatency)
import GHC.Exts (fromList)
import GHC.Int (Int32)

import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Control.Monad.Freer.Writer
import Control.Monad.Freer.State
import System.Exit hiding (ExitSuccess)
import Data.Proxy
import System.Random

data Sth a
type RandomTxIdGen = StdGen

data TX a where
    Commit :: TX ()
    Rollback :: TX ()
    Open :: Transaction_Type -> Options -> Int32 -> TX ()
    Stream :: Sth a -> TX a
    QueryManager :: Sth a -> TX a
    ConceptManager :: Sth a ->TX a
    LogicManager :: Sth a -> TX a
    Rule :: Sth a -> TX a
    Type :: Sth a -> TX a
    Thing :: Sth a -> TX a

commit :: Member TX a => Eff a ()
commit = send Commit

type Opts = [(Data.Text.Internal.Lazy.Text
             ,Data.Text.Internal.Lazy.Text)]


data CompilerState = CS { randomGen :: RandomTxIdGen
                        , program :: [Opts -> Transaction_Req] }

compileTx :: RandomTxIdGen -> Eff '[TX] a -> [Opts -> Transaction_Req]
compileTx gen req = reverse . program . snd 
                  $ run (runState (CS gen []) (reinterpret go req))
  where
    go :: TX v -> Eff '[State CompilerState] v
    go (Open t o i) = withRandom (openTx t (Just o) i)
    go (Commit) = withRandom (commitTx)
    go (Rollback) = withRandom (rollbackTx)

withRandom :: (String -> Opts -> Transaction_Req) -> Eff '[State CompilerState] ()
withRandom a = do
    g <- gets randomGen
    let r = randomRs ('a','z') g
        (_:: Int,g') = random g  
    modify (\s -> s { randomGen = g', program = (a r):program s })

    {-
tryTx :: TypeDBM IO  (StatusCode, StatusDetails)
tryTx = performTx $ map ($opts)  
                  $ [ openTx Transaction_TypeREAD Nothing networkLatency
                    , commitTx ]
    where opts = []
-}

toTx :: (String -> Opts -> Transaction_ReqReq -> Transaction_Req)
toTx txid opts a = Transaction_Req (BS.pack txid) (fromList opts) (Just a)
commitTx txid opts = toTx txid opts $ Transaction_ReqReqCommitReq Transaction_Commit_Req
rollbackTx txid opts = toTx txid opts $ Transaction_ReqReqRollbackReq Transaction_Rollback_Req
openTx txType opts lat txid txopts = toTx txid txopts $ Transaction_ReqReqOpenReq 
       $ Transaction_Open_Req 
            (BS.pack txid)
            (Enumerated $ Right txType)
            opts
            lat
