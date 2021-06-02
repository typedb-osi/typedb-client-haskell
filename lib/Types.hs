{-# -Wnounused-imports #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
module Types where
import Network.GRPC.HighLevel.Generated
import Data.Text
import qualified Data.ByteString as BS
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad.Conc.Class hiding (throw, catch) 
import Control.Monad.Catch
import Network.GRPC.LowLevel.Op
import Network.GRPC.LowLevel.Call


newtype Keyspace = Keyspace { getKeyspace :: Text }
newtype TypeDBSession = TypeDBSession { getTypeDBSession :: BS.ByteString }


data TypeDBConfig = TypeDBConfig { clientConfig   :: ClientConfig
                                 , timeoutSeconds :: Int }

newtype TypeDBM m a = TypeDBM { fromTypeDB :: ReaderT TypeDBConfig m a}
    deriving newtype ( Functor, Applicative, Monad, MonadThrow
                     , MonadCatch, MonadMask, MonadIO, MonadConc)


newtype TypeDBError = TypeDBError { getError :: Text }
    deriving (Show, Exception)


type Callback a b = ClientCall
              -> MetadataMap
              -> StreamRecv a
              -> StreamSend b
              -> WritesDone
              -> IO ()
