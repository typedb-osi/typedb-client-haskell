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
import GHC.Int (Int32, Int64)
import qualified Concept
import qualified Data.Text.Internal.Lazy

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

newtype Scope = Scope { fromScope :: Text }

type Opts = [(Data.Text.Internal.Lazy.Text
             ,Data.Text.Internal.Lazy.Text)]
newtype ThingID = ThingID { fromThingID :: Text }

data AttributeValue = AV_String Text
                    | AV_Boolean Bool
                    | AV_Long Int64
                    | AV_Double Double
                    | AV_DateTime Int64
                    deriving (Show, Eq, Ord)

data AttributeValueType = AVT_Object
                        | AVT_Boolean
                        | AVT_Long
                        | AVT_Double
                        | AVT_String
                        | AVT_DateTime
                        deriving (Show, Eq)

data ThingType = ThingType
                   { tt_typeLabel :: TypeLabel
                   , tt_typeScope :: TypeScope
                   , tt_typeEncoding :: Concept.Type_Encoding
                   , tt_typeValueType :: Concept.AttributeType_ValueType
                   , tt_typeRoot :: IsTypeRoot }
    deriving (Show, Eq)


data Thing = Thing { thingIid :: ThingID
                   , thingType :: Maybe ThingType
                   , thingValue :: Maybe AttributeValue
                   , thingInferred :: IsInferred }



data KeysOnly = KeysOnly | AllKeys
    deriving (Show, Eq)

data IsTypeRoot = RootType | NoRootType
    deriving (Show, Eq)

data IsInferred = IsInferred | NotInferred
    deriving (Show, Eq)

newtype TypeLabel = TypeLabel { fromTypeLabel :: Text }
    deriving (Show, Eq)
newtype RelationLabel = RelationLabel { fromRelationLabel :: Text }
    deriving (Show, Eq)
newtype RoleLabel = RoleLabel { fromRoleLabel :: Text }
    deriving (Show, Eq)
newtype RuleLabel = RuleLabel { fromRuleLabel :: Text }
    deriving (Show, Eq)
newtype TypeScope = TypeScope { fromTypeScope :: Text }
    deriving (Show, Eq)
newtype When = When { fromWhen :: Text }
    deriving (Show, Eq)
newtype Then = Then { fromThen :: Text }
    deriving (Show, Eq)
