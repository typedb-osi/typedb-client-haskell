module QueryDSL where
import Data.List.NonEmpty

data Query 
    = Get (NonEmpty Match) [String] [Modifier]
    | Delete (NonEmpty Match) (NonEmpty Variable) [Modifier]
    | Insert [Match] (NonEmpty Instance)
    | Group (NonEmpty Match) [String] String (Maybe Int)
    | Aggregate (NonEmpty Match) [String] String
    | Compute (NonEmpty Condition)
    deriving Show

($$) :: String -> String
($$) = id

(&&&) :: a -> [a] -> [a]
(&&&) = (:)

get = (:[])

testGet :: [String]
testGet = get $ "hi" &&& "ho"

data  Expr_ a
= (a ∼ Int)
=> LitInt_ 
Int
| (a ∼  Bool) =>  LitBool_ Bool
| (a ∼ Int)
=>  Add_ (Expr_ Int) (Expr_ Int)
| (a ∼  Bool) =>  Not_ (Expr_ Bool)
| If_ (Expr_ Bool) (Expr_ a) (Expr_ a)
