{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module TypeDBQuery where
import Data.Text (Text, pack, unpack, intercalate)
import qualified Data.Text as T
import Prelude hiding (String,or,not,sum,max, min)
import GHC.Exts (IsList(..),IsString(..))
import TypeDBTH
import Data.List (sort)
import qualified Data.Set as Set

data Calculation a = R_LT a
                   | R_GT a
                   | R_GE a
                   | R_LE a
                   | R_EQ a
                   | R_NEQ a

newtype Variable = Var { fromVar :: Text }
newtype Label = Lbl { fromLbl :: Text }

instance Show Variable where
    show (Var v) = unpack v
instance IsString Label where
    fromString = Lbl . pack
instance IsString Variable where
    fromString = Var . pack

data RolePlay a b = RP { role :: IsaMember a => Maybe a, player :: IsaMember b => b }

sRP :: RelationMember b => b -> RolePlay Label b
sRP b = RP Nothing b

rp :: (RelationMember b) => Label -> b -> RolePlay Label b
rp a b = RP (Just a) b

newtype Relation a b = Rel { fromRel :: (IsaMember a, IsaMember b) => [RolePlay a b]}

instance (IsaMember a, IsaMember b) => IsList (Relation a b) where
    type Item (Relation a b) = (RolePlay a b)
    fromList = Rel
    toList = fromRel

class RelationMember a where
instance RelationMember Variable where
instance RelationMember Label where


class IsaMember a where
    showIsaMember :: a -> Text

instance IsaMember Variable where
    showIsaMember = showVarOrText
instance (IsaMember a, IsaMember b) => IsaMember (Relation a b) where
    showIsaMember = showRelation
instance IsaMember Text where
    showIsaMember = showVarOrText
instance IsaMember Label where
    showIsaMember = showVarOrLabel

class HasMember a where
    showHasMember :: a -> Text
    isMemberVar :: a -> Bool
    isMemberVar = const False
instance HasMember Variable where
    showHasMember (Var a) = "$" <> a
    isMemberVar = const True
instance HasMember Label where
    showHasMember (Lbl a) = a
instance HasMember Text where
    showHasMember a = "\"" <> a <> "\""

instance (VarOrLit a) => HasMember (Calculation a) where
    showHasMember (R_LT a) = "< " <> (showVarOrLit  a)
    showHasMember (R_GT a) = "> " <> (showVarOrLit a)
    showHasMember (R_GE a) = ">= " <> (showVarOrLit a)
    showHasMember (R_LE a) = "<= " <> (showVarOrLit a)
    showHasMember (R_EQ a) = "= " <> (showVarOrLit a)
    showHasMember (R_NEQ a) = "!= " <> (showVarOrLit a)


class Show a => QShow a where
    qshow :: a -> Text
    qshow = pack . show

instance QShow Text where
    qshow a = "\"" <> a <> "\""
instance QShow Bool where
instance QShow Int where
instance QShow Double where
instance QShow Date where
    qshow (Date y m d) = (pack $ show y)
                       <> "-"
                       <> (pack $ show m)
                       <> "-"
                       <> (pack $ show d)

data Date = Date Int Int Int
    deriving Show

class QShowOrCalc a where
    showQShowOrCalc :: a -> Text
instance VarOrLit a => QShowOrCalc (Calculation a) where
    showQShowOrCalc = showHasMember
instance QShowOrCalc Text where
    showQShowOrCalc = qshow
instance QShowOrCalc Bool where
    showQShowOrCalc = qshow
instance QShowOrCalc Int where
    showQShowOrCalc = qshow
instance QShowOrCalc Double where
    showQShowOrCalc = qshow
instance QShowOrCalc Date where
    showQShowOrCalc = qshow

class VarOrLit a where
    showVarOrLit :: a -> Text
instance VarOrLit Variable where
    showVarOrLit (Var a) = "$" <> a
instance VarOrLit Text where
    showVarOrLit a = "\"" <> a <> "\""
instance VarOrLit Bool where
    showVarOrLit True = pack "true"
    showVarOrLit False = pack "false"
instance VarOrLit Int where
    showVarOrLit = pack . show
instance VarOrLit Double where
    showVarOrLit = pack . show
instance VarOrLit Label where
    showVarOrLit (Lbl a) = a

class VarOrText a where
    showVarOrText :: a -> Text
instance VarOrText Variable where
    showVarOrText (Var a) = "$" <> a
instance VarOrText Text where
    showVarOrText a = "\"" <> a <> "\"" 

class VarOrLabel a where
    showVarOrLabel :: a -> Text
    isVar :: a -> Bool
    isVar = const False

instance VarOrLabel Variable where
    showVarOrLabel (Var a) = "$" <> a
    isVar = const True
instance VarOrLabel Label where
    showVarOrLabel (Lbl a) = a

data ISA
data REL
data SUB


startsWithOneOf :: Text -> [Text] -> Bool
startsWithOneOf a bs = foldr ((||) . startsWith a) False bs

startsWith :: Text -> Text -> Bool
startsWith a b = T.take (T.length b) a == b

isOneOf :: Text -> [Text] -> Bool
isOneOf t as = foldr (\x acc -> t==x || acc) False as

removeTrailing :: Text -> Text
removeTrailing a = applyProps a   
    where
        applyProps x = foldr (\(p,c) a -> 
                            if p a 
                               then c a
                               else a)
                           x props
        props :: [(Text->Bool, Text->Text)]
        props = [((`startsWithOneOf` [";",","]),T.drop 1)
                ,((`startsWithOneOf` ["; ",", "]),T.drop 2)
                ,((`startsWithOneOf` [" ;"," ,"]) . T.reverse,
                        T.reverse . T.drop 2 . T.reverse)]


compilePattern :: Pattern a -> Text
compilePattern p = postProcess $ compile p
  where 
    postProcess a = T.replace ", ;" ";" 
                  $ a
    compile :: Pattern a -> Text
    compile (EndP) = ""
    compile (Isa v (Just v') n) = "; "
                                <> showVarOrLabel v 
                                <> " isa " <> showVarOrLabel v'
                                <> compile n
    compile (Isa v Nothing n)   = " isa " <> showVarOrLabel v 
                                <> compile n
    compile (Isa v (Just v') n) = "; " 
                                <> showVarOrLabel v 
                                <> " isa " <> showVarOrLabel v'
                                <> compile n
    compile (Isa' v Nothing n)  = " isa! " <> showVarOrLabel v 
                                <> compile n
    compile (Isa' v (Just v') n)= "; " 
                                <> showVarOrLabel v 
                                <> " isa! " <> showVarOrLabel v'
                                <> compile n
    compile (Has v m n)         = (if isVar v then "; " else ", has ")
                                <> showVarOrLabel v <> " "
                                <> (if isVar v 
                                        && isMemberVar m then "has " else "")
                                <> showHasMember m
                                <> compile n
    compile (VarHas v v' m n)   = "; "
                                <> showVarOrLit v <> " has "
                                <> showVarOrLit v' <> " "
                                <> showVarOrText m
                                <> compile n
    compile (HasTitle v m n)    = ", "
                                <> showVarOrLit v <> " has title "
                                <> showHasMember m
                                <> compile n
    compile (AttributeMatch v q n)
                                = "; " <> showVarOrLit v <> " "
                                <> showQShowOrCalc q
                                <> compile n
    compile (Like v t n)
                                = "; " <> showVarOrLit v 
                                <> " like "
                                <> showVarOrText t
                                <> compile n
    compile (RelMatch (Just var) [] pat n) 
                                = "; ("
                                <> showVarOrLabel var <> ")"
                                <> compile pat
                                <> compile n
    compile (RelMatch (Just var) rel pat n) 
                                = "; "
                                <> showVarOrLabel var <> " "
                                <> showRelation rel
                                <> compile pat
                                <> compile n
    compile (RelMatch Nothing rel pat n) 
                                = "; "
                                <> showRelation rel
                                <> compile pat
                                <> compile n
    compile (Contains v a n)    = "; "
                                <> showVarOrLit v 
                                <> " contains "
                                <> qshow a
                                <> compile n
    compile (Iid v t n)         = "; " 
                                <> showVarOrLit v
                                <> " iid "
                                <> t
                                <> compile n
    compile (Sub v m n)         = "; "
                                <> showVarOrLabel v 
                                <> " sub "
                                <> showVarOrLabel m
                                <> compile n
    compile (Sub' v m n)        = "; "
                                <> showVarOrLabel v 
                                <> " sub! "
                                <> showVarOrLabel m
                                <> compile n
    compile (Type_ v m n)       = "; "
                                <> showVarOrLabel v 
                                <> " type "
                                <> showVarOrLabel m
                                <> compile n
    compile (Relates v v' (Just v'') n)
                                = "; "
                                <> showVarOrLabel v 
                                <> " relates "
                                <> showVarOrLabel v'
                                <> " as "
                                <> showVarOrLabel v''
                                <> compile n
    compile (Relates v v' Nothing n)
                                = "; "
                                <> showVarOrLabel v 
                                <> " relates "
                                <> showVarOrLabel v'
                                <> compile n
    compile (Relates_ l n)      = ", relates "
                                <> showVarOrLabel l
                                <> compile n
    compile (RelatesAs l l' n)  = ", relates "
                                <> showVarOrLabel l
                                <> " as "
                                <> showVarOrLabel l'
                                <> compile n
    compile (Plays v r n)       = "; "
                                <> showVarOrLabel v 
                                <> " plays "
                                <> showRolePlay r
                                <> compile n
    compile (Plays_ r n)        = ", plays "
                                <> showRolePlay r
                                <> compile n
    compile (Or a b n)          = "; { " 
                                <> (removeTrailing $ compilePattern a)
                                <> "; }"
                                <> " or "
                                <> "{ " 
                                <> (removeTrailing $ compilePattern b)
                                <> "; }"
                                <> compile n
    compile (Not v c n)         = "; not { " 
                                <> showVarOrText v 
                                <> " "
                                <> showQShowOrCalc c
                                <> "; }"
                                <> compile n
    compile (Own l False n)     = ", owns "
                                <> showVarOrLabel l
                                <> compile n
    compile (Own l True n)      = ", owns "
                                <> showVarOrLabel l
                                <> " @key"
                                <> compile n
    compile (Own' l l' n)       = "; "
                                <> showVarOrLabel l
                                <> " owns "
                                <> showVarOrLabel l'
                                <> ", "
                                <> compile n
    compile (Abstract n)        = ", abstract"
                                <> compile n
    compile (Value v n)         = ", value "
                                <> showVarOrLabel v
                                <> compile n
    compile (Regex t n)         = ", regex "
                                <> showVarOrText t
                                <> compile n

-- friend-request relates $x as located
--newtype Relation a b = Rel { fromRel :: (IsaMember a, IsaMember b) => [RolePlay a b]}
showRelation :: (IsaMember a, IsaMember b) => Relation a b -> Text
showRelation rel        = "( "
                        <> (intercalate ", " $ map showRolePlay $ fromRel rel)
                        <> " )"

-- data RolePlay a b = RP { role :: IsaMember a => Maybe a, player :: IsaMember b => b }
showRolePlay :: (IsaMember a, IsaMember b) => RolePlay a b -> Text
showRolePlay (RP (Just role) player) = showIsaMember role 
                                     <> ": " 
                                     <> showIsaMember player
showRolePlay (RP Nothing player)     = showIsaMember player

data Pattern next where
    EndP :: Pattern ()
    Isa :: (VarOrLabel v,VarOrLabel v') => v -> Maybe v' -> Pattern next -> Pattern ISA
    Isa' :: (VarOrLabel v, VarOrLabel v') => v -> Maybe v' -> Pattern next -> Pattern ISA
    Has :: (VarOrLabel v, HasMember m') => v -> m' -> Pattern next -> Pattern next'
    VarHas :: (VarOrLit v, VarOrLit v', VarOrText m') => v -> v' -> m' -> Pattern next -> Pattern next'
    HasTitle :: (VarOrLit v, HasMember m') => v -> m' -> Pattern next -> Pattern next'
    AttributeMatch :: QShowOrCalc a => Variable -> a -> Pattern next -> Pattern next'
    Like :: Variable -> Text -> Pattern next -> Pattern next'
    RelMatch :: (IsaMember a, IsaMember b) => Maybe Variable -> Relation a b -> Pattern ISA -> Pattern next -> Pattern REL
    Contains :: QShow a => Variable ->  a -> Pattern next -> Pattern next'
    Iid :: Variable -> Text -> Pattern next -> Pattern next'
    Sub :: (VarOrLabel v, VarOrLabel v') => v -> v' -> Pattern next -> Pattern SUB
    Sub' :: (VarOrLabel v, VarOrLabel v') => v -> v' -> Pattern next -> Pattern SUB
    Type_ :: (VarOrLabel v, VarOrLabel v') => v -> v' -> Pattern next -> Pattern next'
    Relates :: (VarOrLabel v, VarOrLabel v', VarOrLabel v'') => v -> v' -> Maybe v'' -> Pattern next -> Pattern next'
    Relates_ :: Label -> Pattern next -> Pattern next'
    RelatesAs :: Label -> Label -> Pattern next -> Pattern next'
    Plays :: (VarOrLabel v,IsaMember a, IsaMember b) => v -> RolePlay a b -> Pattern next -> Pattern next'
    Plays_ :: (IsaMember a, IsaMember b) => RolePlay a b -> Pattern next -> Pattern next'
    Or :: Pattern a -> Pattern b -> Pattern next -> Pattern next'
    Not :: (VarOrText v, VarOrLit v', Show v') => v -> Calculation v' -> Pattern next -> Pattern next'
    Own :: Label -> Bool -> Pattern next -> Pattern next'
    Own' :: Label -> Label -> Pattern next -> Pattern next'
    Abstract :: Pattern next -> Pattern next'
    Value :: Label -> Pattern next -> Pattern next'
    Regex :: Text -> Pattern next -> Pattern next'

regex :: Text -> Pattern next -> Pattern next'
regex = Regex

value :: Label -> Pattern next -> Pattern next'
value = Value

relatedAs :: Label -> Label -> Pattern next -> Pattern next'
relatedAs = RelatesAs

relates_ :: Label -> Pattern next -> Pattern next'
relates_ = Relates_

abstract :: Pattern next -> Pattern next'
abstract = Abstract

owns :: Label -> Pattern next -> Pattern next'
owns l = Own l False

owns' :: Label -> Label -> Pattern next -> Pattern next'
owns' = Own'

ownsKey :: Label -> Pattern next -> Pattern next'
ownsKey l = Own l True

isa :: Variable -> Label -> (Pattern next -> Pattern ISA)
isa var mt = Isa var (Just mt)

isa_ :: Label -> (Pattern next -> Pattern ISA)
isa_ t = Isa t (Nothing :: Maybe Label)

isa' :: Variable -> Label -> (Pattern next -> Pattern ISA)
isa' var mt = Isa' var (Just mt)

isa'_ :: Variable -> (Pattern next -> Pattern ISA)
isa'_ t = Isa' t (Nothing :: Maybe Label)


has :: Label -> Variable -> Pattern next -> Pattern next'
has t var = Has t var

labelHasValue :: Label -> Text -> Pattern next -> Pattern next'
labelHasValue t var = Has t var

labelMatches :: Label -> Variable -> Pattern next -> Pattern next'
labelMatches t var = Has t var

hasValue :: (VarOrLit a) => Label -> Calculation a -> Pattern next -> Pattern next'
hasValue t var = Has t var

hasValue' :: VarField -> Text -> Pattern next -> Pattern next'
hasValue' (VF a b) t = VarHas a b t

data VarField = VF { _v :: Variable, _l :: Label }

varField :: Variable -> Label ->  VarField
varField = VF

calculatedBy :: (VarOrLabel v, VarOrLit a) => v -> Calculation a -> Pattern next -> Pattern next'
calculatedBy t var = Has t var

generallyHas :: Variable -> Variable -> Pattern next -> Pattern next'
generallyHas v v' = Has v v'


relName :: Variable -> Pattern REL -> Pattern REL
relName v (RelMatch _ r i n) = RelMatch (Just v) r i n

isaRel :: (IsaMember a, IsaMember b) =>  Relation a b -> Pattern ISA -> Pattern next -> Pattern REL
isaRel r i n = RelMatch (Nothing :: Maybe Variable) r i n


emptyIsaRel :: Pattern ISA -> Pattern next -> Pattern REL
emptyIsaRel i n = RelMatch (Nothing :: Maybe Variable) θ i n

isaRel_ :: (IsaMember a, IsaMember b) => Relation a b -> Label -> Pattern REL
isaRel_ r i = RelMatch (Nothing :: Maybe Variable) r 
                       (isa_ i $ ε) 
            $ ε


isaRel' :: (IsaMember a, IsaMember b) => Relation a b -> Label -> Pattern next -> Pattern REL
isaRel' r i n = RelMatch (Nothing :: Maybe Variable) r 
                       (isa_ i $ ε) 
            $ n


like :: Variable -> Text -> Pattern next -> Pattern next'
like v t = Like v t


matches :: Variable -> Text -> Pattern next -> Pattern next'
matches n t = AttributeMatch n t

matchesDate :: Variable -> Date -> Pattern next -> Pattern next'
matchesDate n d = AttributeMatch n d

contains :: Variable -> Text -> Pattern next -> Pattern next'
contains v t = Contains v t 


or :: Pattern a -> Pattern b -> Pattern next -> Pattern next'
or a b = Or a b 

iid :: Variable -> Text -> Pattern next -> Pattern next'
iid v t = Iid v t 

sub :: VarOrLabel v => v -> Label -> Pattern next -> Pattern SUB
sub v t = Sub v t 

sub' :: VarOrLabel v => v -> Label -> Pattern next -> Pattern SUB
sub' v t = Sub' v t 

typeOf :: Variable -> Label -> Pattern next -> Pattern next'
typeOf v t = Type_ v t 

relates :: Label -> Variable -> Pattern next -> Pattern next'
relates t v = Relates t v (Nothing :: Maybe Label)

relates' :: Label -> Variable -> Pattern next'
relates' t v = Relates t v (Nothing :: Maybe Label) EndP

as :: Pattern a -> Label -> Pattern next -> Pattern next'
as (Relates t v _ _) a = Relates t v (Just a)

plays :: (IsaMember a, IsaMember b) => Variable -> RolePlay a b -> Pattern next -> Pattern next'
plays v r = Plays v r 

plays_ :: (IsaMember a, IsaMember b) => RolePlay a b -> Pattern next -> Pattern next'
plays_ r = Plays_ r 

hasTitle :: Variable -> Variable -> Pattern next -> Pattern next'
hasTitle v v' = HasTitle v v'

appendToRel :: Pattern REL -> Pattern b -> Pattern REL
appendToRel (RelMatch c d a _) b = RelMatch c d a b 

appendToIsa :: Pattern ISA -> Pattern b -> Pattern ISA
appendToIsa (Isa v v' _) b = Isa v v' b 
appendToIsa (Isa' v v' _) b = Isa v v' b

appendToSub :: Pattern SUB -> Pattern b -> Pattern SUB
appendToSub (Sub v v' _) b = Sub v v' b
appendToSub (Sub' v v' _) b = Sub v v' b

append :: Pattern () -> Pattern b -> Pattern b
append (EndP) b = b
append (Has v m' _) b = Has v m' b
append (HasTitle m' t _) b = HasTitle m' t b
append (AttributeMatch v a _) b = AttributeMatch v a b
append (Contains v a _) b = Contains v a b
append (Iid v t _) b = Iid v t b


data TypeDBOrdering = ASC | DEC
data Modifier = Limit Int | Order Text | Offset Int
              | Sort Variable TypeDBOrdering

showOrdering :: TypeDBOrdering -> Text
showOrdering ASC = "asc"
showOrdering DEC = "dec"


showModifier :: Modifier -> Text
showModifier (Limit i) = "limit " <> (pack $ show i)
showModifier (Order t) = "order " <> t
showModifier (Offset i) = "offset " <> (pack $ show i)
showModifier (Sort v o) = "sort " <> (showVarOrText v) <> " " <> (showOrdering o)

data GET 
data DELETE
data INSERT
data UPDATE
data COUNT
data SUM
data MAX
data MIN
data MEAN
data MEDIAN
data GROUP
data DEFINE
data UNDEFINE

data QueryType = QT_GET 
               | QT_DELETE
               | QT_INSERT
               | QT_UPDATE
               | QT_AGGREGATE
               | QT_DEFINE
               | QT_EXPLAIN
    deriving (Eq,Show,Ord)


getQueryType :: Query a -> [QueryType]
getQueryType = sort . qt
    where
    qt :: Query a -> [QueryType]
    qt = \case
        (Get _ _) -> [QT_GET]
        (Get' _ _ _) -> [QT_GET]
        (Get_ _) -> [QT_GET]
        (Delete _ _) -> [QT_DELETE]
        (Delete' _ _ _) -> [QT_DELETE]
        (DeleteRel _ _ _) -> [QT_DELETE]
        (DeleteRel' _ _ _ _) -> [QT_DELETE]
        (Insert _ _) -> [QT_INSERT]
        (InsertRel _ _ _) -> [QT_INSERT]
        (Update _ _) -> [QT_UPDATE]
        (Count a _) -> QT_AGGREGATE : qt a
        (Sum a _) -> QT_AGGREGATE : qt a
        (Max a _) -> QT_AGGREGATE : qt a
        (Min a _) -> QT_AGGREGATE : qt a
        (Mean a _) -> QT_AGGREGATE : qt a
        (Median a _) -> QT_AGGREGATE : qt a
        (Group a _) -> QT_AGGREGATE : qt a
        (Define _) -> [QT_DEFINE]
        (Undefine _) -> [QT_DEFINE]
    

compileQuery :: Query a -> Text
compileQuery (Get p vs) = "match "
                        <> (removeTrailing $ compilePattern p)
                        <> "; get "
                        <> (T.intercalate ", " $ map showVarOrLabel vs)
                        <> ";"
compileQuery (Get' p vs mods)
                        = "match "
                        <> (removeTrailing $ compilePattern p)
                        <> "; get "
                        <> (T.intercalate ", " $ map showVarOrLabel vs)
                        <> "; "
                        <> showMods mods
                        <> ";"
compileQuery (Get_ p)   = "match "
                        <> (removeTrailing $ compilePattern p)
                        <> "; get;"
compileQuery (Delete m d)
                        = "match "
                        <> (removeTrailing $ compilePattern m)
                        <> "; delete "
                        <> (removeTrailing $ compilePattern d)
                        <> ";"
compileQuery (Delete' m d mods)
                        = "match "
                        <> (removeTrailing $ compilePattern m)
                        <> "; delete "
                        <> (removeTrailing $ compilePattern d)
                        <> "; "
                        <> showMods mods
                        <> ";"
compileQuery (DeleteRel p Nothing  rel) 
                        = "match "
                        <> (removeTrailing $ compilePattern p)
                        <> "; delete "
                        <> showRelation rel
                        <> ";"
compileQuery (DeleteRel p (Just a)  rel) 
                        = "match "
                        <> (removeTrailing $ compilePattern p)
                        <> "; delete "
                        <> showVarOrLabel a
                        <> " "
                        <> showRelation rel
                        <> ";"
compileQuery (DeleteRel' p Nothing  rel mods) 
                        = "match "
                        <> (removeTrailing $ compilePattern p)
                        <> "; delete "
                        <> showRelation rel
                        <> "; "
                        <> showMods mods 
                        <> ";"
compileQuery (DeleteRel' p (Just a)  rel mods) 
                        = "match "
                        <> (removeTrailing $ compilePattern p)
                        <> "; delete "
                        <> showVarOrLabel a
                        <> " "
                        <> showRelation rel
                        <> "; "
                        <> showMods mods 
                        <> ";"
compileQuery (Insert Nothing p) 
                        = "insert "
                        <> (removeTrailing $ compilePattern p)
                        <> ";"
compileQuery (Insert (Just pa) p) 
                        = "match "
                        <> (removeTrailing $ compilePattern pa)
                        <> "; insert "
                        <> (removeTrailing $ compilePattern p)
                        <> ";"
compileQuery (InsertRel Nothing Nothing r)
                        = "insert "
                        <> showRelation r
                        <> ";"
compileQuery (InsertRel Nothing (Just var) r)
                        = "insert "
                        <> showVarOrLabel var
                        <> " "
                        <> showRelation r
                        <> ";"
compileQuery (InsertRel (Just p) (Just var) r)
                        = "match "
                        <> (removeTrailing $ compilePattern p)
                        <> "insert "
                        <> showVarOrLabel var
                        <> " "
                        <> showRelation r
                        <> ";"
             -- other cases for InsertRel ruled out by smart constructors
compileQuery (Update del ins) 
                        = compileQuery del
                        <> " "
                        <> compileQuery ins
compileQuery (Count q vars) 
                        = compileQuery q
                        <> " count"
                        <> (if length vars > 0 then " " else "")
                        <> (T.intercalate ", " $ map showVarOrLabel vars)
                        <> ";"
compileQuery (Sum q vars) 
                        = compileQuery q
                        <> " sum"
                        <> (if length vars > 0 then " " else "")
                        <> (T.intercalate ", " $ map showVarOrLabel vars)
                        <> ";"
compileQuery (Max q vars) 
                        = compileQuery q
                        <> " max"
                        <> (if length vars > 0 then " " else "")
                        <> (T.intercalate ", " $ map showVarOrLabel vars)
                        <> ";"
compileQuery (Min q vars) 
                        = compileQuery q
                        <> " min"
                        <> (if length vars > 0 then " " else "")
                        <> (T.intercalate ", " $ map showVarOrLabel vars)
                        <> ";"
compileQuery (Mean q vars) 
                        = compileQuery q
                        <> " mean"
                        <> (if length vars > 0 then " " else "")
                        <> (T.intercalate ", " $ map showVarOrLabel vars)
                        <> ";"
compileQuery (Median q vars) 
                        = compileQuery q
                        <> " median"
                        <> (if length vars > 0 then " " else "")
                        <> (T.intercalate ", " $ map showVarOrLabel vars)
                        <> ";"
compileQuery (Group q vars) 
                        = compileQuery q
                        <> " group"
                        <> (if length vars > 0 then " " else "")
                        <> (T.intercalate ", " $ map showVarOrLabel vars)
                        <> ";"
compileQuery (Define p) = "define "
                        <> (removeTrailing $ compilePattern p)
                        <> ";"
compileQuery (Undefine p) = "undefine "
                        <> (removeTrailing $ compilePattern p)
                        <> ";"

showMods :: [Modifier] -> Text
showMods mods = (T.intercalate ", " $ map showModifier mods)

-- TODO: EXPLAIN && COMPUTE!!
data Query a where
    Get :: Pattern a -> [Variable] -> Query GET
    Get' :: Pattern a -> [Variable] -> [Modifier] -> Query GET
    Get_ :: Pattern a -> Query GET
    Delete :: Pattern a -> Pattern b -> Query DELETE
    Delete' :: Pattern a -> Pattern b -> [Modifier] -> Query DELETE
    DeleteRel :: (IsaMember b, IsaMember c) => Pattern a -> Maybe Variable -> Relation b c -> Query DELETE
    DeleteRel' :: (IsaMember b, IsaMember c) => Pattern a -> Maybe Variable -> Relation b c -> [Modifier] -> Query DELETE
    Insert :: Maybe (Pattern a) -> Pattern b -> Query INSERT
    InsertRel :: (IsaMember b, IsaMember c) => Maybe (Pattern a) -> Maybe Variable-> Relation b c -> Query INSERT
    Update :: Query DELETE -> Query INSERT -> Query UPDATE
    Count :: Query a -> [Variable] -> Query COUNT
    Sum :: Query a -> [Variable] -> Query SUM
    Max :: Query a -> [Variable] -> Query MAX
    Min :: Query a -> [Variable] -> Query MIN
    Mean :: Query a -> [Variable] -> Query MEAN
    Median :: Query a -> [Variable] -> Query MEDIAN
    Group :: Query a -> [Variable] -> Query GROUP
    Define :: Pattern a -> Query DEFINE
    Undefine :: Pattern a -> Query UNDEFINE



undefine :: Pattern a -> Query UNDEFINE 
undefine = Undefine

match :: Pattern a -> Pattern a
match = id

get :: Pattern a -> [Variable] -> Query GET 
get = Get

get' :: Pattern a -> [Variable] -> ([Modifier] -> Query GET)
get' = Get'


ε :: Pattern ()
ε = (EndP)


e :: Pattern ()
e = (EndP)


delete :: Pattern a -> Pattern b -> Query DELETE 
delete = Delete

deleteRelation :: (IsaMember b, IsaMember c) => Pattern a -> Variable -> Relation b c -> Query DELETE
deleteRelation p v r = DeleteRel p (Just v) r

deleteRelation' :: (IsaMember b, IsaMember c) => Pattern a -> Variable -> Relation b c -> ([Modifier] -> Query DELETE)
deleteRelation' p v r = DeleteRel' p (Just v) r

delete' :: Pattern a -> Pattern b-> ([Modifier] -> Query DELETE)
delete' = Delete'

forWhich :: a -> a
forWhich = id
asWellAs :: a -> a
asWellAs = id
insert :: Pattern a -> Pattern b -> Query INSERT
insert a b = Insert (Just a) b

update :: Query DELETE -> Query INSERT -> Query UPDATE
update a b = Update a b

insert_ :: Pattern b -> Query INSERT
insert_ b = Insert (Nothing :: Maybe (Pattern ())) b



insertRelation :: (IsaMember b, IsaMember c) => Pattern a -> Variable -> Relation b c -> Query INSERT
insertRelation p v r = InsertRel (Just p) (Just v) r

insertRelation_ :: (IsaMember b, IsaMember c) => Variable -> Relation b c -> Query INSERT
insertRelation_ v r = InsertRel (Nothing:: Maybe (Pattern ())) (Just v) r

not :: (VarOrText v, VarOrLit v', Show v') => v -> Calculation v' -> Pattern next -> Pattern next'
not = Not


count :: Query a -> [Variable] -> Query COUNT
count = Count

count_ :: Query a -> Query COUNT
count_ a = Count a []

sum_ :: Query GET -> Query SUM
sum_ a = Sum a []

sum :: Query GET -> [Variable] -> Query SUM
sum = Sum

(!$) :: a -> (a -> b) -> b
(!$) = flip ($)
emptyRel :: Relation Text Text 
emptyRel = []

θ :: Relation Text Text
θ = emptyRel 

max_ :: Query GET -> Query MAX
max_ a = Max a []

max :: Query GET -> [Variable] -> Query MAX
max = Max

min_ :: Query GET -> Query MIN
min_ a = Min a []

min :: Query GET -> [Variable] -> Query MIN
min = Min

mean_ :: Query GET -> Query MEAN
mean_ a = Mean a []

mean :: Query GET -> [Variable] -> Query MEAN
mean = Mean


median :: Query GET -> [Variable] -> Query MEDIAN
median = Median

median_ :: Query GET -> Query MEDIAN
median_ a = Median a []


group :: Query GET -> [Variable] -> Query GROUP
group = Group

group_ :: Query GET -> Query GROUP
group_ a = Group a []

partOfRel :: Variable -> Label -> Pattern next -> Pattern REL
partOfRel v t = relName v
                . emptyIsaRel (isa_ t $ε)




define :: Pattern a -> Query DEFINE
define = Define

                    
[defLbls| ;entity;relation;attribute;string;datetime |]

