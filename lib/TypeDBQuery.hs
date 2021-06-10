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
import Data.Text (Text, pack)
import Prelude hiding (String,or,not,sum,max, min)
import GHC.Exts (IsList(..),IsString(..))
import TypeDBTH

data Calculation a = R_LT a
                   | R_GT a
                   | R_GE a
                   | R_LE a
                   | R_EQ a
                   | R_NEQ a

newtype Variable = Var { fromVar :: Text }
newtype Label = Lbl { fromLbl :: Text }

instance IsString Label where
    fromString = Lbl . pack

data RolePlay a b = RP { role :: IsaMember a => Maybe a, player :: IsaMember b => b }

sRP :: IsaMember b => b -> RolePlay Text b
sRP b = RP Nothing b

rp :: (IsaMember b) => Text -> b -> RolePlay Text b
rp a b = RP (Just a) b

newtype Relation a b = Rel { fromRel :: (IsaMember a, IsaMember b) => [RolePlay a b]}

instance (IsaMember a, IsaMember b) => IsList (Relation a b) where
    type Item (Relation a b) = (RolePlay a b)
    fromList = Rel
    toList = fromRel


class IsaMember a where

instance IsaMember Variable where
instance IsaMember (Relation a b) where
instance IsaMember Text where

class HasMember a where
instance HasMember Variable where
instance HasMember Label where
instance HasMember (Calculation a) where

class Show a => QShow a where
    qshow :: a -> Text
    qshow = pack . show

instance QShow Text where
instance QShow Bool where
instance QShow Int where
instance QShow Double where

class QShowOrCalc a where
instance QShowOrCalc (Calculation a) where
instance QShowOrCalc Text where
instance QShowOrCalc Bool where
instance QShowOrCalc Int where
instance QShowOrCalc Double where

class VarOrLit a where
instance VarOrLit Variable where
instance VarOrLit Text where
instance VarOrLit Bool where
instance VarOrLit Int where
instance VarOrLit Double where
instance VarOrLit Label where

class VarOrText a where
instance VarOrText Variable where
instance VarOrText Text where

data ISA
data REL

data Pattern next where
    EndP :: Pattern ()
    Isa :: (VarOrText v,VarOrText v') => v -> Maybe v' -> Pattern next -> Pattern ISA
    Isa' :: (VarOrText v, VarOrText v') => v -> Maybe v' -> Pattern next -> Pattern ISA
    Has :: (VarOrLit v, HasMember m') => v -> m' -> Pattern next -> Pattern next'
    VarHas :: (VarOrLit v, VarOrLit v', VarOrText m') => v -> v' -> m' -> Pattern next -> Pattern next'
    HasTitle :: (VarOrLit v, HasMember m') => v -> m' -> Pattern next -> Pattern next'
    AttributeMatch :: QShowOrCalc a => Variable -> a -> Pattern next -> Pattern next'
    RelMatch :: Maybe Variable -> Relation a b -> Pattern ISA -> Pattern next -> Pattern REL
    Contains :: QShow a => Variable ->  a -> Pattern next -> Pattern next'
    Like :: Variable -> Text -> Pattern next -> Pattern next'
    Iid :: Variable -> Text -> Pattern next -> Pattern next'
    Sub :: (VarOrText v, VarOrText v') => v -> v' -> Pattern next -> Pattern next'
    Sub' :: (VarOrText v, VarOrText v') => v -> v' -> Pattern next -> Pattern next'
    Type_ :: (VarOrText v, VarOrText v') => v -> v' -> Pattern next -> Pattern next'
    Relates :: (VarOrText v, VarOrText v', VarOrText v'') => v -> v' -> Maybe v'' -> Pattern next -> Pattern next'
    Plays :: (VarOrText v) => v -> RolePlay a b -> Pattern next -> Pattern next'
    Or :: Pattern a -> Pattern b -> Pattern next -> Pattern next'
    Not :: (VarOrText v, VarOrText v') => v -> Calculation v' -> Pattern next -> Pattern next'

-- $p isa person
pIsaPerson :: Pattern ISA
pIsaPerson = (Var "p") `isa` "person" $ EndP

isa :: Variable -> Text -> (Pattern next -> Pattern ISA)
isa var mt = Isa var (Just mt)

isa_ :: Text -> (Pattern next -> Pattern ISA)
isa_ t = Isa t (Nothing :: Maybe Text)

isa' :: Variable -> Text -> (Pattern next -> Pattern ISA)
isa' var mt = Isa' var (Just mt)

isa'_ :: Variable -> (Pattern next -> Pattern ISA)
isa'_ t = Isa' t (Nothing :: Maybe Text)

-- $p isa person, has full-name $n
pIsaPersonHasFullNameN :: Pattern ISA
pIsaPersonHasFullNameN
  = (Var "p") `isa` "person"
        $ has "fullName" (Var "n")
        $ EndP

has :: Text -> Variable -> Pattern next -> Pattern next'
has t var = Has t var

labelHasValue :: Text -> Text -> Pattern next -> Pattern next'
labelHasValue t var = Has t (Lbl var)

labelMatches :: Text -> Variable -> Pattern next -> Pattern next'
labelMatches t var = Has t var

hasValue :: Text -> Calculation a -> Pattern next -> Pattern next'
hasValue t var = Has t var

hasValue' :: VarField -> Text -> Pattern next -> Pattern next'
hasValue' (VF a b) t = VarHas a b t

data VarField = VF { _v :: Variable, _l :: Label }

varField :: Variable -> Label ->  VarField
varField = VF

calculatedBy :: Variable -> Calculation a -> Pattern next -> Pattern next'
calculatedBy t var = Has t var

generallyHas :: Variable -> Variable -> Pattern next -> Pattern next'
generallyHas v v' = Has v v'

-- $emp (employer: $x, employee: $y) isa employment
empIsaEmployment :: Pattern REL
empIsaEmployment = relName (Var "emp")
                     $ [rp "employer" (Var "x")
                       ,rp "employee" (Var "y")]
                        `isaRel` (isa_ "employment" $ EndP)
                     $ EndP

relName :: Variable -> Pattern REL -> Pattern REL
relName v (RelMatch _ r i n) = RelMatch (Just v) r i n

isaRel :: Relation a b -> Pattern ISA -> Pattern next -> Pattern REL
isaRel r i n = RelMatch (Nothing :: Maybe Variable) r i n


emptyIsaRel :: Pattern ISA -> Pattern next -> Pattern REL
emptyIsaRel i n = RelMatch (Nothing :: Maybe Variable) θ i n

isaRel_ :: Relation a b -> Text -> Pattern REL
isaRel_ r i = RelMatch (Nothing :: Maybe Variable) r 
                       (isa_ i $ ε) 
            $ ε


isaRel' :: Relation a b -> Text -> Pattern next -> Pattern REL
isaRel' r i n = RelMatch (Nothing :: Maybe Variable) r 
                       (isa_ i $ ε) 
            $ n

-- $emp (employer: $x, employee: $y) isa employment, has reference-id $ref
empIsaEmploymentHasRepId :: Pattern REL
empIsaEmploymentHasRepId = appendToRel empIsaEmployment
                                $ "reference-id"
                                    `has` (Var "ref")
                                $ EndP
                            

-- (employer: $x, employee: $y) isa employment
nothingIsaEmployment :: Pattern REL
nothingIsaEmployment = [rp "employer" (Var "x")
                       ,rp "employee" (Var "y")]
                            `isaRel` (isa_ "employment" EndP)
                     $ EndP

-- $fr ($x, $y) isa friendship
nothingIsaFriendship :: Pattern REL
nothingIsaFriendship = relName (Var "fr")
                         $   Rel (map (sRP . Var) ["x","y"])
                                `isaRel` (isa_ "friendship" EndP)
                    $ EndP

-- $x "like"
xLike :: Pattern ()
xLike = (Var "x") `like` "like"
      $ EndP

like :: Variable -> Text -> Pattern next -> Pattern next'
like v t = AttributeMatch v t

-- $n isa nickname; $n "Mitzi"
isaNicknameMitzi :: Pattern ISA
isaNicknameMitzi 
  = n `isa` "nickname"
  $ n `matches` "Mitzi"
  $ EndP
      where n = Var "n"

matches :: Variable -> Text -> Pattern next -> Pattern next'
matches n t = AttributeMatch n t

-- $phone-number contains "+44"
phoneNumberContains :: Pattern ()
phoneNumberContains
  = (Var "phone-number") `contains` "+44"
  $ EndP

contains :: Variable -> Text -> Pattern next -> Pattern next'
contains v t = Contains v t 

-- $x like "(Miriam Morton|Solomon Tran)"
xLikeRegex :: Pattern ()
xLikeRegex 
  = (Var "x") `like` "(Miriam Morton|Solomon Tran)" 
  $ EndP

-- $p isa person, has nickname $nn, has full-name $fn
pIsaPersonNicknameFullname :: Pattern ISA
pIsaPersonNicknameFullname
  = (Var "p") `isa` "person"
  $ "nickname" `has` (Var "nn") 
  $ "full-name" `has` (Var "fn") 
  $ EndP

-- $s isa school, has ranking < 100
sIsaSchoolHasRanking :: Pattern ISA
sIsaSchoolHasRanking
  = (Var "s") `isa` "school"
  $ "ranking" `hasValue` (R_LT (100::Int)) 
  $ EndP

-- $s isa school, has ranking $r; $r < 100
sIsaSchoolHasRankingSep :: Pattern ISA
sIsaSchoolHasRankingSep 
  = (Var "s") `isa` "school"
  $ "ranking" `has` (Var "r")
  $ (Var "r") `calculatedBy` (R_LT (100::Int)) 
  $ EndP

-- $p isa person, has full-name $fn; { $fn contains "Miriam"; } or { $fn contains "Solomon"; }
personFullNameAorB :: Pattern ISA
personFullNameAorB
  = (Var "p") `isa` "person"
  $ "full-name" `has` (Var "fn")
  $ (Contains (Var "fn") ("Miriam" :: Text) EndP)
        `or` 
    (Contains (Var "fn") ("Solomon" :: Text) EndP)
  $ EndP

or :: Pattern a -> Pattern b -> Pattern next -> Pattern next'
or a b = Or a b 

-- $rr isa! romantic-relationship
isa'RomanticRelationship :: Pattern ISA
isa'RomanticRelationship 
  = (Var "rr") `isa'` "romantic-relationship" $ε

-- $x iid 0x966e80018000000000000000
xIIDSomething :: Pattern ()
xIIDSomething
  = (Var "x") `iid` "0x966e80018000000000000000"
  $ EndP

iid :: Variable -> Text -> Pattern next -> Pattern next'
iid v t = Iid v t 

-- $x sub post
xSubPost :: Pattern ()
xSubPost = (Var "x") `sub` "post"
         $ EndP

sub :: Variable -> Text -> Pattern next -> Pattern next'
sub v t = Sub v t 

sub' :: Variable -> Text -> Pattern next -> Pattern next'
sub' v t = Sub' v t 

-- $x sub! post
xSub'Post :: Pattern ()
xSub'Post = (Var "x") `sub'` "post"
          $ EndP

-- $x type post
xTypePost :: Pattern ()
xTypePost = (Var "x") `typeOf` "post"
          $ EndP

typeOf :: Variable -> Text -> Pattern next -> Pattern next'
typeOf v t = Type_ v t 

-- employment relates $x
employmentRelatesX :: Pattern ()
employmentRelatesX = "employment" `relates` (Var "x") 
                   $ EndP

relates :: Text -> Variable -> Pattern next -> Pattern next'
relates t v = Relates t v (Nothing :: Maybe Text)

relates' :: Text -> Variable -> Pattern next'
relates' t v = Relates t v (Nothing :: Maybe Text) EndP

as :: Pattern a -> Text -> Pattern next -> Pattern next'
as (Relates t v _ _) a = Relates t v (Just a)

-- friend-request relates $x as located
friendRequestRelatesXAsLocated :: Pattern ()
friendRequestRelatesXAsLocated
  = "friend-request" `relates'` (Var "x") 
        `as` "located"
  $ EndP

-- $x plays employment:employee
xPlaysEmploymentEmployee :: Pattern ()
xPlaysEmploymentEmployee 
  = (Var "x") `plays` (RP (Just ("employment"::Text)) "employee")
  $ EndP

plays :: Variable -> RolePlay a b -> Pattern next -> Pattern next'
plays v r = Plays v r 

-- $x has title $t
xHasTitleT :: Pattern ()
xHasTitleT = (Var "x") `hasTitle` (Var "t")
            $ EndP

hasTitle :: Variable -> Variable -> Pattern next -> Pattern next'
hasTitle v v' = HasTitle v v'

appendToRel :: Pattern REL -> Pattern b -> Pattern REL
appendToRel (RelMatch c d a _) b = RelMatch c d a b 

appendToIsa :: Pattern ISA -> Pattern b -> Pattern ISA
appendToIsa (Isa v v' _) b = Isa v v' b 
appendToIsa (Isa' v v' _) b = Isa v v' b

append :: Pattern () -> Pattern b -> Pattern b
append (EndP) b = b
append (Has v m' _) b = Has v m' b
append (HasTitle m' t _) b = HasTitle m' t b
append (AttributeMatch v a _) b = AttributeMatch v a b
append (Contains v a _) b = Contains v a b
append (Like v t _) b = Like v t b
append (Iid v t _) b = Iid v t b
append (Sub v v' _) b = Sub v v' b


data TypeDBOrdering = ASC | DEC
data Modifier = Limit Int | Order Text | Offset Int
              | Sort Variable TypeDBOrdering

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


data Query a where
    Get :: Pattern a -> [Variable] -> Query GET
    Get' :: Pattern a -> [Variable] -> [Modifier] -> Query GET
    Get_ :: Pattern a -> Query GET
    Delete :: Pattern a -> Pattern b -> Query DELETE
    Delete' :: Pattern a -> Pattern b -> [Modifier] -> Query DELETE
    DeleteRel :: Pattern a -> Maybe Variable -> Relation b c -> Query DELETE
    DeleteRel' :: Pattern a -> Maybe Variable -> Relation b c -> [Modifier] -> Query DELETE
    Insert :: Maybe (Pattern a) -> Pattern b -> Query INSERT
    InsertRel :: Maybe (Pattern a) -> Maybe Variable-> Relation b c -> Query INSERT
    Update :: Query DELETE -> Query INSERT -> Query UPDATE
    Count :: Query a -> [Variable] -> Query COUNT
    Sum :: Query a -> [Variable] -> Query SUM
    Max :: Query a -> [Variable] -> Query MAX
    Min :: Query a -> [Variable] -> Query MIN
    Mean :: Query a -> [Variable] -> Query MEAN
    Median :: Query a -> [Variable] -> Query MEDIAN
    Group :: Query a -> [Variable] -> Query GROUP


{-
match
$fr ($x, $y) isa friendship;
$x isa person, has full-name $x-fn;
$x-fn contains "Miriam";
$y isa person, has full-name $y-fn, has phone-number $y-pn;
get $x-fn, $y-fn, $y-pn;
-}

match :: Pattern a -> Pattern a
match = id

get :: Pattern a -> [Variable] -> Query GET 
get = Get

get' :: Pattern a -> [Variable] -> ([Modifier] -> Query GET)
get' = Get'

bigMatch :: Query GET
bigMatch = match 
         ( relName fr
         $ Rel (map (sRP . Var) ["x","y"])
             `isaRel` (isa_ "friendship" EndP)
         $ x `isa` "person"
         $ has "full-name" x_fn
         $ x_fn `contains` "Miriam"
         $ y `isa` "person"
         $ has "full-name" y_fn
         $ has "phone-number" y_pn
         $ EndP )
          `get` [x_fn,y_fn,y_pn]

    where 
        fr = Var "fr"
        x = Var "x"
        y = Var "y"
        x_fn = Var "x-fn"
        y_fn = Var "y-fn"
        y_pn = Var "y-pn"

ε :: Pattern ()
ε = (EndP)


e :: Pattern ()
e = (EndP)

-- match $p isa person; get $p; limit 1;
smallMatch :: Query GET
smallMatch = match 
           ( p `isa` "person" $ ε)
                `get'` [p] $ [Limit 1]
    where 
        p = Var "p"

-- match $p isa person, has full-name $fn; get $fn; sort $fn asc;
matchWithSort :: Query GET
matchWithSort = match
              ( p `isa` "person" 
              $ has "full-name" fn $ ε)
                `get'` [fn] $ [Sort fn ASC]
        where
            [p,fn]= map Var ["p","fn"]
                

delete :: Pattern a -> Pattern b -> Query DELETE 
delete = Delete

deleteRelation :: Pattern a -> Variable -> Relation b c -> Query DELETE
deleteRelation p v r = DeleteRel p (Just v) r

deleteRelation' :: Pattern a -> Variable -> Relation b c -> ([Modifier] -> Query DELETE)
deleteRelation' p v r = DeleteRel' p (Just v) r

delete' :: Pattern a -> Pattern b-> ([Modifier] -> Query DELETE)
delete' = Delete'

{- match $p isa person
    , has email "someMail"; 
   delete $p isa person;
-}
deletePersonByMail :: Query DELETE
deletePersonByMail 
    = match
    ( p `isa` "person"
    $ forWhich "email" `labelHasValue` "someMail" $ ε)
        `delete` (p `isa` "person" $ ε)
    where
        p = Var "p"

forWhich :: a -> a
forWhich = id
asWellAs :: a -> a
asWellAs = id
{- match
    $org isa organisation, has name "Pharos";
    $emp (employer: $org, employee: $p) isa employment;
    delete $emp isa employment;
-}
removeEmployee :: Query DELETE
removeEmployee 
    = match 
    ( org `isa` "organisation" 
    $ forWhich "name" `labelHasValue` "Pharos"
    $ relName emp 
        $ [rp "employer" org, rp "employee" p]
            `isaRel_` "friendship")
        `delete` (emp `isa` "employment" $ ε)
    where
        [emp,org,p] = map Var ["emp","org","p"]

-- match $t isa travel, has start-date $st; $d 2013-12-22; delete $t has $st;
deleteAttribute :: Query DELETE
deleteAttribute
    = match 
    ( t `isa` "travel"
    $ has "start-date" st
    $ asWellAs d `matches` "2013-12-22" $ε)
        `delete` (t `generallyHas` st $ε)
    where
        [t,st,d] = map Var ["t","st","d"]

{- match
    $org isa organisation, has name "Pharos";
    $emp (employer: $org, employee: $p) isa employment;
    delete $emp (employer: $org);
-}
deleteRolePlayers :: Query DELETE
deleteRolePlayers 
    = match
    ( org `isa` "organisation"
    $ forWhich "name" `labelHasValue` "Pharos"
    $ relName emp
        $ [rp "employer" org,rp "employee" p]
            `isaRel_` "employment")
    `deleteRelation` emp $ [rp "employer" org]
    where 
        [org,p,emp] = map Var ["org","p","emp"]

insert :: Pattern a -> Pattern b -> Query INSERT
insert a b = Insert (Just a) b

update :: Query DELETE -> Query INSERT -> Query UPDATE
update a b = Update a b

insert_ :: Pattern b -> Query INSERT
insert_ b = Insert (Nothing :: Maybe (Pattern ())) b



insertRelation :: Pattern a -> Variable -> Relation b c -> Query INSERT
insertRelation p v r = InsertRel (Just p) (Just v) r

insertRelation_ :: Variable -> Relation b c -> Query INSERT
insertRelation_ v r = InsertRel (Nothing:: Maybe (Pattern ())) (Just v) r

{- insert $p isa person, 
    has full-name "John Parkson", 
    has gender "male", 
    has email "someEmail",
    has phone-number "+44-1234=567890";
-}
insertPerson :: Query INSERT
insertPerson 
    = insert_
    ( p `isa` "person"
    $ forWhich "full-name" `labelHasValue` "John Parkson"
    $ asWellAs "gender" `labelHasValue` "male"
    $ asWellAs "email" `labelHasValue` "someEmail"
    $ asWellAs "phone-number" `labelHasValue` "+44-1234=567890" $ε)
        where
            p = Var "p"

-- insert $x isa emotion; $x "like";
insertEmotion :: Query INSERT
insertEmotion 
    = insert_
    ( x `isa` "emotion"
    $ x `matches` "like" $ε)
        where 
            x = Var "x"

{-
match
    $org isa organisation, has name "Facelook";
    $person isa person, has email "someEmail";
insert $new-employment (employer: $org, employee: $person) isa employment;
    $new-employment has reference-id "WGFTSH";
-}
insertInstanceOfRelType :: Query INSERT
insertInstanceOfRelType
    = (match
    $ org `isa` "organisation"
    $ forWhich "name" `labelHasValue` "Facelook"
    $ person `isa` "person"
    $ forWhich "email" `labelHasValue` "someEmail" $ε)
        `insert`
    (relName new_employment
        $ [rp "employer" org, rp "employee" person]
            `isaRel'` "employment"
    $ new_employment `varField` "reference-id" 
                  `hasValue'` "WGFTSH" $ε)
    where
        [org,person,new_employment] = map Var ["org","person","new_employment"]


{-
match
    $person isa person;
insert
    $reflexive-friendship (friend: $person, friend: $person) isa friendship;
-}
duplicateRolePLayers :: Query INSERT
duplicateRolePLayers
    = match 
    ( person `isa` "person" $ε)
      `insert`
    ( relName reflexive_friendship
        $ [rp "friend" person, rp "friend" person]
        `isaRel_` "friendship")
    where 
        [reflexive_friendship, person] 
          = map Var ["reflexive_friendship", "person"]

{-
match 
    $friendship (friend: $p, friend: $p) isa friendship; 
get $friendship;
-}
matchDuplicates :: Query GET
matchDuplicates 
    = match
    ( relName friendship 
        $ [rp "friend" p, rp "friend" p]
        `isaRel_` "friendship")
    `get` [friendship]
        where
            [friendship,p] = map Var ["friendship", "p"]


not :: (VarOrText v, VarOrText v') => v -> Calculation v' -> Pattern next -> Pattern next'
not = Not

{-
match
$emp (employer: $org, employee: $p) isa employment;
    $p2 isa person;
    not { $p = $p2; };
insert $emp (employee: $p2) isa employment;
-}
extendingRelation :: Query INSERT
extendingRelation 
    = match
    ( relName emp
        $ [rp "employer" org, rp "employee" p]
        `isaRel'` "employment"
    $ p2 `isa` "person"
    $ not p (R_EQ p2) $ε)
      `insert`
    ( relName emp
        $ [rp "employee" p2]
        `isaRel_` "employemnt")
    where
        [emp,p,p2,org] = map Var ["emp","p","p2","org"]



{-
match $org isa organisation, has name "Medicely", has registration-number $rn;
delete $org has $rn;
insert $org has registration-number "81726354";
-}
updateAttributeOwnedByConcept :: Query UPDATE
updateAttributeOwnedByConcept
    = ((match
        ( org `isa` "organisation"
        $ forWhich "name" `labelHasValue` "Medicely"
        $ asWellAs "registration-number" `labelMatches` rn $ε))
      `delete` ( org `generallyHas` rn $ε))
      `update`
      (insert_ ( org `varField` "registration-number"
                        `hasValue'` "81726354" $ε))
    where 
        [org,rn] = map Var ["org","rn"]

{-
match
    $m isa media, has caption $c;
    $c contains "inappropriate word";
delete 
    $c isa caption;
insert 
    $m has caption "deleted";
-}

updateInstancesOfAttribute :: Query UPDATE
updateInstancesOfAttribute
    = update
        (( match
          $ m `isa` "media"
          $ forWhich "caption" `labelMatches` c 
          $ c `contains` "inappropriate word" $ε)
          `delete` (c `isa'` "caption" $ε))
        ( insert_ 
        $   m `varField` "caption"
                    `hasValue'` "deleted" $ε)
    where 
        [m,c] = map Var ["m","c"]
{-
match
    $org isa organisation, has name "Pharos";
    $new-org isa organisation, has name "Medicely";
    $emp (employer: $org, employee: $p) isa employment;
delete
    $emp (employer: $org);
insert
    $emp (employer: $new-org);
-}
modifyingRoleplayers :: Query UPDATE
modifyingRoleplayers
    = update 
        (( match 
         $ org `isa` "organisation"
         $ forWhich "name" `labelHasValue` "Pharos"
         $ new_org `isa` "organisation"
         $ forWhich "name" `labelHasValue` "Medicely"
         $ relName emp
            $ [rp "employer" org, rp "employee" p]
                `isaRel_` "employment")
         `deleteRelation` emp $ [rp "employer" org]
         )
        (insertRelation_ emp [rp "employer" new_org])



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
{-
match
    $sce isa studentship, has score $sco;
    $sco > 7.0;
get $sco; 
count;
-}
countQuery :: Query COUNT
countQuery = match
           ( sce `isa` "studentship"
           $ forWhich "score" `labelMatches` sco
           $ sco `calculatedBy` (R_GT 7.0) $ε)
            `get` [sco]
           !$ count_

{-
match
    $org isa organisation, has name $orn;
    $orn "Medicely";
    ($org) isa employment, has salary $sal;
get $sal; 
sum $sal;
-}
sumQuery :: Query SUM
sumQuery = match
         ( org `isa` "organisation"
         $ forWhich "name" `labelMatches` orn
         $ orn `matches` "Medicely"
         $ relName org
            $ emptyIsaRel (isa_ "employment" $ε)
         $ forWhich "salary" `labelMatches` sal $ε)
         `get` [sal]
         `sum` [sal]

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
{-
match
    $sch isa school, has ranking $ran;
get $ran; max $ran;
-}
maxQuery :: Query MAX
maxQuery = match
         ( sch `isa` "school"
         $ forWhich "ranking" `labelMatches` ran $ε)
         `get` [ran]
         `max` [ran]


partOfRel :: Variable -> Text -> Pattern next -> Pattern REL
partOfRel v t = relName v
                . emptyIsaRel (isa_ t $ε)


{-
match
    ($per) isa marriage;
    ($per) isa employment, has salary $sal;
get $sal;
min $sal;
-}
minQuery :: Query MIN
minQuery = match 
         ( per `partOfRel` "marriage"
         $ per `partOfRel` "employment"
         $ forWhich "salary" `labelMatches` sal $ε)
         `get` [sal]
         `min` [sal]

{-
match
    $emp isa employment, has salary $sal;
get $sal; 
mean $sal;
-}
meanQuery :: Query MEAN
meanQuery = match
          ( emp `isa` employment
          $ forWhich salary `labelMatches` sal $ε)
          `get` [sal]
          `mean` [sal]
          

{-
match
    $org isa organisation, has name $orn;
    $orn = "Facelook";
    (employer: $org, employee: $per) isa employment;
    ($per) isa studentship, has score $sco;
get $sco; 
median $sco;
-}
medianQuery :: Query MEDIAN
medianQuery = match
            ( org `isa` organisation
            $ forWhich name `labelMatches` orn 
            $ [employer `rp` org, employee `rp` per]
                `isaRel` (isa_ employment $ε)
            $ relName per 
                $ emptyIsaRel (isa_ studentship $ε)
            $ forWhich score `labelMatches` sco $ε)
            `get` [sco]
            `median` [sco]
            

{-
match
    $per isa person;
    $scc isa school-course, has title $title;
    (student: $per, course: $scc) isa studentship;
get $scc, $title; 
group $title;
-}
groupQuery :: Query GROUP
groupQuery = match 
           ( per `isa` person
           $ scc `isa` school_course
           $ forWhich "title" `labelMatches` title
           $ ["student" `rp` per, course `rp` scc]
                `isaRel_` studentship )
           `get` [scc,title]
           `group` [title]


countGroupQuery :: Query COUNT
countGroupQuery = groupQuery !$ count_

[defTxts| ;employment;organisation;employee;student
          ;studentship;employer;score;name;salary;
          ;school-course;course;person|]
[defVars| per sch ran org orn sal sce sco new_org emp p scc title |]
