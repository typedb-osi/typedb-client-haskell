{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module TypeDBQuery where
import Data.Text (Text, pack)
import Prelude hiding (String(..),or)

data Calculation a = R_LT a
                   | R_GT a
                   | R_GE a
                   | R_LE a
                   | R_EQ a
                   | R_NEQ a

newtype Variable = Var { fromVar :: Text }
newtype Label = Lbl { fromLbl :: Text }

data RolePlay a b = RP { role :: IsaMember a => Maybe a, player :: IsaMember b => b }

newtype Relation a b = Rel { fromRel :: (IsaMember a, IsaMember b) => [RolePlay a b]}


class IsaMember a where

instance IsaMember Variable where
instance IsaMember (Relation a b) where

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

-- $p isa person
pIsaPerson :: Pattern ISA
pIsaPerson = (Var "p") `isa` (Just "person") $ EndP

isa :: Variable -> Maybe Text -> (Pattern next -> Pattern ISA)
isa var mt = Isa var mt

isa_ :: Text -> (Pattern next -> Pattern ISA)
isa_ t = Isa t (Nothing :: Maybe Text)

isa' :: Variable -> Maybe Text -> (Pattern next -> Pattern ISA)
isa' var mt = Isa' var mt

isa'_ :: Text -> (Pattern next -> Pattern ISA)
isa'_ t = Isa' t (Nothing :: Maybe Text)

-- $p isa person, has full-name $n
pIsaPersonHasFullNameN :: Pattern ISA
pIsaPersonHasFullNameN
  = (Var "p") `isa` (Just "person") 
        $ has "fullName" (Var "n")
        $ EndP

has :: Text -> Variable -> Pattern next -> Pattern next'
has t var = Has t var

hasValue :: Text -> Calculation a -> Pattern next -> Pattern next'
hasValue t var = Has t var

varHasValue :: Variable -> Calculation a -> Pattern next -> Pattern next'
varHasValue t var = Has t var

-- $emp (employer: $x, employee: $y) isa employment
empIsaEmployment :: Pattern REL
empIsaEmployment = relName (Var "emp")
                     $ Rel [RP (Just "employer") (Var "x")
                           ,RP (Just "employee") (Var "y")]
                        `isaRel` (isa_ "employment" $ EndP)
                     $ EndP

relName :: Variable -> Pattern REL -> Pattern REL
relName v (RelMatch _ r i n) = RelMatch (Just v) r i n

isaRel :: Relation a b -> Pattern ISA -> Pattern next -> Pattern REL
isaRel r i n = RelMatch (Nothing :: Maybe Variable) r i n

-- $emp (employer: $x, employee: $y) isa employment, has reference-id $ref
empIsaEmploymentHasRepId :: Pattern REL
empIsaEmploymentHasRepId = appendToRel empIsaEmployment
                                $ "reference-id"
                                    `has` (Var "ref")
                                $ EndP
                            

-- (employer: $x, employee: $y) isa employment
nothingIsaEmployment :: Pattern REL
nothingIsaEmployment = Rel [RP (Just "employer") (Var "x")
                            ,RP (Just "employee") (Var "y")]
                            `isaRel` (isa_ "employment" EndP)
                     $ EndP

-- $fr ($x, $y) isa friendship
nothingIsaFriendship :: Pattern REL
nothingIsaFriendship = Rel [RP (Nothing) (Var "x")
                           ,RP (Nothing) (Var "y")]
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
  = n `isa` (Just "nickname")
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
  = (Var "p") `isa` (Just "person")
  $ "nickname" `has` (Var "nn") 
  $ "full-name" `has` (Var "fn") 
  $ EndP

-- $s isa school, has ranking < 100
sIsaSchoolHasRanking :: Pattern ISA
sIsaSchoolHasRanking
  = (Var "s") `isa` (Just "school")
  $ "ranking" `hasValue` (R_LT (100::Int)) 
  $ EndP

-- $s isa school, has ranking $r; $r < 100
sIsaSchoolHasRankingSep :: Pattern ISA
sIsaSchoolHasRankingSep 
  = (Var "s") `isa` (Just "school")
  $ "ranking" `has` (Var "r")
  $ (Var "r") `varHasValue` (R_LT (100::Int)) 
  $ EndP

-- $p isa person, has full-name $fn; { $fn contains "Miriam"; } or { $fn contains "Solomon"; }
personFullNameAorB :: Pattern ISA
personFullNameAorB
  = (Var "p") `isa` (Just "person")
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
  = (Var "rr") `isa'` (Just "romantic-relationship")
  $ EndP

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
appendToRel (RelMatch c d e _) b = RelMatch c d e b 

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

