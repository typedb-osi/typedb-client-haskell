{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module TypeDBQuery where
import Data.Text (Text, pack)
import Prelude hiding (String(..))
{- PATTERNS 
$p isa person
$p isa person, has full-name $n
$emp (employer: $x, employee: $y) isa employment
$emp (employer: $x, employee: $y) isa employment, has reference-id $ref
(employer: $x, employee: $y) isa employment
$fr ($x, $y) isa friendship
$x "like"
$n isa nickname; $n "Mitzi"
$phone-number contains "+44"
$x like "(Miriam Morton|Solomon Tran)"
$p isa person, has nickname $nn, has full-name $fn
$s isa school, has ranking < 100
$s isa school, has ranking $r; $r < 100
$p isa person, has full-name $fn; { $fn contains "Miriam"; } or { $fn contains "Solomon"; }
$rr isa! romantic-relationship
$x iid 0x966e80018000000000000000
$x sub post
$x sub! post
$x type post
employment relates $x
friend-request relates $x as located
$x plays employment:employee
$x has title $t
    -}

data RelOp = R_LT 
           | R_GT
           | R_GE
           | R_LE
           | R_EQ
           | R_NEQ

data Calculation a = Calc { relOp :: RelOp, val :: QShow a => a } 
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

data Pattern next where
    EndP :: Pattern ()
    Isa :: (VarOrText v,VarOrText v') => v -> Maybe v' -> Pattern next -> Pattern ISA
    Isa' :: (VarOrText v, VarOrText v') => v -> Maybe v' -> Pattern next -> Pattern ISA
    Has :: (VarOrLit v, HasMember m') => v -> m' -> Pattern next -> Pattern next
    HasTitle :: (VarOrLit v, HasMember m') => v -> m' -> Text -> Pattern next -> Pattern next
    AttributeMatch :: QShow a => Variable -> a -> Pattern next -> Pattern next
    RelMatch :: Maybe Variable -> Relation a b -> Pattern ISA -> Pattern next -> Pattern ISA
    Contains :: QShow a => Variable ->  a -> Pattern next -> Pattern next
    Like :: Variable -> Text -> Pattern next -> Pattern next
    Iid :: Variable -> Text -> Pattern next -> Pattern next
    Sub :: (VarOrText v, VarOrText v') => v -> v' -> Pattern next -> Pattern ISA
    Sub' :: (VarOrText v, VarOrText v') => v -> v' -> Pattern next -> Pattern ISA
    Type_ :: (VarOrText v, VarOrText v') => v -> v' -> Pattern next -> Pattern ISA
    Relates :: (VarOrText v, VarOrText v', VarOrText v'') => v -> v' -> Maybe v'' -> Pattern next -> Pattern next
    Plays :: (VarOrText v, VarOrText v') => Variable -> v -> v' -> Pattern next -> Pattern next

-- $p isa person
pIsaPerson :: Pattern ISA
pIsaPerson = Isa (Var "p") (Just ("person" :: Text)) EndP

-- $p isa person, has full-name $n
pIsaPersonHasFullNameN :: Pattern ISA
pIsaPersonHasFullNameN
  = Isa (Var "p") (Just ("person" :: Text)) 
                $ Has ("fullName" :: Text) (Var "n") 
                $ EndP

-- $emp (employer: $x, employee: $y) isa employment
empIsaEmployment :: Pattern ISA
empIsaEmployment = RelMatch (Just (Var "emp"))
                            (Rel [RP (Just "employer") (Var "x")
                                 ,RP (Just "employee") (Var "y")])
                     (Isa ("employment" :: Text) (Nothing :: Maybe Text) EndP)
                    $ EndP
--
-- $emp (employer: $x, employee: $y) isa employment, has reference-id $ref
empIsaEmploymentHasRepId :: Pattern ISA
empIsaEmploymentHasRepId = (\case
                            (RelMatch a b c _) -> 
                                RelMatch a b c 
                                    $ Has ("reference-id" :: Text) 
                                          (Var "ref")
                                    $ EndP ) empIsaEmployment
                            

-- (employer: $x, employee: $y) isa employment
nothingIsaEmployment :: Pattern ISA
nothingIsaEmployment =  RelMatch (Nothing :: Maybe Variable)
                            (Rel [RP (Just "employer") (Var "x")
                                 ,RP (Just "employee") (Var "y")])
                     (Isa ("employment" :: Text) (Nothing :: Maybe Text) EndP)
                    $ EndP

-- $fr ($x, $y) isa friendship
nothingIsaFriendship :: Pattern ISA
nothingIsaFriendship =  RelMatch (Nothing :: Maybe Variable)
                            (Rel [RP (Nothing) (Var "x")
                                 ,RP (Nothing) (Var "y")])
                     (Isa ("friendship" :: Text) (Nothing :: Maybe Text) EndP)
                    $ EndP

-- $x "like"
xLike :: Pattern ()
xLike = AttributeMatch (Var "x") ("like" :: Text)
            $ EndP

-- $n isa nickname; $n "Mitzi"
isaNicknameMitzi :: Pattern ISA
isaNicknameMitzi 
  = Isa (Var "n") (Just ("nicknamen" :: Text)) 
        $ AttributeMatch (Var "n") ("Mitzi" :: Text)
        $ EndP


-- $phone-number contains "+44"
-- $x like "(Miriam Morton|Solomon Tran)"
-- $p isa person, has nickname $nn, has full-name $fn
-- $s isa school, has ranking < 100
-- $s isa school, has ranking $r; $r < 100
-- $p isa person, has full-name $fn; { $fn contains "Miriam"; } or { $fn contains "Solomon"; }
-- $rr isa! romantic-relationship
-- $x iid 0x966e80018000000000000000
-- $x sub post
-- $x sub! post
-- $x type post
-- employment relates $x
-- friend-request relates $x as located
-- $x plays employment:employee
-- $x has title $t




                        {- 
append :: Pattern a -> Pattern b -> forall c. Pattern c
append (EndP) b = b
append (Isa v v' _) b = Isa v v' b
append (Isa' v v' _) b = Isa v v' b
append (Has v m' _) b = Has v m' b
append (HasTitle v m' t _) b = HasTitle v m' t b
append (AttributeMatch v a _) b = AttributeMatch v a b
append (RelMatch v r p _) b = RelMatch v r p b
append (Contains v a _) b = Contains v a b
append (Like v t _) b = Like v t b
append (Iid v t _) b = Iid v t b
append (Sub v v' _) b = Sub v v' b
-}
    {-
append Sub' :: (VarOrText v, VarOrText v') => v -> v' -> next -> Pattern ISA
append Type_ :: (VarOrText v, VarOrText v') => v -> v' -> next -> Pattern ISA
append Relates :: (VarOrText v, VarOrText v', VarOrText v'') => v -> v' -> Maybe v'' -> next -> Pattern next
append Plays :: (VarOrText v, VarOrText v') => Variable -> v -> v' -> next -> Pattern next
append (
-}
