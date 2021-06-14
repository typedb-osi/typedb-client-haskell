{-# OPTIONS_GHC -Wno-missing-signatures -Wno-name-shadowing -Wno-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Tests_TypeDBQuery where
import Prelude hiding (sum,max,min,or,not)
import TypeDBQuery
import TypeDBTH
import qualified Data.Text as T
import Data.Text (Text,pack)

-- $(genCheckAll "checkAllPatterns" "cmp_pspec" "pspec")
$(genCheckAll "checkAllQueries"  "cmp_qspec" "qspec")

cmp_pspec :: T.Text -> T.Text -> Pattern a -> IO ()
cmp_pspec name t p = case (removeTrailing $ compilePattern p) == t of
                       True -> putStrLn $ T.unpack $ name <> ": OK"
                       False -> do
                            putStrLn $ T.unpack $ name <> ": ERR;"
                            putStrLn $ T.unpack $ "  compiled: " <> (removeTrailing $ compilePattern p)
                            putStrLn $ T.unpack $ "  given:    " <> t
                                
cmp_qspec :: T.Text -> T.Text -> Query a -> IO ()
cmp_qspec name t q = case (removeTrailing $ compileQuery q) == t of
                       True -> putStrLn $ T.unpack $ name <> ": OK"
                       False -> do
                            putStrLn $ T.unpack $ name <> ": ERR;"
                            putStrLn $ T.unpack $ "  compiled: " <> (removeTrailing $ compileQuery q)
                            putStrLn $ T.unpack $ "  given:    " <> t

pspec_pIsaPerson = "$p isa person"
pIsaPerson :: Pattern ISA
pIsaPerson = (Var "p") `isa` "person" $ EndP


pspec_pIsaPersonHasFullNameN = "$p isa person, has full-name $n"
pIsaPersonHasFullNameN :: Pattern ISA
pIsaPersonHasFullNameN
  = (Var "p") `isa` person
        $ has "full-name" (Var "n")
        $ EndP

pspec_empIsaEmployment = "$emp ( employer: $x, employee: $y ) isa employment"
empIsaEmployment :: Pattern REL
empIsaEmployment = relName (Var "emp")
                     $ [rp "employer" (Var "x")
                       ,rp "employee" (Var "y")]
                        `isaRel` (isa_ "employment" $ EndP)
                     $ EndP


pspec_empIsaEmploymentHasRepId = "$emp ( employer: $x, employee: $y ) isa employment, has reference-id $ref"
empIsaEmploymentHasRepId :: Pattern REL
empIsaEmploymentHasRepId = appendToRel empIsaEmployment
                                $ "reference-id"
                                    `has` (Var "ref")
                                $ EndP
                            

pspec_nothingIsaEmployment = "( employer: $x, employee: $y ) isa employment"
nothingIsaEmployment :: Pattern REL
nothingIsaEmployment = [rp "employer" (Var "x")
                       ,rp "employee" (Var "y")]
                            `isaRel` (isa_ "employment" EndP)
                     $ EndP

pspec_nothingIsaFriendship = "$fr ( $x, $y ) isa friendship"
nothingIsaFriendship :: Pattern REL
nothingIsaFriendship = relName (Var "fr")
                         $   Rel (map (sRP . Var) ["x","y"])
                                `isaRel` (isa_ "friendship" EndP)
                    $ EndP

pspec_xLike = "$x \"like\""
xLike :: Pattern ()
xLike = (Var "x") `matches` "like"
      $ EndP


pspec_isaNicknameMitzi = "$n isa nickname; $n \"Mitzi\""
isaNicknameMitzi :: Pattern ISA
isaNicknameMitzi 
  = n `isa` "nickname"
  $ n `matches` "Mitzi"
  $ EndP
      where n = Var "n"



pspec_phoneNumberContains = "$phone-number contains \"+44\""
phoneNumberContains :: Pattern ()
phoneNumberContains
  = (Var "phone-number") `contains` "+44"
  $ EndP




pspec_xLikeRegex = "$x like \"(Miriam Morton|Solomon Tran)\""
xLikeRegex :: Pattern ()
xLikeRegex 
  = (Var "x") `like` "(Miriam Morton|Solomon Tran)" 
  $ EndP

pspec_pIsaPersonNicknameFullname = "$p isa person, has nickname $nn, has full-name $fn"
pIsaPersonNicknameFullname :: Pattern ISA
pIsaPersonNicknameFullname
  = (Var "p") `isa` "person"
  $ "nickname" `has` (Var "nn") 
  $ "full-name" `has` (Var "fn") 
  $ EndP

pspec_sIsaSchoolHasRanking = "$s isa school, has ranking < 100"
sIsaSchoolHasRanking :: Pattern ISA
sIsaSchoolHasRanking
  = (Var "s") `isa` "school"
  $ (Lbl "ranking") `calculatedBy` (R_LT (100::Int)) 
  $ EndP

pspec_sIsaSchoolHasRankingSep = "$s isa school, has ranking $r; $r < 100"
sIsaSchoolHasRankingSep :: Pattern ISA
sIsaSchoolHasRankingSep 
  = (Var "s") `isa` "school"
  $ (Lbl "ranking") `has` (Var "r")
  $ (Var "r") `calculatedBy` (R_LT (100::Int)) 
  $ EndP

pspec_personFullNameAorB = "$p isa person, has full-name $fn; { $fn contains \"Miriam\"; } or { $fn contains \"Solomon\"; }"
personFullNameAorB :: Pattern ISA
personFullNameAorB
  = (Var "p") `isa` "person"
  $ "full-name" `has` (Var "fn")
  $ (Contains (Var "fn") ("Miriam" :: Text) EndP)
        `or` 
    (Contains (Var "fn") ("Solomon" :: Text) EndP)
  $ EndP


pspec_isa'RomanticRelationship = "$rr isa! romantic-relationship"
isa'RomanticRelationship :: Pattern ISA
isa'RomanticRelationship 
  = (Var "rr") `isa'` "romantic-relationship" $ ε

pspec_xIIDSomething = "$x iid 0x966e80018000000000000000"
xIIDSomething :: Pattern ()
xIIDSomething
  = (Var "x") `iid` "0x966e80018000000000000000"
  $ EndP

pspec_xSubPost = "$x sub post"
xSubPost :: Pattern SUB
xSubPost = (Var "x") `sub` "post"
         $ EndP

pspec_xSub'Post = "$x sub! post"
xSub'Post :: Pattern SUB
xSub'Post = (Var "x") `sub'` "post"
          $ EndP

pspec_xTypePost = "$x type post"
xTypePost :: Pattern ()
xTypePost = (Var "x") `typeOf` "post"
          $ EndP

pspec_employmentRelatesX = "employment relates $x"
employmentRelatesX :: Pattern ()
employmentRelatesX = "employment" `relates` (Var "x") 
                   $ EndP

pspec_friendRequestRelatesXAsLocated = "friend-request relates $x as located"
friendRequestRelatesXAsLocated :: Pattern ()
friendRequestRelatesXAsLocated
  = friend_request `relates'` (Var "x") 
        `as` (Lbl "located")
  $ EndP

pspec_xPlaysEmploymentEmployee = "$x plays employment: employee"
xPlaysEmploymentEmployee :: Pattern ()
xPlaysEmploymentEmployee 
  = (Var "x") `plays` (RP (Just employment) employee)
  $ EndP

pspec_xHasTitleT = "$x has title $t"
xHasTitleT :: Pattern ()
xHasTitleT = (Var "x") `hasTitle` (Var "t")
            $ EndP

qspec_bigMatch = "match $fr ( $x, $y ) isa friendship; $x isa person, has full-name $x-fn; $x-fn contains \"Miriam\"; $y isa person, has full-name $y-fn, has phone-number $y-pn; get $x-fn, $y-fn, $y-pn;"
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



qspec_smallMatch = "match $p isa person; get $p; limit 1;"
smallMatch :: Query GET
smallMatch = match 
           ( p `isa` "person" $ ε)
                `get'` [p] $ [Limit 1]
    where 
        p = Var "p"

qspec_matchWithSort = "match $p isa person, has full-name $fn; get $fn; sort $fn asc;"
matchWithSort :: Query GET
matchWithSort = match
              ( p `isa` "person" 
              $ has "full-name" fn $ ε)
                `get'` [fn] $ [Sort fn ASC]
        where
            [p,fn]= map Var ["p","fn"]
                
qspec_deletePersonByMail = "match $p isa person, has email \"someMail\"; delete $p isa person;"
deletePersonByMail :: Query DELETE
deletePersonByMail 
    = match
    ( p `isa` "person"
    $ forWhich "email" `labelHasValue` "someMail" $ ε)
        `delete` (p `isa` "person" $ ε)
    where
        p = Var "p"

qspec_removeEmployee = "match $org isa organisation, has name \"Pharos\"; $emp ( employer: $org, employee: $p ) isa employment; delete $emp isa employment;"
removeEmployee :: Query DELETE
removeEmployee 
    = match 
    ( org `isa` "organisation" 
    $ forWhich "name" `labelHasValue` "Pharos"
    $ relName emp 
        $ [rp "employer" org, rp "employee" p]
            `isaRel_` "employment")
        `delete` (emp `isa` "employment" $ ε)
    where
        [emp,org,p] = map Var ["emp","org","p"]

qspec_deleteAttribute = "match $t isa travel, has start-date $st; $d 2013-12-22; delete $t has $st;"
deleteAttribute :: Query DELETE
deleteAttribute
    = match 
    ( t `isa` "travel"
    $ has "start-date" st
    $ asWellAs d `matchesDate` (Date 2013 12 22) $ ε)
        `delete` (t `generallyHas` st $ ε)
    where
        [t,st,d] = map Var ["t","st","d"]

qspec_deleteRolePlayers = "match $org isa organisation, has name \"Pharos\"; $emp ( employer: $org, employee: $p ) isa employment; delete $emp ( employer: $org );"
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

qspec_insertPerson = "insert $p isa person, has full-name \"John Parkson\", has gender \"male\", has email \"someEmail\", has phone-number \"+44-1234=567890\";"
insertPerson :: Query INSERT
insertPerson 
    = insert_
    ( p `isa` "person"
    $ forWhich "full-name" `labelHasValue` "John Parkson"
    $ asWellAs "gender" `labelHasValue` "male"
    $ asWellAs "email" `labelHasValue` "someEmail"
    $ asWellAs "phone-number" `labelHasValue` "+44-1234=567890" $ ε)
        where
            p = Var "p"

qspec_insertEmotion = "insert $x isa emotion; $x \"like\";"
insertEmotion :: Query INSERT
insertEmotion 
    = insert_
    ( x `isa` "emotion"
    $ x `matches` "like" $ ε)
        where 
            x = Var "x"

qspec_insertInstanceOfRelType = "match $org isa organisation, has name \"Facelook\"; $person isa person, has email \"someEmail\"; insert $new-employment ( employer: $org, employee: $person ) isa employment; $new-employment has reference-id \"WGFTSH\";"
insertInstanceOfRelType :: Query INSERT
insertInstanceOfRelType
    = (match
    $ org `isa` "organisation"
    $ forWhich "name" `labelHasValue` "Facelook"
    $ person `isa` "person"
    $ forWhich "email" `labelHasValue` "someEmail" $ ε)
        `insert`
    (relName new_employment
        $ [rp "employer" org, rp "employee" person]
            `isaRel'` "employment"
    $ new_employment `varField` "reference-id" 
                  `hasValue'` "WGFTSH" $ ε)
    where
        [org,person,new_employment] = map Var ["org","person","new-employment"]


qspec_duplicateRolePlayers = "match $person isa person; insert $reflexive-friendship ( friend: $person, friend: $person ) isa friendship;"
duplicateRolePlayers :: Query INSERT
duplicateRolePlayers
    = match 
    ( person `isa` "person" $ ε)
      `insert`
    ( relName reflexive_friendship
        $ [rp "friend" person, rp "friend" person]
        `isaRel_` "friendship")
    where 
        [reflexive_friendship, person] 
          = map Var ["reflexive-friendship", "person"]

qspec_matchDuplicates = "match $friendship ( friend: $p, friend: $p ) isa friendship; get $friendship;"
matchDuplicates :: Query GET
matchDuplicates 
    = match
    ( relName friendship 
        $ [rp "friend" p, rp "friend" p]
        `isaRel_` "friendship")
    `get` [friendship]
        where
            [friendship,p] = map Var ["friendship", "p"]


qspec_extendingRelation = "match $emp ( employer: $org, employee: $p ) isa employment; $p2 isa person; not { $p = $p2; }; insert $emp ( employee: $p2 ) isa employment;"
extendingRelation :: Query INSERT
extendingRelation 
    = match
    ( relName emp
        $ [rp "employer" org, rp "employee" p]
        `isaRel'` "employment"
    $ p2 `isa` "person"
    $ not p (R_EQ p2) $ ε)
      `insert`
    ( relName emp
        $ [rp "employee" p2]
        `isaRel_` "employment")
    where
        [emp,p,p2,org] = map Var ["emp","p","p2","org"]



qspec_updateAttributeOwnedByConcept = "match $org isa organisation, has name \"Medicely\", has registration-number $rn; delete $org has $rn; insert $org has registration-number \"81726354\";"
updateAttributeOwnedByConcept :: Query UPDATE
updateAttributeOwnedByConcept
    = ((match
        ( org `isa` "organisation"
        $ forWhich "name" `labelHasValue` "Medicely"
        $ asWellAs "registration-number" `labelMatches` rn $ ε))
      `delete` ( org `generallyHas` rn $ ε))
      `update`
      (insert_ ( org `varField` "registration-number"
                        `hasValue'` "81726354" $ ε))
    where 
        [org,rn] = map Var ["org","rn"]

qspec_updateInstancesOfAttribute = "match $m isa media, has caption $c; $c contains \"inappropriate word\"; delete $c isa caption; insert $m has caption \"deleted\";"
updateInstancesOfAttribute :: Query UPDATE
updateInstancesOfAttribute
    = update
        (( match
          $ m `isa` "media"
          $ forWhich "caption" `labelMatches` c 
          $ c `contains` "inappropriate word" $ ε)
          `delete` (c `isa` "caption" $ ε))
        ( insert_ 
        $   m `varField` "caption"
                    `hasValue'` "deleted" $ ε)
    where 
        [m,c] = map Var ["m","c"]

qspec_modifyingRoleplayers = "match $org isa organisation, has name \"Pharos\"; $new-org isa organisation, has name \"Medicely\"; $emp ( employer: $org, employee: $p ) isa employment; delete $emp ( employer: $org ); insert $emp ( employer: $new-org );"
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



qspec_countQuery = "match $sce isa studentship, has score $sco; $sco > 7.0; get $sco; count;"
countQuery :: Query COUNT
countQuery = match
           ( sce `isa` "studentship"
           $ forWhich "score" `labelMatches` sco
           $ sco `calculatedBy` (R_GT (7.0 ::Double)) $ ε)
            `get` [sco]
           !$ count_

qspec_sumQuery = "match $org isa organisation, has name $orn; $orn \"Medicely\"; ($org) isa employment, has salary $sal; get $sal; sum $sal;"
sumQuery :: Query SUM
sumQuery = match
         ( org `isa` "organisation"
         $ forWhich "name" `labelMatches` orn
         $ orn `matches` "Medicely"
         $ relName org
            $ emptyIsaRel (isa_ "employment" $ ε)
         $ forWhich "salary" `labelMatches` sal $ ε)
         `get` [sal]
         `sum` [sal]

qspec_maxQuery = "match $sch isa school, has ranking $ran; get $ran; max $ran;"
maxQuery :: Query MAX
maxQuery = match
         ( sch `isa` "school"
         $ forWhich "ranking" `labelMatches` ran $ ε)
         `get` [ran]
         `max` [ran]


qspec_minQuery = "match ($per) isa marriage; ($per) isa employment, has salary $sal; get $sal; min $sal;"
minQuery :: Query MIN
minQuery = match 
         ( per `partOfRel` "marriage"
         $ per `partOfRel` "employment"
         $ forWhich "salary" `labelMatches` sal $ ε)
         `get` [sal]
         `min` [sal]

qspec_meanQuery = "match $emp isa employment, has salary $sal; get $sal; mean $sal;"
meanQuery :: Query MEAN
meanQuery = match
          ( emp `isa` employment
          $ forWhich salary `labelMatches` sal $ ε)
          `get` [sal]
          `mean` [sal]
          

qspec_medianQuery = "match $org isa organisation, has name $orn; $orn = \"Facelook\"; ( employer: $org, employee: $per ) isa employment; ($per) isa studentship, has score $sco; get $sco; median $sco;"
medianQuery :: Query MEDIAN
medianQuery = match
            ( org `isa` organisation
            $ forWhich name `labelMatches` orn 
            $ orn `calculatedBy` (R_EQ ("Facelook"::Text))
            $ [employer `rp` org, employee `rp` per]
                `isaRel` (isa_ employment $ ε)
            $ relName per 
                $ emptyIsaRel (isa_ studentship $ ε)
            $ forWhich score `labelMatches` sco $ ε)
            `get` [sco]
            `median` [sco]
            

qspec_groupQuery = "match $per isa person; $scc isa school-course, has title $title; ( student: $per, course: $scc ) isa studentship; get $scc, $title; group $title;"
groupQuery :: Query GROUP
groupQuery = match 
           ( per `isa` person
           $ scc `isa` school_course
           $ forWhich "title" `labelMatches` title
           $ ["student" `rp` per, course `rp` scc]
                `isaRel_` studentship )
           `get` [scc,title]
           `group` [title]

qspec_countGroupQuery = "match $per isa person; $scc isa school-course, has title $title; ( student: $per, course: $scc ) isa studentship; get $scc, $title; group $title; count;"
countGroupQuery :: Query COUNT
countGroupQuery = (match 
           ( per `isa` person
           $ scc `isa` school_course
           $ forWhich "title" `labelMatches` title
           $ ["student" `rp` per, course `rp` scc]
                `isaRel_` studentship )
           `get` [scc,title]
           `group` [title])
            !$ count_

qspec_definePerson = "define person sub entity;"
definePerson :: Query DEFINE
definePerson = define
             $ person `sub` entity $ ε
        where person = Lbl "person"


qspec_assignAttributes = "define person sub entity, owns full-name, owns nickname, owns gender;"
assignAttributes :: Query DEFINE
assignAttributes = define
                 $ person `sub` entity 
                 $ owns "full-name"
                 $ owns "nickname"
                 $ owns "gender" $ ε
                    
        where person = Lbl "person"

qspec_assignAttributeAsUID = "define person sub entity, owns email @key;"
assignAttributeAsUID :: Query DEFINE
assignAttributeAsUID = define
                     $ person `sub` entity
                     $ ownsKey "email" $ ε
        where person = Lbl "person"


qspec_entityToPlayARole = "define person sub entity, plays employment: employee; organisation sub entity, plays employment: employer;"
entityToPlayARole :: Query DEFINE
entityToPlayARole = define
                  $ person `sub` entity
                      $ plays_ (employment `rp` employee)
                  $ organisation `sub` entity
                      $ plays_ (employment `rp` employer)
                  $ ε
        where
            [person,organisation,employer,employee] = map Lbl ["person","organisation","employer","employee"]

qspec_subtypeEntity = "define post sub entity, plays reply: to, plays tagging: in_, plays reaction: to; comment sub post, owns content, plays attachment: to; media sub post, owns caption, owns file, plays attachment: attached; video sub media; photo sub media;"
subtypeEntity :: Query DEFINE
subtypeEntity = define
              $ post `sub` entity 
                $ plays_ (reply `rp` to)
                $ plays_ (tagging `rp` in_)
                $ plays_ (reaction `rp` to)
              $ comment `sub` post
                $ owns content
                $ plays_ (attachment `rp` to)
              $ media `sub` post
                $ owns caption
                $ owns file
                $ plays_ (attachment `rp` attached)
              $ video `sub` media
              $ photo `sub` media
              $ ε
        where

qspec_abstractEntity = "define post sub entity, abstract; media sub post, abstract;"
abstractEntity :: Query DEFINE
abstractEntity = define
               $ post `sub` entity $ abstract
               $ media `sub` post  $ abstract
               $ ε

qspec_newRelation = "define employment sub relation;"
newRelation :: Query DEFINE
newRelation = define
            $ employment `sub` relation
            $ ε

qspec_relateRelatees = "define employment sub relation, relates employee, relates employer;"
relateRelatees :: Query DEFINE
relateRelatees = define
               $ employment `sub` relation
               $ relates_ employee
               $ relates_ employer
               $ ε


qspec_relationalRoleplay = "define friendship sub relation, relates friend, plays friend-request: friendship; friend-request sub relation, relates friendship, relates requester, relates respondent; person sub entity, plays friendship: friend, plays friend-request: requester, plays friend-request: respondent;"
relationalRoleplay :: Query DEFINE
relationalRoleplay = define
                   $ friendship `sub` relation
                        $ relates_ friend
                        $ plays_ (friend_request `rp` friendship)
                   $ friend_request `sub` relation
                        $ relates_ friendship
                        $ relates_ requester
                        $ relates_ respondent
                   $ person `sub` entity
                        $ plays_ (friendship `rp` friend)
                        $ plays_ (friend_request `rp` requester)
                        $ plays_ (friend_request `rp` respondent)
                   $ ε

qspec_manyRolePlayers = "define reaction sub relation, relates emotion, relates to, relates by; emotion sub attribute, value string, plays reaction: emotion; post sub entity, plays reaction: to; person sub entity, plays reaction: by;"
manyRolePlayers :: Query DEFINE
manyRolePlayers = define
                $ reaction `sub` relation
                    $ relates_ emotion
                    $ relates_ to
                    $ relates_ by
                $ emotion `sub` attribute
                    $ value string
                    $ plays_ (reaction `rp` emotion)
                $ post `sub` entity
                    $ plays_ (reaction `rp` to)
                $ person `sub` entity
                    $ plays_ (reaction `rp` by)
                $ ε
                    
qspec_assignAttributeToRelation = "define friend-request sub relation, owns approved-date, relates friendship, relates requester, relates respondent;"
assignAttributeToRelation :: Query DEFINE
assignAttributeToRelation = define
                          $ friend_request `sub` relation
                            $ owns approved_date
                            $ relates_ friendship
                            $ relates_ requester
                            $ relates_ respondent
                          $ ε
qspec_assignRelAttributeAsKey = "define employment sub relation, owns reference-id @key, relates employer, relates employee;"
assignRelAttributeAsKey :: Query DEFINE
assignRelAttributeAsKey = define
                        $ employment `sub` relation
                        $ ownsKey reference_id
                        $ relates_ employer
                        $ relates_ employee
                        $ ε

qspec_subtypeRelation = "define request sub relation, abstract, relates subject, relates requester, relates respondent; friend-request sub request, owns approved-date, relates friendship as subject, relates friend-requester as requester, relates friend-respondent as respondent; membership-request sub request, owns approved-date, relates approved as subject, relates membership-requester as requester, relates membership-respondent as respondent;"
subtypeRelation :: Query DEFINE
subtypeRelation = define
                $ request `sub` relation
                    $ abstract
                    $ relates_ subject
                    $ relates_ requester
                    $ relates_ respondent
                $ friend_request `sub` request
                    $ owns approved_date
                    $ friendship `relatedAs` subject
                    $ friend_requester `relatedAs` requester
                    $ friend_respondent `relatedAs` respondent
                $ membership_request `sub` request
                    $ owns approved_date
                    $ approved `relatedAs` subject
                    $ membership_requester `relatedAs` requester
                    $ membership_respondent `relatedAs` respondent
                $ ε
        where friendship = Lbl "friendship"

qspec_abstractRelation = "define request sub relation, abstract, relates subject, relates requester, relates respondent;"
abstractRelation :: Query DEFINE
abstractRelation = define
                 $ request `sub` relation
                    $ abstract
                    $ relates_ subject
                    $ relates_ requester
                    $ relates_ respondent
                $ ε


qspec_defineAttribute = "define name sub attribute, value string;"
defineAttribute :: Query DEFINE
defineAttribute = define
                $ name `sub` attribute
                $ value string
                $ ε

qspec_attributeOwning = "define start-date sub attribute, value datetime; residency sub relation, owns start-date; travel sub relation, owns start-date;"
attributeOwning :: Query DEFINE
attributeOwning = define
                $ start_date `sub` attribute
                    $ value datetime
                $ residency `sub` relation
                    -- roles and other attributes
                    $ owns start_date
                $ travel `sub` relation
                    -- roles and other attributes
                    $ owns start_date
                $ ε

qspec_attributeOwning' = "define phone-number sub attribute, value string; person sub entity, owns phone-number;"
attributeOwning' :: Query DEFINE
attributeOwning' = define
                 $ phone_number `sub` attribute
                    $ value string
                 $ person `sub` entity
                    $ owns phone_number
                 $ ε

qspec_restrictAttrValRegex = "define emotion sub attribute, value string, regex \"^(like|love|funny|shocking|sad|angry)$\";"
restrictAttrValRegex :: Query DEFINE
restrictAttrValRegex = define
                     $ emotion `sub` attribute
                        $ value string
                        $ regex "^(like|love|funny|shocking|sad|angry)$"
                    $ ε

qspec_assignAttributeToOtherValue = "define content sub attribute, value string, owns language; language sub attribute, value string;"
assignAttributeToOtherValue :: Query DEFINE
assignAttributeToOtherValue = define
                            $ content `sub` attribute $ value string
                                $ owns language
                            $ language `sub` attribute
                                $ value string
                            $ ε

qspec_attributePlaysRole = "define language sub attribute, value string, plays fluency: language; person sub entity, plays fluency: speaker; fluency sub relation, relates speaker, relates language;"
attributePlaysRole :: Query DEFINE
attributePlaysRole = define
                   $ language `sub` attribute $ value string
                        $ plays_ (fluency `rp` language)
                   $ person `sub` entity
                        $ plays_ (fluency `rp` speaker)
                   $ fluency `sub` relation
                        $ relates_ speaker
                        $ relates_ language
                   $ ε

qspec_subtypeAttribute = "define event-date sub attribute, abstract, value datetime; birth-date sub event-date; start-date sub event-date; end-date sub event-date;"
subtypeAttribute :: Query DEFINE
subtypeAttribute = define
                 $ event_date `sub` attribute $ abstract $ value datetime 
                 $ birth_date `sub` event_date
                 $ start_date `sub` event_date
                 $ end_date `sub` event_date $ ε

qspec_abstractAttribute = "define event-date sub attribute, abstract, value datetime;"
abstractAttribute :: Query DEFINE
abstractAttribute = define
                  $ event_date `sub` attribute
                  $ abstract
                  $ value datetime $ ε

qspec_undefineAssociation = "undefine person owns nickname;"
undefineAssociation :: Query UNDEFINE
undefineAssociation = undefine
                    $ person `owns'` nickname 
                    $ ε

qspec_undefineRelation = "undefine fluency sub relation;"
undefineRelation :: Query UNDEFINE
undefineRelation = undefine $ fluency `sub` relation $ ε

[defLbls| ;content;caption;file;approved-date;reference-id;
          ;friend-requester;friend-respondent;
          ;membership-requester;membership-respondent;
          ;approved;employment;
          ;organisation;studentship;person;school-course;
          ;employer;employee;course;reply;tagging;
          ;reaction;attachment;phone-number;
          ;friendship;friend-request;
          ;friend;requester;respondent;emotion;
          ;to;by;subject;in_;
          ;attached;request;media;
          ;membership-request;name;
          ;score;salary;residency;travel;
          ;comment;post;language;
          ;video;photo;
          ;speaker;fluency;nickname;
          ;event-date;birth-date;start-date;end-date; |]

[defVars| ;per;sch;ran;org;orn;sal;sce;sco;new-org;emp;p;scc;title; |]


