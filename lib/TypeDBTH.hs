{-# LANGUAGE TemplateHaskell #-}
module TypeDBTH where
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.List.Split
import Data.Text (pack)
import Data.List (intercalate)

mkVars :: [String] -> [Dec]
mkVars vars = intercalate [] $ map (\v ->
    [ SigD (mkName v) $ ConT (mkName "Variable")
    , ValD (VarP $ mkName v)
         (NormalB $ AppE (ConE $ mkName "Var")
                  $ AppE (VarE $ mkName "pack")
                  (LitE $ StringL v))
         []
    ])
    vars


mkTexts :: [String] -> [Dec]
mkTexts vars = intercalate [] $ map (\v ->
    [ SigD (mkName $ replaceWhite v) $ ConT (mkName "Text")
    , ValD (VarP $ mkName $ replaceWhite v)
         (NormalB $ AppE (VarE $ mkName "pack")
                         (LitE $ StringL v))
         []
    ])
    vars
        where replaceWhite = filter (\x -> x/='\n' && x/='\t')
                           . map (\c -> case c of
                                          ' '-> '_'
                                          '-'-> '_'
                                          _ -> c) 

defVars :: QuasiQuoter
defVars = QuasiQuoter { quoteDec = expQuoteVars }

defTxts :: QuasiQuoter
defTxts = QuasiQuoter { quoteDec = expQuoteTexts } 


expQuoteTexts :: String -> Q [Dec]
expQuoteTexts s = 
    let str = reverse . dropWhile (==' ') . reverse . dropWhile (==' ') $ s
        char = head str
        rest = tail str
     in
    return $ (mkTexts . filter (/= "") . splitOn [char] $ rest)

expQuoteVars :: String -> Q [Dec]
expQuoteVars s = return $ (mkVars . filter (/= "") . splitOn " " $ s)
