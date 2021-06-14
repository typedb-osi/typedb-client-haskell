{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module TypeDBTH where
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.List.Split
import Data.Text (pack)
import Data.List (intercalate)
import qualified System.IO as S

mkTextNewtype :: String -> String -> [String] -> [Dec]
mkTextNewtype typeName constructor vars = intercalate [] $ map (\v ->
    [ SigD (mkName $ replaceWhite v) $ ConT (mkName typeName) 
    , ValD (VarP $ mkName $ replaceWhite v)
         (NormalB $ AppE (ConE $ mkName constructor)
                  $ AppE (VarE $ mkName "pack")
                  (LitE $ StringL v))
         []
    ])
    vars

mkVars :: [String] -> [Dec]
mkVars = mkTextNewtype "Variable" "Var"

mkLabels :: [String] -> [Dec]
mkLabels = mkTextNewtype "Label" "Lbl"

mkTexts :: [String] -> [Dec]
mkTexts vars = intercalate [] $ map (\v ->
    [ SigD (mkName $ replaceWhite v) $ ConT (mkName "Text")
    , ValD (VarP $ mkName $ replaceWhite v)
         (NormalB $ AppE (VarE $ mkName "pack")
                         (LitE $ StringL v))
         []
    ])
    vars

replaceWhite :: String -> String
replaceWhite = filter (\x -> x/='\n' && x/='\t')
                           . map (\c -> case c of
                                          ' '-> '_'
                                          '-'-> '_'
                                          _ -> c) 


defVars :: QuasiQuoter
defVars = QuasiQuoter { quoteDec = expQuoteVars }

defTxts :: QuasiQuoter
defTxts = QuasiQuoter { quoteDec = expQuoteTexts } 

defLbls :: QuasiQuoter
defLbls = QuasiQuoter { quoteDec = expQuoteLabels } 

prepString :: String -> (Char, String)
prepString s = (char, no_newlines)
    where
        str = reverse . dropWhile (==' ') . reverse . dropWhile (==' ') $ s
        char = head str
        rest = tail str
        no_newlines = filter (/='\n')
                    $ filter (/='\t')
                    $ filter (/='\r')
                    $ rest

splitString :: String -> [String]
splitString s = filter (not . all (==' '))
              . filter (/= "") 
              . splitOn [char] $ no_newlines
    where 
        (char,no_newlines) = prepString s 



expQuoteTexts :: String -> Q [Dec]
expQuoteTexts = return . mkTexts . splitString

expQuoteLabels :: String -> Q [Dec]
expQuoteLabels = return . mkLabels . splitString

expQuoteVars :: String -> Q [Dec]
expQuoteVars = return . mkVars . splitString


mkCheckAll :: String -> [(String,String,String)] -> Q [Dec]
mkCheckAll name defs = do
    runIO (putStrLn $ "TypeDBTH: splicing " ++ name)
    sig <- [t| IO () |]
    sig2 <- [t| [IO ()] |]
    return [] 
    return $ 
        [ SigD (mkName name ) $ sig
        , ValD (VarP $ mkName name)
          (NormalB 
            $ AppE (VarE $ mkName "sequence_")
                   (ParensE $ SigE
                      (ListE $ map (\(textDefName,exprName,compName) ->
                                AppE (AppE (AppE (VarE $ mkName compName)
                                                 (LitE $ StringL textDefName))
                                           (VarE $ mkName exprName))
                                     (VarE $ mkName 
                                            $ tail 
                                            $ dropWhile (/='_') exprName)
                            ) defs
                      )
                      sig2
                   )
            )
            []
        ]

genCheckAll :: String -> String -> String -> Q [Dec]
genCheckAll name compareFunc prefix = do
    return []
    Loc { loc_filename = filename } <- location
    --when (filename == "<interactive>") $ error "don't run this interactively"
    ls <- runIO (fmap lines (readUTF8File filename))
    
    let specs = map (takeWhile (/='\t') . takeWhile (/=' ')) 
               $ filter (\l -> take 5 l == prefix) ls
        patternsAndSpecNames = map (\s -> (drop 6 s, s, compareFunc)) specs
   
    mkCheckAll name patternsAndSpecNames


---------- from QuickCheck -------------
readUTF8File name = S.openFile name S.ReadMode >>=
                    set_utf8_io_enc >>=
                    S.hGetContents

-- Deal with UTF-8 input and output.
set_utf8_io_enc :: S.Handle -> IO S.Handle
#if __GLASGOW_HASKELL__ > 611
-- possibly if MIN_VERSION_base(4,2,0)
set_utf8_io_enc h = do S.hSetEncoding h S.utf8; return h
#else
set_utf8_io_enc h = return h
#endif
