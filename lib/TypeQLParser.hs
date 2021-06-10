{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module TypeQLParser where
    {-
[g4|
  atom
     : STRING -> Str
     | SYMBOL -> Symb
     | NUMBER -> Number
     ;
  STRING : ( ('\\' .) | ~ [\\] )* -> String;
  WHITESPACE : [ \n\t\r]+ -> String;
  NUMBER : ('+' | '-')? DIGIT+ ('.' DIGIT+)? -> Double;
  SYMBOL : SYMBOL_START (SYMBOL_START | DIGIT)* -> String;
  fragment SYMBOL_START : [a-zA-Z+\-*/] ;
  fragment DIGIT : [0-9] ;
|]
-}
