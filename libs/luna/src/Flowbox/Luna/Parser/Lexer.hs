---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Flowbox.Luna.Parser.Lexer where

import Prelude hiding(lex)
import Data.Char hiding (Space)
import           Text.ParserCombinators.UU hiding(parse, pMany)
import qualified Text.ParserCombinators.UU.Utils as Utils
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)
--import qualified Data.ListLike as LL
--import Text.ParserCombinators.UU.Idioms
--import Text.ParserCombinators.UU.Interleaved


import qualified Flowbox.Luna.Parser.Keywords as Keywords


import Flowbox.Luna.Parser.Utils
-------------------


pSpace = pSym ' '
pTab   = pSym '\t'
pWSpace = pSpace <|> pTab
pWSpaces = pMany pWSpace
pWSpaces1 = pMany1 pWSpace

indentOf s = case s of
    ' '  -> 1
    '\t' -> 4

pIndentLvl = (+) <$> (indentOf <$> (pSpace <|> pTab)) <*> pIndentLvl `opt` 0


--pSign  =    (((:) <$> (pSym '+')) 
--       <|>   ((:) <$> (pSym '-')) 
pSign  =    ((:) <$> (pSym '-')
      `opt`  id) 
       <?>  "Sign"

pIntegerStr = pSign <*> pNatural
pDoubleStr  =   (\i d r -> i ++ [d] ++ r) <$> pIntegerStr <*> pSym '.' <*> ( pIntegerStr `opt` "" )
            <|> (\  d r ->      [d] ++ r) <$>                 pSym '.' <*> pIntegerStr

pNatural    = pList1 Utils.pDigit            <?> "Natural"

pVarIdent'     = (:) <$> pLower <*> pMunch isAlpha `micro` 2
pTypeClsIdent' = (:) <$> pUpper <*> pMunch isAlpha `micro` 2
pTypeVarIdent' = pVarIdent'
pTypeIdent'    = pTypeClsIdent' <|> pTypeVarIdent'
pParenL'       = pSym '('
pParenR'       = pSym ')'
pParens'       = pParenL' <* pParenR'
pIdent'        = pVarIdent' <|> pTypeIdent'
pInteger'      = pIntegerStr <?> "Integer"
pChar'         = pPacked (pSym '\'') (pSym '\'') (pNoneOf "'")
pString'       = pPacked (pSym '"') (pSym '"') (pMany $ pNoneOf "\"")
pCommentSingle = pSym '#' *> (pMany $ pNoneOf "\n")
pCommentMulti  = concat <$ pSyms "#[" <*> pMany (liftList (pNoneOf "#") 
                                             <|> ((\x y->[x,y]) <$> pSym '#' <*> pNoneOf "][")
                                             <|> pCommentMulti
                                          ) <* pSyms "#]"
pComment       = pCommentSingle -- pCommentMulti <|>
pVarIdent      = lexeme $ pVarIdent' 
pTypeClsIdent  = lexeme $ pTypeClsIdent'
pTypeVarIdent  = lexeme $ pTypeVarIdent'
pTypeIdent     = lexeme $ pTypeIdent'
pIdent         = lexeme $ pIdent'
pAssign        = lexeme $ pSym '='
pTerminator    = lexeme $ pSym ';'
pBlockBegin    = lexeme $ pSym ':'
pInteger       = lexeme $ pInteger'
pParenL        = lexeme $ pParenL'
pParenR        = lexeme $ pParenR'
pTypeDecl      = pSpaced $ pSyms "::"
pIndent        = pIndentLvl
pParensed      = pPacked pParenL (pSpaces *> pParenR')
pParensed'     = pPacked pParenL' pParenR'
pPath          = lexeme $ pSepBy1 (pSym '.') pIdent


-------------------

--pWSpace    = pChoice [pSpace, pTab]
--pSpace     = Tokens.Space   <$  pSym ' '
--pTab       = Tokens.Tab     <$  pSym '\t'



--indentOf tok = case tok of
--    Tokens.Space -> 1
--    Tokens.Tab   -> 4
--    _            -> error "Can compute only indentations of Space and Tab"



---------------------

--pKeyword (sem, symbol) = sem <$ pKey symbol
--pKey keyw   = pToken keyw `micro` 1
--anyKeyword  = pChoice.map pKeyword
--pKeywords   = anyKeyword [ ( Keywords.Def       , "def"       )
--                                            , ( Keywords.Class     , "class"     )
--                                            , ( Keywords.Interface , "interface" )
--                                            , ( Keywords.Import    , "import"    )
--                                            , ( Keywords.From      , "from"      )
--                                            , ( Keywords.As        , "as"        )
--                                            ] <?> "Keyword"



---------------------



---------------------



--pDouble     = Tokens.Double  <$> pDoubleStr  <?> "Double"

--pString     =   (\x -> [Tokens.StringBegin, Tokens.String x, Tokens.StringEnd]) 
--            <$> pPacked (pSym '"') (pSym '"') (pMany (pNoneOf "\"")) <?> "String Literal"

---------------------

--pOp = Tokens.Operator <$> (pMany1 $ pElem "!$%^&*/+-") 

--pAccessor   = Tokens.Accessor   <$ pSym '.'
--pLambda     = Tokens.Lambda     <$ pSym ':'
--pGrpOpen    = Tokens.GrpOpen    <$ pSym '('
--pGrpClose   = Tokens.GrpClose   <$ pSym ')'
--pSeparator  = Tokens.Separator  <$ pSym ','

--pGrp        = pGrpOpen <|> pGrpClose

---------------------






--liftl a        = [a]

--pWSpaces      = []   <$  pWSpace                     <?> "Whitespace"
--pNewLine      = (\x y-> [x, y]) <$> pEOL <*> pIndent <?> "EOL"
----pIdentKeyword = liftl <$> (pIdent <|> pKeywords)      <?> "Identifier"
--pNumber       = liftl <$> (pDouble <|> pInteger)      <?> "Number"
--pOps          = liftl <$> pOp                         <?> "Operator"
--pAssigns      = liftl <$> pAssign                     <?> "Assignment"
--pAccessors    = liftl <$> pAccessor                   <?> "Accessor"
--pLambdas      = liftl <$> pLambda                     <?> "Lambda"
--pGrps         = liftl <$> pGrp                        <?> "Group"
--pSeparators   = liftl <$> pSeparator                  <?> "Separator"
--pTerminators  = liftl <$> pTerminator                 <?> "Terminator"


---------------------

--pTok = pChoice [ pWSpaces
--               , pNewLine
--               --, pIdentKeyword
--               , pOps
--               , pAssigns
--               , pAccessors
--               , pLambdas
--               , pNumber
--               , pGrps
--               , pSeparators
--               , pTerminators
--               , pString
--               ]

--pLex = (++) <$> pTok <*> pLex `opt` []


--s1 = "def a(x,y,z):\
--   \\n    x+y+z"


--lex s = parse ( (,) <$> pLex <*> pEnd) (createStr (LineColPos 0 0 0) s)



--main :: IO ()
--main = do 
--    run pLex s1
--    return ()

--type Parser a = P (Str Char String LineColPos) a

--run :: Show t =>  Parser t -> String -> IO ()
--run p inp = do  let (a, errors) =  lex inp
--                putStrLn ("--  Result: " ++ show a)
--                if null errors then  return ()
--                               else  do putStr ("--  Correcting steps: \n")
--                                        show_errors errors
--                putStrLn "-- "
--             where show_errors :: (Show a) => [a] -> IO ()
--                   show_errors = sequence_ . (map (putStrLn . show))

--p =     f <$> q <*> r
--    <|> g <$> q <*> s

--p = q <**> (flip f <$> r <|> flip g <$> s)



--pa  ::Parser String 
--pa  = lift <$> pSym 'a'


--pb = (++) <$> pa <*> pa 

--pc =  pMany $ pSym 'a'

--pPlus = (+) <$> pInteger <* pSym '+' <*> pInteger


--applyAll x (f:fs) = applyAll (f x) fs
--applyAll x []     = x



--pPlus' = applyAll <$> pInteger <*> pMany ((+) <$ pSyms "+" <*> pInteger)

--pPlusMinus = applyAll <$> pInteger <*> pMany ( (flip (-) <$ pSyms "-"
--                                                <|>
--                                                flip (+) <$ pSyms "+")
--                                             <*> pInteger)


--pPlusMinus' = ((-) <$ pSym '-' <|> (+) <$ pSym '+') `pChainl` pInteger


--pOp (sem, symbol) = sem <$ pSyms symbol




--pSeq []       = pReturn []
--pSeq (p : pp) = (:) <$> p <*> pSeq pp


--anyOp  = pChoice.map pOp
--addops = anyOp [ ((+),  "+")
--               , ((-),  "-")
--               ]
--mulops = anyOp [ ((*),  "*")
--               ]
--andops = anyOp [ ((&&), "&&")
--               ]
--orops  = anyOp [ ((||), "||")
--               ]

---- simple precedense levels:
--pTimes           = mulops `pChainl` pInteger
--pPlusMinusTimes  = addops `pChainl` pTimes
--pPlusMinusTimes2 = pChainl addops pTimes
--pPlusMinusTimes3 = pChainl addops (pChainl mulops pInteger)
---- multiple precedense levels:
--pPlusMinusTimes4 = foldr pChainl pInteger [addops, mulops]


-----

--pPack o p c = pSyms o *> p <* pSyms c

--pFactor = pInteger <|> pPack "(" pExpr ")"

--pExpr   = foldr pChainl pFactor [addops, mulops ] <|> pIfThenElse

--pIfThenElse = choose <$  pSyms "if"
--                     <*> pBoolExpr
--                     <*  pSyms "then"
--                     <*> pExpr
--                     <*  pSyms "else"
--                     <*> pExpr

--choose c t e = if c then t else e

--pBoolExpr = foldr pChainr pRelExpr [orops, andops ]

--pRelExpr =          True  <$   pSyms "True"
--                <|> False <$   pSyms "False"
--                <|> pExpr <**> pRelOp <*> pExpr


--pRelOp = anyOp [ ((<=), "<=")
--               , ((>=), ">=")
--               , ((==), "==")
--               , ((/=), "/=")
--               ]



--ident = ((:) <$> pRange ('a','z') <*> pMunch (\x -> 'a' <= x && x <= 'z') `micro` 2) <* spaces
--idents = pList1 ident

--pKey keyw = pToken keyw `micro` 1 <* spaces
--spaces :: Parser String
--spaces = pMunch (`elem` " \n")
 
--takes_second_alt =   pList ident 
--              <|> (\ c -> ["IfThenElse"] ++  [c]  ) 
--                  <$ pKey "if"   <*>  ident 
--                  -- <* pKey "then" <*> pList_ng ident
--                  -- <* pKey "else" <*> pList_ng ident

----------------------------

