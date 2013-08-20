---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Flowbox.Luna.Parser.Parser where

import Debug.Trace
import System.TimeIt

import Prelude hiding(lex)
import Data.Char hiding (Space)
import qualified Text.ParserCombinators.UU as UU
import           Text.ParserCombinators.UU hiding(parse, pMany, (<??>))
import qualified Text.ParserCombinators.UU.Utils as Utils
import           Text.ParserCombinators.UU.BasicInstances hiding (Parser)
--import qualified Data.ListLike as LL
--import Text.ParserCombinators.UU.Idioms
--import Text.ParserCombinators.UU.Interleaved


import qualified Flowbox.Luna.Parser.Keywords as Keywords
import qualified Flowbox.Luna.Parser.Lexer as L
import Flowbox.Luna.AST.AST
import qualified Flowbox.Luna.AST.Constant as Constant
import Flowbox.Luna.Parser.Utils
import qualified Data.ListLike as LL


import Control.Exception

import qualified Text.Show.Pretty as PP

import Flowbox.Luna.AST.Constant -- only for tests

import Data.Monoid

---------- Entities ----------

pIdent'      = Identifier       <$> L.pVarIdent'
pTypeIdent   = TypeIdentifier   <$> L.pTypeIdent
pInteger'    = Constant.Integer <$> L.pInteger'
pChar'       = Constant.Char    <$> L.pChar'
pString'     = Constant.String  <$> L.pString'
pConstant    = Constant         <$> pChoice [ pInteger'
                                            , pChar'
                                            , pString'
                                            ]
pTupleSep    = pSpaced $ pSym ','
pTupleBody p = pSepBy pTupleSep p <*? pTupleSep
pTuplePure p = L.pParensed $ pTupleBody p
pTuple i     = Tuple            <$> (L.pParens' *> pure []                                       --empty
                                <|>  L.pParensed (liftList (pOpExpr i) <* pTupleSep)           --one-element
                                <|>  L.pParensed (pSepBy2 pTupleSep (pOpExpr i) <*? pTupleSep) --multi-element
                                    )
pEnt         = pChoice [ pIdent'
                       , pConstant
                       ]

pTyped p   = flip Typed       <$> p <* L.pTypeDecl <*> L.pTypeIdent


---------- Expressions ----------

pNOP          = NOP        <$  L.pTerminator

pImportPath _ = Path <$> L.pPath <??> (Named <$ Keywords.pAs <*> L.pIdent)

pImport i     =     (ImportQualified <$ Keywords.pFrom <*> pImportPath i) 
              <<?*> (Import <$  Keywords.pImport  <*> ((:) <$> pImportPath i) <<?*> pBlock pImportPath (i+1))

--pAssignment   = Assignment <$> pEnt <* L.pAssign <*> (pExpr 0 <|> pEnt)

pFunc i       = Function   <$  Keywords.pDef 
                           <*> L.pVarIdent 
                           <*> (pTuplePure (pOpExpr2 i) <<|> pTupleBody (pOpExpr2 i))
                           <*> pExprBlock i

pLambda i     = Lambda     <$> (pTuplePure (pOpExpr2 i) <<|> liftList pIdent')
                           <*> pExprBlock i <* pSpaces --pSpaces is used because of micro usage
                           `micro` 2

pClass i      = Class      <$ Keywords.pClass
                           <*> L.pTypeClsIdent
                           <*> pMany L.pTypeVarIdent
                           <*> pExprBlock i

pInterface i  = Interface  <$ Keywords.pInterface
                           <*> L.pTypeIdent
                           <*> pExprBlock i

pComment      = Comment    <$> L.pComment

--pCall i       = Call       <$> pTrace L.pIdent' <*> ( (items <$> pTuple i) <<|> pSpaces *> (pMany $ pTupleItem i) )   -- <* pSpaces <*> pMany (pValue i)

--pExprBlock i  = L.pBlockBegin *> pBlock pExpr (i+1)

pCall i       = Call <$> pIdent' <*> pTuplePure (pOpExpr i)

pExprBlock i  = L.pBlockBegin *> ((:[]) <$> pExpr i <<|> pBlock pExpr (i+1))

pOpExpr i     = aftermatch <$> foldr pChainL (pFactor i) pPrecOps

pOpExpr2 i     = aftermatch <$> foldr pChainL (pFactor2 i) pPrecOps

pFactor i = pChoice [ pFactor2 i
                    , pLambda i
                    ] -- `micro` 2

pFactor2 i = pChoice [ pEnt
                    , pCall i
                    , L.pParensed (pOpExpr i)
                    , pTuple i
                    , pTyped pEnt
                    
                    --, pLambda i
                    ] 

pExpr i       =   pChoice [ pImport i
                          , pFunc i
                          , pClass i
                          , pOpExpr i
                          ]
              <|> pNOP



pOp       sym = Operator   <$> pSpaced (pSyms sym)
pAssignOp sym = Assignment <$  pSpaced (pSyms sym)
pAccOp    sym = Accessor   <$  pSpaced (pSyms sym)
pFuncOp       = callConstructor <$ L.pWSpaces1

pOps   = [ [ pAssignOp "="
           ]
         , [ pFuncOp
           ]
         , [ pOp "+"
           ]
         , [ pOp "*"
           , pOp "/" 
           ]
         , [ pOp "^"
           ]
         , [ pAccOp "."
           ]
         ]

pPrecOps = map pChoice pOps

---------- Nested Segments ----------

pIndentAtLast i = length <$> pAtLeast i (pSym ' ')
pIndentExact i  = pExact i (pSym ' ')

pBlock p i          = (pEOL *> pSegmentsBegin p i) <|> pure [] 

pSegmentEmpty       = pSpaces <* (pComment <|> pure NOP) <* pEOL 

pSegmentEnd p i     = pSpaces *> ((:) <$> pComment) <<?*> (pEOL *> pSegments p i <|> pure [])

pSegments p i       = pSegment p i <|> ( pSegmentEmpty *> pSegments p i )

pSegment p i        = ((:) <$ pIndentExact i <*> p i <*> pSegmentEnd p i) 

pSegmentsBegin p i  = pSegmentBegin p i <|> ( pSegmentEmpty *> pSegmentsBegin p i )

pSegmentBegin p i   = addLength 2 $ do
    j <- pIndentAtLast i
    (:) <$> p j <*> pSegmentEnd p j


tests = [
        --  ("Empty input",    ""                  , [])  
        --, ("Empty function", "def f():"         , [Function {name = "f", signature = [], body = []}]) 
        --, ("Simple function","def f(x=0):x"     , [Function {name = "f", signature = [Assignment (Identifier "x") (Constant (Integer "0"))], body = [Identifier "x"]}]) 
        --, ("Multiline function",
        --      "def f(x):\
        --    \\n    x()\
        --    \\n    y()\
        --    \\n"
        --                                        , [Function {name = "f", signature = [Identifier "x"], body = [Call {src = Identifier "x", args = []},Call {src = Identifier "y", args = []}]}]) 
        --, ("Multiple simple functions",
        --      "def f(x):x\
        --    \\ndef g(y):y"
        --                                        , [Function {name = "f", signature = [Identifier "x"], body = [Identifier "x"]},Function {name = "g", signature = [Identifier "y"], body = [Identifier "y"]}]) 
        --, ("Empty class", "class A:"            , [Class {name = "A", params = [], body = []}]) 
        --, ("Simple class",
        --      "class A:\
        --    \\n    i::Int"
        --                                        , [Class {name = "A", params = [], body = [Typed "Int" (Identifier "i")]}]) 
        --, ("Class with a function",
        --      "class A:\
        --    \\n    def f(x):\
        --    \\n        x"
        --                                        , [Class {name = "A", params = [], body = [Function {name = "f", signature = [Identifier "x"], body = [Identifier "x"]}]}]) 
        --, ("Testing emptylines 1",
        --      "class A:\
        --    \\n \
        --    \\n\
        --    \\n    def f(x):\
        --    \\n \
        --    \\n\
        --    \\n        x\n"
        --                                        , [Class {name = "A", params = [], body = [Function {name = "f", signature = [Identifier "x"], body = [Identifier "x"]}]}]) 
        --, ("Simple function call", "f(x,y,z)"   , [Call {src = Identifier "f", args = [Identifier "x",Identifier "y",Identifier "z"]}]) 
        --, ("Simple operators", "a+b"            , [Operator {name = "+", srd = Identifier "a", dst = Identifier "b"}]) 
        --, ("test", "f"                          , [Identifier "f"]) 
        --, ("test", "g h"                        , [Call {src = Identifier "g", args = [Identifier "h"]}]) 
        --, ("test", "g h()"                      , [Call {src = Identifier "g", args = [Call {src = Identifier "h", args = []}]}]) 
        --, ("test", "g h ()"                     , [Call {src = Identifier "g", args = [Identifier "h",Tuple {items = []}]}]) 
        --, ("test", "g(h,i)"                     , [Call {src = Identifier "g", args = [Identifier "h",Identifier "i"]}]) 
        --, ("test", "g h i"                      , [Call {src = Identifier "g", args = [Identifier "h",Identifier "i"]}]) 
        --, ("test", "g + 1"                      , [Operator {name = "+", srd = Identifier "g", dst = Constant (Integer "1")}]) 
        --, ("test", "g(x) h"                     , [Call {src = Call {src = Identifier "g", args = [Identifier "x"]}, args = [Identifier "h"]}]) 
        --, ("test", "g x h"                      , [Call {src = Identifier "g", args = [Identifier "x",Identifier "h"]}]) 

        --, ("Class with a function",
        --      "class Vector a:\
        --    \\n    x :: a     \
        --    \\n    y :: a     \
        --    \\n    z :: a     \
        --    \\n    def length(self):  \
        --    \\n        self.x^2 + self.y^2 + self.z^2\n"
        --                                        , [Class {name = "Vector", params = ["a"], body = [Typed "a" (Identifier "x"),Typed "a" (Identifier "y"),Typed "a" (Identifier "z"),Function {name = "length", signature = [Identifier "self"], body = [Operator {name = "+", srd = Operator {name = "+", srd = Operator {name = "^", srd = Accessor {src = Identifier "self", dst = Identifier "x"}, dst = Constant (Integer "2")}, dst = Operator {name = "^", srd = Accessor {src = Identifier "self", dst = Identifier "y"}, dst = Constant (Integer "2")}}, dst = Operator {name = "^", srd = Accessor {src = Identifier "self", dst = Identifier "z"}, dst = Constant (Integer "2")}}]}]}]) 

        ----, ("Simple lambda", "x:x+1")
        ----, ("Double lambda", "x: y:x+y")

        --, ("Exception catch", "a.catch e:         \
        --                    \\n    print 1        \
        --                    \\n    print e.message"
        --                                        , [Call {src = Accessor {src = Identifier "a", dst = Identifier "catch"}, args = [Lambda {signature = [Identifier "e"], body = [Call {src = Identifier "print", args = [Constant (Integer "1")]},Call {src = Identifier "print", args = [Accessor {src = Identifier "e", dst = Identifier "message"}]}]}]}]) 
        --, ("Simple Char literals", "'a'"        , [Constant (Char 'a')])
        --, ("Simple string literals", "\"ala\""  , [Constant (String "ala")])
        --, ("Simple import", "import Std.Math.Vector as Vector", [Import {paths = [Named {name = "Vector", item = Path {segments = ["Std","Math","Vector"]}}]}])
        --, ("Simple from import", "from Std.Math import Vector", [ImportQualified {path = Path {segments = ["Std","Math"]}, imports = Import {paths = [Path {segments = ["Vector"]}]}}])
        --, ("Complex import", 
        --   "import Std.Math.Vector as Vector\
        -- \\n       Std.Math.Scalar as Scalar"
        --                                        ,[Import {paths = [Named {name = "Vector", item = Path {segments = ["Std","Math","Vector"]}},Named {name = "Scalar", item = Path {segments = ["Std","Math","Scalar"]}}]}])
        --, ("Complex from import", 
        --   "from Std.Math import Vector\
        -- \\n                     Scalar"        
        --                                        ,[ImportQualified {path = Path {segments = ["Std","Math"]}, imports = Import {paths = [Path {segments = ["Vector"]},Path {segments = ["Scalar"]}]}}]
        --  )
        ----, ("test", "a=b;b=c", []) -- currently unimplemented
        --("Simple comment", "a=b #[c         \
        --                 \\ndalej komentarz \
        --                 \\n#[nested comment\
        --                 \\n#]xx            \
        --                 \\n#]", [])
        ("Simple comment2", "###################", [])

        ]


-- komentarze

---------- Program ----------


pProgram = pSegments pExpr 0 <?* pMany pSegmentEmpty <* pMany pComment

--pProgram = pSegments pExpr 0 <?* pMany pSegmentEmpty <* pMany pComment

--pProgram = pMany (pure NOP <* pSegmentEmpty)



--pProgram = pOpExpr 0

--pProgram = pTestOps 0

--pProgram = do
--    ((ind::Int,exprs):xs) <- pSegments pExpr
--    let
--        out = if ind /= 0
--            then throw "Co to za wciecie na poczatku?"
--            else if not $ null xs
--                then throw "Miss-matched indentations found."
--                else exprs
--    return out

type Parser a = P (Str Char String LineColPos) a


parse p s = UU.parse ( (,) <$> p <*> pEnd) (createStr (LineColPos 0 0 0) (s ++ "\n"))


main :: IO ()
main = do
    timeIt maininner
    return ()

maininner = do 
    --s     <- readFile "lunalib/Std/Math/Vector.luna"
    --run pProgram s [] False

    print "START"
    mapM_ (\(desc, inp, exp) -> do
              putStr ("=== " ++ desc ++ " ===" ++ replicate (30 - length desc) ' ')
              run pProgram inp exp False
          ) tests
    return ()

run :: Parser [Expr] -> String -> [Expr] -> Bool -> IO ()
run p inp exp force = 
    do 
        let (a, errors) =  parse p inp
        if (a == exp) && (null errors)
            then do putStrLn "ok."
            else putStrLn $ "\n--  Wrong Result:"

        if (a /= exp) || force || (not $ null errors)
            then do putStrLn $ "\n" ++ show a ++ "\n"
                    putStrLn $ "=== \n\n" ++ PP.ppShow a ++ "\n"
                    if null errors then  return ()
                                   else  do putStr ("--  Correcting steps: \n")
                                            show_errors errors
            else return ()
                            
                            
        where show_errors :: (Show a) => [a] -> IO ()
              show_errors = sequence_ . (map (putStrLn . show))



--pTestOps i = foldr pChainl (pValue i) [addops]

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
