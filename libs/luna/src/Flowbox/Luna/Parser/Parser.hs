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



import Control.Exception

import qualified Text.Show.Pretty as PP


--s1 = "def a(x,y,z):\
--   \\n  a\
--   \\n  b=a\
--   \\n"

--s1 = "class Ala:\
--   \\n    a::Int   \
--   \\n    def f(x):\
--   \\n        x\n"



--def f(x,y,z):
--  a = x
--      + y
--        + z


--s1 = "def f(x):\
--   \\n  x\
--   \\n y"

--s1 = "from Std.Math import m.Vector as V\
--   \\n                     Scalar\
--   \\nimport A.B"

--s1 = "import Std.Math.Vector as Vector\
--   \\n       Std.String as S"


--import Std.Math.Vector as V
--       Std.Math.Scalar as S
--from Std.Math import Vector as V
--                     Scalar as S

--Imports { path :: [String], elems :: [String] }

---------- Entities ----------

pIdent'      = Identifier       <$> L.pVarIdent'
pTypeIdent   = TypeIdentifier   <$> L.pTypeIdent
pInteger     = Constant.Integer <$> L.pInteger
pConstant    = Constant         <$> pChoice [ pInteger ]
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

pImport i     =    (ImportQualified <$ Keywords.pFrom <*> pImportPath i) 
              <?*> (Import <$  Keywords.pImport  <*> ((:) <$> pImportPath i) <?*> pBlock pImportPath (i+1))

--pAssignment   = Assignment <$> pEnt <* L.pAssign <*> (pExpr 0 <|> pEnt)

pFunc i       = Function   <$  Keywords.pDef 
                           <*> L.pVarIdent 
                           <*> (pTuplePure (pOpExpr2 i) <<|> pTupleBody (pOpExpr2 i))
                           <*> pExprBlock i

pLambda i     = Lambda     <$> (pTuplePure (pOpExpr2 i) <<|> pTupleBody (pOpExpr2 i))
                           <*> pExprBlock i

pClass i      = Class      <$ Keywords.pClass
                           <*> L.pTypeClsIdent
                           <*> pMany L.pTypeVarIdent
                           <*> pExprBlock i

pInterface i  = Interface  <$ Keywords.pInterface
                           <*> L.pTypeIdent
                           <*> pExprBlock i

--pCall i       = Call       <$> pTrace L.pIdent' <*> ( (items <$> pTuple i) <<|> pSpaces *> (pMany $ pTupleItem i) )   -- <* pSpaces <*> pMany (pValue i)

--pExprBlock i  = L.pBlockBegin *> pBlock pExpr (i+1)

pCall i       = Call <$> pIdent' <*> pTuplePure (pOpExpr i)

pExprBlock i  = L.pBlockBegin *> ((:[]) <$> pExpr i <<|> pBlock pExpr (i+1))

pOpExpr i     = aftermatch <$> foldr pChainL (pFactor i) pPrecOps

pOpExpr2 i     = aftermatch <$> foldr pChainL (pFactor2 i) pPrecOps

pFactor i = pChoice [ pFactor2 i
                    , pLambda i
                    ]

pFactor2 i = pChoice [ pEnt
                    , pCall i
                    , L.pParensed (pOpExpr i)
                    , pTuple i
                    , pTyped pEnt
                    --, pLambda i
                    ]

pExpr i       = pChoice [ 
                        --pImport i
                        pFunc i
                        --, pLambda i
                        , pClass i
                        , pOpExpr i
                        ]
              -- <|> pNOP



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

pBlock p i          = (pEOL *> pSegmentsBegin p i) <|> pure [] -- ->>> pSegmentsBegin->pSegments

pSegmentEmpty       = pMany L.pWSpace <* pEOL 

pSegments p i       = pSegment p i <|> ( pSegmentEmpty *> pSegments p i )

pSegment p i        = (:) <$ pIndentExact i <*> p i <*> ((pEOL *> pSegments p i) <|> pReturn [])

pSegmentsBegin p i  = pSegmentBegin p i <|> ( pSegmentEmpty *> pSegmentsBegin p i )

pSegmentBegin p i   = addLength 2 $ do
    j <- pIndentAtLast i
    (:) <$> p j <*> ((pEOL *> pSegments p j) <|> pure [])


tests = [("Empty input",    "")  
        , ("Empty function", "def f():")
        , ("Simple function","def f(x=0):x")
        , ("Multiline function",
              "def f(x):\
            \\n    x()\
            \\n    y()\
            \\n"
          )
        , ("Multiple simple functions",
              "def f(x):x\
            \\ndef g(y):y"
          )
        , ("Empty class", "class A:")
        , ("Simple class",
              "class A:\
            \\n    i::Int"
          )
        , ("Class with a function",
              "class A:\
            \\n    def f(x):\
            \\n        x"
          )
        , ("Testing emptylines 1",
              "class A:\
            \\n \
            \\n\
            \\n    def f(x):\
            \\n \
            \\n\
            \\n        x\n"
          )
        , ("Simple function call", "f(x,y,z)")
        , ("Simple operators", "a+b")
        --("test", "f"),
        --("test", "g h"),
        --("test", "g h()"),
        --("test", "g h ()"),
        --("test", "g(h,i)"),
        --("test", "g h i"),
        --, ("test", "g + 1")
        --("test", "g(x) h"),
        --("test", "g x h")

        , ("Class with a function",
              "class Vector a:\
            \\n    x :: a     \
            \\n    y :: a     \
            \\n    z :: a     \
            \\n    def length(self):  \
            \\n        self.x^2 + self.y^2 + self.z^2\n"
          )

        , ("Simple lambda", "x:x")

        ]

rep x i = if i == 0
    then x
    else x ++ rep x (i-1)

--g()(h)
--g(h)

--(g) h
--g h

---------- Program ----------

pProgram = pSegments pExpr 0 <* pMany pSegmentEmpty `opt` []

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


parse p s = UU.parse ( (,) <$> p <*> pEnd) (createStr (LineColPos 0 0 0) s)


main :: IO ()
main = do 
    print "START"
    mapM_ (\(desc, p) -> putStrLn ("\n=== " ++ desc ++ " ===") >> run pProgram p) tests
    return ()

run :: Show t =>  Parser t -> String -> IO ()
run p inp = do  let (a, errors) =  parse p inp
                --print a
                putStrLn ("--  Result: \n" ++ PP.ppShow a)
                if null errors then  return ()
                               else  do putStr ("--  Correcting steps: \n")
                                        show_errors errors
                putStrLn "-- "
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
