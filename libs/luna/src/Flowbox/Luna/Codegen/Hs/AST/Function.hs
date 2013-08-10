---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Codegen.Hs.AST.Function (
    Function(..),
    empty,
    basic,
    genCode,
    --addExpr,
    addAlias,
    --setCtx,
    setBody,
    getter,
    setter,
    mkSpec
)where

import           Debug.Trace                        

import qualified Flowbox.Luna.Codegen.Hs.AST.Expr as Expr
import           Flowbox.Luna.Codegen.Hs.AST.Expr   (Expr)
import           Data.String.Utils                  (join)
import qualified Flowbox.Luna.Codegen.Hs.Path     as Path


data Function = Function { name       :: String,
                           signature  :: [Expr],
                           body       :: Expr
                         } deriving (Show)


empty :: Function
empty = Function "" [] (Expr.mkBlock Path.outputs) 


basic :: Function
basic = empty { signature = [Expr.Var Path.inputs] }


ctx :: Function -> Expr.Context
ctx func = Expr.ctx $ body func

genCode :: Function -> String
genCode func =  head' ++ " = " ++ body' ++ "\n"
             ++ headM ++ " = " ++ bodyM where
    fname    = name func 
    header   = fname ++ " "
    headerM  = Path.mkMonadName fname ++ " "
    inputs   = join " " (map Expr.genCode $ signature func)
    head'    = header  ++ inputs
    headM    = headerM ++ inputs
    body'    = genBodyPure func
    bodyM    = case ctx func of
                   Expr.IO   -> genBodyM func
                   Expr.Pure -> "return $ " ++ head'


genBodyM :: Function -> String
genBodyM func = Expr.genCode (body func) 


genBodyPure :: Function -> String
genBodyPure func = Expr.genCode (Expr.mkPure $ body func) 


--simple :: String -> Expr -> Function
--simple name' expr = Function name' [Path.signature] [expr] Expr.Pure

--setCtx :: Expr.Context -> Function -> Function
--setCtx nctx func = func{ctx = nctx}


setBody :: Expr -> Function -> Function
setBody expr func = func { body = expr }

--addExpr :: Expr -> Function -> Function
--addExpr expr func = func { body = expr : body func }


addAlias :: (String, String) -> Function -> Function
addAlias alias func = setBody (Expr.addExpr (Expr.mkAlias alias) $ body func) func


getter :: String -> String -> String -> Expr
getter obj _ param = Expr.Call "getter''" [Expr.THTypeCtx obj, Expr.THExprCtx param] Expr.Pure


setter :: String -> String -> String -> Expr
setter obj _ param = Expr.Call "setter''" [Expr.THTypeCtx obj, Expr.THExprCtx param] Expr.Pure



mkSpec :: String -> String -> String -> Function
mkSpec spec name' basefunc = empty { name      = name'
                                   , signature = [Expr.At Path.inputs (Expr.Tuple [Expr.Cons spec [], Expr.Any])]
                                   , body      = Expr.Call basefunc [Expr.Var Path.inputs] Expr.IO
                                   }