---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds, TupleSections #-}

module Flowbox.Luna.Passes.SSA.SSA where

import qualified Flowbox.Luna.Parser.AST.AST            as LAST
import qualified Flowbox.Luna.Parser.AST.Type           as Type
import           Flowbox.Luna.Parser.AST.Type             (Type)
import qualified Flowbox.Luna.Parser.AST.Constant       as LConstant
import qualified Flowbox.Luna.Passes.HSGen.AST.Expr     as Expr
import           Flowbox.Luna.Passes.HSGen.AST.Expr       (Expr)
import qualified Flowbox.Luna.Passes.HSGen.AST.Constant as Constant
import qualified Flowbox.Luna.Passes.HSGen.AST.Module   as Module
import qualified Flowbox.Luna.Passes.HSGen.AST.DataType as DataType
import qualified Flowbox.Luna.Passes.HSGen.AST.Function as Function
import qualified Flowbox.Luna.Passes.HSGen.AST.Cons     as Cons
import           Flowbox.Luna.Passes.HSGen.AST.Function   (Function)
import qualified Flowbox.Luna.Passes.SSA.State          as SSAState
import           Flowbox.Luna.Passes.SSA.State            (SSAState)
import qualified Flowbox.Luna.Passes.Pass               as Pass
import           Flowbox.Luna.Passes.Pass                 (PassMonad)

import           Control.Monad.State                      
import           Control.Applicative                      

import           Debug.Trace                              

import           Control.Monad.State                      
import           Control.Monad.Writer                     
import           Control.Monad.RWS                        
import           Control.Monad.Trans.Maybe                
import           Control.Monad.Trans.Either               
import           Data.Maybe                               (fromJust)

import qualified Flowbox.System.Log.Logger              as Logger
import           Flowbox.System.Log.Logger                
import qualified Flowbox.System.Log.LogEntry            as LogEntry

import qualified Prelude                                as Prelude
import           Prelude                                hiding (error)

import           Control.Error                            

logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.SSA"


data Mode = Write | Read

type SSAMonad m = PassMonad SSAState m


run :: PassMonad s m => LAST.Expr -> Pass.Result m LAST.Expr
run = (Pass.runM SSAState.empty) . (ssaAST Read)


runNested :: SSAMonad m => Pass.Transformer SSAState a m b 
runNested = Pass.runM SSAState.empty


ssaAST :: SSAMonad m => Mode -> LAST.Expr -> Pass.Result m LAST.Expr
ssaAST mode ast = case ast of
    LAST.Program    body                  -> LAST.Program <$> mapM (ssaAST mode) body
    LAST.Function   name signature body   -> runNested $ do
                                                    SSAState.registerVar (name, name)
                                                    ssaType signature
                                                    LAST.Function name signature <$> mapM (ssaAST mode) body
    LAST.Assignment src dst               -> flip LAST.Assignment <$> ssaAST mode dst <*> ssaAST Write src
    LAST.Pattern    pat                   -> LAST.Pattern         <$> ssaAST mode pat
    LAST.Identifier name                  -> case mode of
                                                 Write -> LAST.Identifier <$> SSAState.handleVar name
                                                 Read  -> do
                                                     v <- SSAState.lookupVar name
                                                     case v of
                                                        Nothing      -> (logger error $ "Not in scope: '" ++ name ++ "'") *> Pass.fail
                                                        Just newname -> return $ LAST.Identifier newname
    LAST.Operator   name src dst          -> LAST.Operator name <$> ssaAST mode src <*> ssaAST mode dst
    LAST.Call       src args              -> LAST.Call <$> ssaAST mode src <*> mapM (ssaAST mode) args
    LAST.Constant {}                      -> return ast
    _                                     -> logger error "SSA Pass error: Unknown expression." *> Pass.fail


ssaType :: SSAMonad m => Type -> Pass.Result m ()
ssaType ast = case ast of
    Type.Lambda inputs outputs -> ssaType inputs
    Type.Tuple  items          -> mapM ssaType items *> return ()
    Type.Type   name           -> SSAState.registerVar (name, name)
