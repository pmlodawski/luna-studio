---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds, TupleSections #-}

module Flowbox.Luna.Passes.HSGen.HSGen where

--import           Flowbox.Prelude                          
--import qualified Flowbox.Luna.AST.AST                   as LAST
--import qualified Flowbox.Luna.AST.Type                  as Type
--import           Flowbox.Luna.AST.Type                    (Type)
--import qualified Flowbox.Luna.AST.Constant              as LConstant
--import qualified Flowbox.Luna.Passes.HSGen.AST.Expr     as HAST
--import qualified Flowbox.Luna.Passes.HSGen.AST.Constant as Constant
--import qualified Flowbox.Luna.Passes.HSGen.AST.Module   as Module
--import qualified Flowbox.Luna.Passes.HSGen.AST.DataType as DataType
--import qualified Flowbox.Luna.Passes.HSGen.AST.Cons     as Cons
--import qualified Flowbox.Luna.Passes.HSGen.GenState     as GenState
--import           Flowbox.Luna.Passes.HSGen.GenState       (GenState)
--import qualified Flowbox.Luna.Passes.Pass               as Pass
--import           Flowbox.Luna.Passes.Pass                 (PassMonad)

--import           Control.Monad.State                      
--import           Control.Applicative                      

--import           Debug.Trace                              

--import           Control.Monad.State                      
--import           Control.Monad.Writer                     
--import           Control.Monad.RWS                        
--import           Control.Monad.Trans.Maybe                
--import           Control.Monad.Trans.Either               
--import           Data.Maybe                               (fromJust)

--import qualified Flowbox.System.Log.Logger              as Logger
--import           Flowbox.System.Log.Logger                
--import qualified Flowbox.System.Log.LogEntry            as LogEntry

--import qualified Prelude                                as Prelude
--import           Prelude                                hiding (error)

--logger :: Logger
--logger = getLogger "Flowbox.Luna.Passes.HSGen.HSGen"

--type GenMonad m = PassMonad GenState m




--run :: PassMonad s m => LAST.Expr -> Pass.Result m HAST.Expr
--run = (Pass.runM GenState.empty) . genModule


--genModule :: GenMonad m => LAST.Expr -> Pass.Result m HAST.Expr
--genModule ast = case ast of
--    LAST.Module cls imports classes fields methods modules -> do GenState.setModule Module.empty
--                                                                 mapM (genHAST >=> GenState.addDataType) classes
--                                                                 mapM (genHAST >=> GenState.addImport)   imports
--                                                                 mapM (genHAST >=> GenState.addMethod)   methods
--                                                                 GenState.getModule
--    _                                                      -> fail "o nie"


--genHAST :: GenMonad m => LAST.Expr -> Pass.Result m HAST.Expr
--genHAST ast = case ast of
--    --LAST.Module     cls classes fields
--    --                methods modules     -> do GenState.setModule Module.empty
--    --                                          mapM genHAST classes *> return ()
                         
--    --LAST.Constant   cst                 -> case cst of
--    --                                           LConstant.Integer val -> return $ HAST.Constant $ Constant.Integer val
--    --                                           _                     -> logger criticalFail "Unknown LUNA.AST HASTession"
--    LAST.Identifier name                -> return $ HAST.Var (name)
                                 
--    LAST.Function   name signature body -> do
--                                           lambda <- genType signature
--                                           body'  <- mapM genHAST body
--                                           return $ lambda { HAST.name = name
--                                                           , HAST.expr = HAST.LetBlock body' HAST.NOP
--                                                           }
--                                            --HAST.Function name <$> return [] <*> mapM genHAST body

--    --LAST.Import segments name           -> 
--    --LAST.Class  cls classes fields 
--    --            methods                 -> do
--    --                                       efields <- mapM genField fields
--    --                                       let name = Type.name cls
--    --                                           cons = Cons.empty { HAST.name   = name
--    --                                                             , HAST.fields = efields
--    --                                                             }
--    --                                       return $ DataType.empty { HAST.name         = name
--    --                                                               , HAST.params       = Type.params cls
--    --                                                               , HAST.constructors = [cons]
--    --                                                               }  

--    LAST.Import segments name           -> return $ HAST.Import segments name

--    LAST.Class  cls classes fields 
--                methods                 -> do cons   <- HAST.Cons name <$> mapM genField fields
--                                              return $  HAST.DataType name params [cons] 
--                                              where name   =  Type.name   cls
--                                                    params =  Type.params cls

--    LAST.Operator name src dst          -> HAST.Operator name <$> genHAST src <*> genHAST dst



                                            
--genType :: GenMonad m => Type -> Pass.Result m HAST.Expr
--genType t = case t of
--    Type.Type   name             -> return $ HAST.Var ("" ++ name)
--    Type.Tuple  items            -> HAST.Tuple <$> mapM genType items
--    Type.Lambda inputs outputs   -> do
--                                    inputs' <- HAST.items <$> genType inputs
--                                    return $ HAST.Function "" inputs' (HAST.NOP)

--genField :: GenMonad m => LAST.Expr -> Pass.Result m HAST.Expr
--genField (LAST.Field name t) = return $ HAST.Typed (Type.name t) (HAST.Var name)



--    -- Class name params []
--    --LAST.Operator   name src dst -> HAST.Operator name <$> genHAST src <*> genHAST dst
--    ----LAST.Identifier name         -> do
--    --                                    --vname <- SSAState.genVarName
--    --                                    --return $ HAST.Var vname
--    ----LAST.Assignment src dst      -> do
--    --                                    --dst' <- genHAST dst
--    --                                    --src' <- SSAState.genVarName
--    --                                    --SSAState.registerVar src' src
--    --                                    --return $ HAST.Assignment (HAST.Var src') dst' HAST.Pure
--    --_ -> return HAST.NOP




------data X a b c = X{a::a,b::b,c::c} | Y

