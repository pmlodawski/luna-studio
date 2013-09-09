---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds, TupleSections #-}

module Flowbox.Luna.Passes.Graph2AST.Graph2AST where

import           Control.Applicative                       
import           Control.Monad.State                       
import           Data.Foldable                             (foldrM)
import qualified Data.List                               as List

import           Flowbox.Prelude                           
import           Flowbox.Control.Error                     ()
import qualified Flowbox.Luna.AST.AST                    as AST
import qualified Flowbox.Luna.AST.Type                   as ASTType
import qualified Flowbox.Luna.AST.Constant               as ASTConstant
import qualified Flowbox.Luna.Network.Def.DefManager     as DefManager
import           Flowbox.Luna.Network.Def.DefManager       (DefManager)
import qualified Flowbox.Luna.Network.Def.Definition     as Definition
import           Flowbox.Luna.Network.Def.Definition       (Definition(Definition))
import           Flowbox.Luna.Network.Flags                (Flags(Flags))
import qualified Flowbox.Luna.Network.Graph.Graph        as Graph
import           Flowbox.Luna.Network.Graph.Graph          (Graph)
import           Flowbox.Luna.Network.Graph.DefaultValue   (DefaultValue(..))
import           Flowbox.Luna.Network.Graph.Edge           (Edge(Edge))
import qualified Flowbox.Luna.Network.Graph.Node         as Node
import qualified Flowbox.Luna.Passes.Pass                as Pass
import           Flowbox.Luna.Passes.Pass                  (PassMonad)
import qualified Flowbox.Luna.Passes.Txt2AST.Parser      as Parser
import qualified Flowbox.Luna.XOLD.Type.Type             as Type
import           Flowbox.Luna.XOLD.Type.Type               (Type)
import           Flowbox.System.Log.Logger                 



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Graph2AST.Graph2AST"

type Graph2ASTMonad m = PassMonad Pass.NoState m


run :: PassMonad s m => DefManager -> (Definition.ID, Definition) -> Pass.Result m AST.Expr
run defManager (defID, def) = (Pass.runM Pass.NoState) $ def2AST defManager (defID, def)


def2AST :: Graph2ASTMonad m => DefManager -> (Definition.ID, Definition) -> Pass.Result m AST.Expr
def2AST defManager (defID, def) = do 
    let 
        nextDefs :: Graph2ASTMonad m => Pass.Result m [AST.Expr]
        nextDefs = (mapM (def2AST defManager) $ DefManager.sucl defManager defID)

    case def of 
        Definition _   _     _       (Flags _ True ) _ -> return $ AST.Comment ""
        Definition cls graph imports (Flags _ False) _ -> case cls of
            Type.Class _ _ params  -> AST.Class (snd $ type2ASTType cls)
                                                (map type2Field params)
                                            <$> nextDefs
            Type.Function name _ _ -> AST.Function name (snd $ type2ASTType cls) <$> graph2AST graph
            Type.Module   name     -> AST.Module <$> (module2ASTPath defManager defID name)
                                                 <*>  nextDefs
            _                      -> return AST.NOP
        

module2ASTPath :: Graph2ASTMonad m => DefManager -> Definition.ID -> String -> Pass.Result m AST.Expr
module2ASTPath defManager defID name = do
    let 
        module2ASTPath' :: Graph2ASTMonad m => Definition.ID -> Pass.Result m [String]
        module2ASTPath' dID = case DefManager.prel defManager dID of
            [(preID, Definition cls _ _ _ _)] -> do prev <- (module2ASTPath' preID)
                                                    return $ prev ++ [Type.name cls]
            []                                -> return []
            _                                 -> fail "Definition has multiple parents"
    prev <- module2ASTPath' defID
    return $ AST.Path $ prev ++ [name]



notImplementedList :: [a]
notImplementedList = []


type2ASTType :: Type -> (String, ASTType.Type)
type2ASTType t = case t of 
    Type.Undefined                        -> (""  , ASTType.Unknown)
    Type.TypeName     name                -> (name, ASTType.Type name)
    Type.Class        name typeparams _   -> (""  , ASTType.Class name typeparams)
    Type.Function     name inputs outputs -> (name, ASTType.Lambda (snd $ type2ASTType inputs) (snd $ type2ASTType outputs))
    Type.Tuple        items               -> (""  , ASTType.Tuple (map (snd.type2ASTType) items))
    Type.Interface    fields methods      -> (""  , ASTType.Unknown)
    Type.Module       name                -> (name, ASTType.Unknown)
    Type.Named        name cls            -> (name, snd $ type2ASTType cls)


defaultVal2ASTConstant :: DefaultValue -> ASTConstant.Constant
defaultVal2ASTConstant value = case value of 
    DefaultChar   v -> ASTConstant.Char  v
    DefaultInt    v -> ASTConstant.Integer v
    DefaultString v -> ASTConstant.String  v


type2Field :: Type -> AST.Expr
type2Field t = AST.Field name cls where
    (name, cls) = type2ASTType t


graph2AST :: Graph2ASTMonad m => Graph -> Pass.Result m [AST.Expr]
graph2AST graph = foldrM (node2AST graph) [] $ Graph.topsortl graph


node2AST :: Graph2ASTMonad m => Graph -> (Node.ID, Node.Node) -> [AST.Expr] -> Pass.Result m [AST.Expr] 
node2AST graph (nodeID, node) list = do
    let 
        return' :: Graph2ASTMonad m => AST.Expr -> Pass.Result m [AST.Expr] 
        return' a = return (list ++ [a])

    case node of 
        Node.Expr expression (Flags _ True) _ -> return' $ AST.Comment expression
        Node.Inputs          (Flags _ True) _ -> return' $ AST.Comment ""
        Node.Outputs         (Flags _ True) _ -> return' $ AST.Comment ""
        Node.NTuple          (Flags _ True) _ -> return' $ AST.Comment ""
        _                                     -> do
            let 
                order :: (Node.ID, Node.Node, Edge) -> (Node.ID, Node.Node, Edge) -> Ordering
                order (_, _, Edge a) (_, _, Edge b) = compare a b

                resultName :: Node.ID -> AST.Expr
                resultName nID = AST.Identifier $ "result_" ++ show nID

                argName :: (Node.ID, Node.Node, Edge) -> AST.Expr
                argName (nID, _, _) = resultName nID


                argNodes = List.sortBy order $ Graph.lprel graph nodeID
                argNames = map argName argNodes

            call <- case node of 
                Node.Expr expression _ _ -> case Parser.parseExpr expression of 
                                                    Left  e    -> fail $ show e
                                                    Right expr -> return $ AST.Call expr argNames
                Node.Default value _ -> return $ AST.Constant $ defaultVal2ASTConstant value
                Node.Inputs  _     _ -> return   AST.NOP
                Node.Outputs _     _ -> return $ AST.Tuple argNames
                Node.NTuple  _     _ -> return $ AST.Tuple argNames

            return' $ AST.Assignment (AST.Pattern $ resultName nodeID) call