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
import           Control.Monad.Writer                      
import           Control.Monad.RWS                         
import           Control.Monad.Trans.Maybe                 
import           Control.Monad.Trans.Either                
import           Data.Foldable                             (foldrM)
import           Data.Maybe                                (fromJust)
import qualified Data.List                               as List

import           Flowbox.Prelude                           
import           Flowbox.Control.Error                     
import qualified Flowbox.Luna.AST.AST                    as AST
import qualified Flowbox.Luna.AST.Type                   as ASTType
import qualified Flowbox.Luna.AST.Constant               as ASTConstant
import qualified Flowbox.Luna.Network.Attributes         as Attributes
import qualified Flowbox.Luna.Network.Def.DefManager     as DefManager
import           Flowbox.Luna.Network.Def.DefManager       (DefManager)
import qualified Flowbox.Luna.Network.Def.Definition     as Definition
import           Flowbox.Luna.Network.Def.Definition       (Definition(..))
import qualified Flowbox.Luna.Network.Flags              as Flags
import           Flowbox.Luna.Network.Flags                (Flags(..))
import qualified Flowbox.Luna.Network.Graph.Graph        as Graph
import           Flowbox.Luna.Network.Graph.Graph          (Graph)
import qualified Flowbox.Luna.Network.Graph.DefaultValue as DefaultValue
import           Flowbox.Luna.Network.Graph.DefaultValue   (DefaultValue(..))
import qualified Flowbox.Luna.Network.Graph.Edge         as Edge
import           Flowbox.Luna.Network.Graph.Edge           (Edge(..))
import qualified Flowbox.Luna.Network.Graph.Node         as Node
import qualified Flowbox.Luna.Passes.Pass                as Pass
import           Flowbox.Luna.Passes.Pass                  (PassMonad)
import qualified Flowbox.Luna.Passes.Txt2AST.Parser      as Parser
import qualified Flowbox.Luna.XOLD.Type.Type             as Type
import           Flowbox.Luna.XOLD.Type.Type               (Type)
import           Flowbox.System.Log.Logger                 
import qualified Flowbox.System.UniPath                  as UniPath
import           Flowbox.System.UniPath                    (UniPath)



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Graph2AST.Graph2AST"

type Graph2ASTMonad m = PassMonad Pass.NoState m


run :: PassMonad s m => DefManager -> (Definition.ID, Definition) -> Pass.Result m AST.Expr
run defManager (defID, def) = (Pass.runM Pass.NoState) $ def2AST defManager (defID, def)



def2AST :: Graph2ASTMonad m => DefManager -> (Definition.ID, Definition) -> Pass.Result m AST.Expr
def2AST defManager (defID, def) = do 
    let (Definition acls graph aimports (Flags _ aomit) _) = def
    case acls of
        Type.Class _ _ params             -> do let astCls    = (snd . toASTType) acls
                                                    astFields = map type2Field params
                                                astMethods <- mapM (def2AST defManager) $ DefManager.sucl defManager defID
                                                return $ AST.Class astCls astFields astMethods
        Type.Function name inputs outputs -> AST.Function name (snd $ toASTType acls) <$> graph2AST graph
        _                          -> return AST.NOP
    



notImplementedList :: [a]
notImplementedList = []


toASTType :: Type -> (String, ASTType.Type)
toASTType t = case t of 
    Type.Undefined                           -> (""  , ASTType.Unknown)
    Type.Type         name                   -> (name, ASTType.Type name)
    Type.TypeVariable name                   -> (name, ASTType.Unknown)
    Type.Class        name typeparams params -> (""  , ASTType.Class name typeparams)
    Type.Function     name inputs outputs    -> (name, ASTType.Lambda (snd $ toASTType inputs) (snd $ toASTType outputs))
    Type.Tuple        items                  -> (""  , ASTType.Tuple (map (snd.toASTType) items))
    Type.List         item                   -> (""  , ASTType.Unknown)
    Type.Interface    fields methods         -> (""  , ASTType.Unknown)
    Type.Module       name                   -> (name, ASTType.Unknown)
    Type.Named        name acls              -> (name, snd $ toASTType acls)


toASTConstant :: DefaultValue -> ASTConstant.Constant
toASTConstant value = case value of 
    DefaultChar   v -> ASTConstant.Char  v
    DefaultInt    v -> ASTConstant.Integer v
    DefaultString v -> ASTConstant.String  v


type2Field :: Type -> AST.Expr
type2Field t = AST.Field name cls where
    (name, cls) = toASTType t


graph2AST :: Graph2ASTMonad m => Graph -> Pass.Result m [AST.Expr]
graph2AST graph = foldrM (node2AST graph) [] $ Graph.topsortl graph


node2AST :: Graph2ASTMonad m => Graph -> (Node.ID, Node.Node) -> [AST.Expr] -> Pass.Result m [AST.Expr] 
node2AST graph (nodeID, node) list = do
    let 
        order :: (Node.ID, Node.Node, Edge) -> (Node.ID, Node.Node, Edge) -> Ordering
        order (_, _, Edge a) (_, _, Edge b) = compare a b

        resultName :: Node.ID -> AST.Expr
        resultName id = AST.Identifier $ "result_" ++ show id

        argName :: (Node.ID, Node.Node, Edge) -> AST.Expr
        argName (id, _, _) = resultName id


        argNodes = List.sortBy order $ Graph.lprel graph nodeID
        argNames = map argName argNodes

    call <- case node of 
        Node.Expr expression flags _ -> case Parser.parseExpr expression of 
                                            Left  err  -> fail $ show err
                                            Right expr -> return $ AST.Call expr argNames
        Node.Default value _ -> return $ AST.Constant $ toASTConstant value
        Node.Inputs  flags _ -> return AST.NOP
        Node.Outputs flags _ -> return $ AST.Tuple argNames
        Node.NTuple  flags _ -> return $ AST.Tuple argNames

    let assignment =  AST.Assignment (AST.Pattern $ resultName nodeID) call

    return $ list ++ [assignment]