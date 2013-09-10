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
import           Data.Foldable                             (foldlM)
import qualified Data.List                               as List

import           Flowbox.Prelude                           
import           Flowbox.Control.Error                     ()
import qualified Flowbox.Luna.Data.List                  as FList
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
import qualified Flowbox.Luna.Network.Graph.Port         as Port
import           Flowbox.Luna.Network.Graph.Port           (Port)
import           Flowbox.Luna.Network.Path.Import          (Import(Import))
import           Flowbox.Luna.Network.Path.Path            (Path(Path))
import qualified Flowbox.Luna.Passes.Pass                as Pass
import           Flowbox.Luna.Passes.Pass                  (PassMonad)
import qualified Flowbox.Luna.Passes.Txt2AST.Parser      as Parser
import qualified Flowbox.Luna.XOLD.Type.Type             as Type
import           Flowbox.Luna.XOLD.Type.Type               (Type)



type Graph2ASTMonad m = PassMonad Pass.NoState m


run :: PassMonad s m => DefManager -> (Definition.ID, Definition) -> Pass.Result m AST.Expr
run defManager (defID, def) = (Pass.runM Pass.NoState) $ def2AST defManager (defID, def)


def2AST :: Graph2ASTMonad m => DefManager -> (Definition.ID, Definition) -> Pass.Result m AST.Expr
def2AST defManager (defID, def) = do 
    let 
        nextDefs :: Graph2ASTMonad m => ((Definition.ID, Definition) -> Bool) -> Pass.Result m [AST.Expr]
        nextDefs f = (mapM (def2AST defManager) $ filter f $ DefManager.sucl defManager defID)

        classes :: (Definition.ID, Definition) -> Bool
        classes (_, Definition (Type.Class {}) _ _ _ _) = True
        classes (_, Definition {}                     ) = False

        methods :: (Definition.ID, Definition) -> Bool
        methods (_, Definition (Type.Function {}) _ _ _ _) = True
        methods (_, Definition {}                        ) = False

        modules :: (Definition.ID, Definition) -> Bool
        modules (_, Definition (Type.Module {}) _ _ _ _) = True
        modules (_, Definition {}                      ) = False

        getInputsNames :: Graph2ASTMonad m => Type -> Pass.Result m [String]
        getInputsNames (Type.Tuple items) = return $ map (fst . type2ASTType) items
        getInputsNames _                  = fail "Inputs is not a tuple"

    case def of 
        Definition _   _     _       (Flags _ True ) _ -> return $ AST.Comment ""
        Definition cls graph imports (Flags _ False) _ -> case cls of
            Type.Class _ _ params       -> AST.Class (snd $ type2ASTType cls)
                                                 <$> nextDefs classes
                                                 <*> pure (map type2Field params)
                                                 <*> nextDefs methods
            Type.Function name inputs _ -> do inputsNames <- getInputsNames inputs
                                              graphAst <- graph2AST graph inputsNames
                                              return $ AST.Function name (snd $ type2ASTType cls) graphAst
                                               -- notImplementedList
            Type.Module   _ params      -> AST.Module (snd $ type2ASTType cls)
                                                      (map import2ASTimport imports)
                                                  <$> nextDefs classes
                                                  <*> pure (map type2Field params)
                                                  <*> nextDefs methods
                                                  <*> nextDefs modules
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
    Type.Class        name params _       -> (""  , ASTType.Class name params)
    Type.Function     name inputs outputs -> (name, ASTType.Lambda (snd $ type2ASTType inputs) (snd $ type2ASTType outputs))
    Type.Tuple        items               -> (""  , ASTType.Tuple (map (snd.type2ASTType) items))
    Type.Module       name _              -> (name, ASTType.Module name)
    Type.Named        name cls            -> (name, snd $ type2ASTType cls)


defaultVal2ASTConstant :: DefaultValue -> ASTConstant.Constant
defaultVal2ASTConstant value = case value of 
    DefaultChar   v -> ASTConstant.Char  v
    DefaultInt    v -> ASTConstant.Integer v
    DefaultString v -> ASTConstant.String  v


type2Field :: Type -> AST.Expr
type2Field t = AST.Field name cls where
    (name, cls) = type2ASTType t


import2ASTimport :: Import -> AST.Expr
import2ASTimport (Import (Path path) name) = AST.Import path name


graph2AST :: Graph2ASTMonad m => Graph -> [String] -> Pass.Result m [AST.Expr]
graph2AST graph inputsNames = foldlM (node2AST graph inputsNames) [] $ Graph.topsortl graph


node2AST :: Graph2ASTMonad m => Graph -> [String] -> [AST.Expr] -> (Node.ID, Node.Node) -> Pass.Result m [AST.Expr] 
node2AST graph inputsNames list (nodeID, node) = do
    let 
        return' :: Graph2ASTMonad m => AST.Expr -> Pass.Result m [AST.Expr] 
        return' a = return (list ++ [a])

    case node of 
        Node.Expr expression (Flags _ True) _ -> return' $ AST.Comment expression
        Node.Inputs          _              _ -> return list
        Node.Outputs         (Flags _ True) _ -> return' $ AST.Comment ""
        Node.Tuple           (Flags _ True) _ -> return' $ AST.Comment ""
        _                                     -> do
            let 
                order :: (Node.ID, Node.Node, Edge) -> (Node.ID, Node.Node, Edge) -> Ordering
                order (_, _, Edge _ a) (_, _, Edge _ b) = compare a b

                resultName :: Node.ID -> Port -> String
                resultName nID port = case Graph.lab graph nID of 
                    Just (Node.Inputs {} ) -> case port of 
                                                Port.All      -> "arguments"
                                                Port.Number p -> inputsNames !! p
                    _                      -> case port of 
                                                Port.All      -> "result_" ++ show nID
                                                Port.Number p -> "result_" ++ show nID ++ "_" ++ show p


                argName :: (Node.ID, a, Edge) -> AST.Expr
                argName (nID, _, Edge srcPort _) = AST.Identifier $ resultName nID srcPort

                argNodes = List.sortBy order $ Graph.lprel graph nodeID
                argNames = map argName argNodes

            call <- case node of 
                Node.Expr expression _ _ -> case Parser.parseExpr expression of 
                                                    Left  e    -> fail $ show e
                                                    Right expr -> return $ AST.Call expr argNames
                Node.Default value _ -> return $ AST.Constant $ defaultVal2ASTConstant value
                Node.Inputs  _     _ -> return   AST.NOP
                Node.Outputs _     _ -> return $ AST.Tuple argNames
                Node.Tuple   _     _ -> return $ AST.Tuple argNames


            let inEdges          = Graph.out graph nodeID 
                allConnected     = length inEdges
                gettersConnected = FList.count isGetter inEdges

                isGetter :: (Node.ID, Node.ID, Edge) -> Bool
                isGetter (_, _, Edge (Port.Number _) _) = True
                isGetter _                              = False

                allPattern     = AST.Pattern $ AST.Identifier $ resultName nodeID Port.All
                gettersPattern = AST.Pattern $ AST.Tuple $ map (\i -> AST.Identifier $ resultName nodeID $ Port.Number i) [0..(allConnected - 1)] 

            if gettersConnected == 0 
                then return' $ AST.Assignment allPattern call
                else if gettersConnected == allConnected
                    then return' $ AST.Assignment gettersPattern call
                    else return  $ list ++ [AST.Assignment allPattern call
                                           ,AST.Assignment gettersPattern allPattern]
