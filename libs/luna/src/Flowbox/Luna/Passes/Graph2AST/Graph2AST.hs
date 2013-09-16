---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds, TupleSections #-}

module Flowbox.Luna.Passes.Graph2AST.Graph2AST (
    run
) where

import           Control.Applicative                       
import           Control.Monad.State                       
import           Data.Foldable                             (foldlM)
import qualified Data.List                               as List

import           Flowbox.Prelude                           
import           Flowbox.Control.Error                     ()
import qualified Flowbox.Luna.Data.List                  as FList
import qualified Flowbox.Luna.AST.Expr                   as ASTExpr
import qualified Flowbox.Luna.AST.Lit                    as ASTLit
import qualified Flowbox.Luna.AST.Pat                    as ASTPat
import qualified Flowbox.Luna.AST.Type                   as ASTType
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
import qualified Flowbox.Luna.Passes.Graph2AST.IdState   as IdState
import           Flowbox.Luna.Passes.Graph2AST.IdState     (IdState)
import qualified Flowbox.Luna.Passes.Pass                as Pass
import           Flowbox.Luna.Passes.Pass                  (PassMonad)
import qualified Flowbox.Luna.Passes.Txt2AST.Parser      as Parser
import qualified Flowbox.Luna.XOLD.Type.Type             as Type
import           Flowbox.Luna.XOLD.Type.Type               (Type)


type Graph2ASTMonad m = PassMonad IdState m


run :: PassMonad s m => DefManager -> (Definition.ID, Definition) -> Pass.Result m ASTExpr.Expr
run defManager (defID, def) = (Pass.run_ IdState.empty) $ def2AST defManager (defID, def)


def2AST :: Graph2ASTMonad m => DefManager -> (Definition.ID, Definition) -> Pass.Result m ASTExpr.Expr
def2AST defManager (defID, def) = do 
    let 
        nextDefs :: Graph2ASTMonad m => ((Definition.ID, Definition) -> Bool) -> Pass.Result m [ASTExpr.Expr]
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
        Definition _   _     _       (Flags _ True ) _ -> return $ ASTExpr.NOP defID -- Comment
        Definition cls graph imports (Flags _ False) _ -> case cls of
            Type.Class _ _ params       -> ASTExpr.Class defID (snd $ type2ASTType cls)
                                                 <$> nextDefs classes
                                                 <*> pure (map type2Field params)
                                                 <*> nextDefs methods
            Type.Function name inputs _ -> do inputsNames <- getInputsNames inputs
                                              graphAst <- graph2AST graph inputsNames
                                              return $ ASTExpr.Function defID name 
                                                                        (function2signature cls)
                                                                        graphAst
                                               -- notImplementedList
            Type.Module   _ params      -> ASTExpr.Module defID (snd $ type2ASTType cls)
                                                      (map import2ASTimport imports)
                                                  <$> nextDefs classes
                                                  <*> pure (map type2Field params)
                                                  <*> nextDefs methods
                                                  <*> nextDefs modules
        


--module2ASTPath :: Graph2ASTMonad m => DefManager -> Definition.ID -> String -> Pass.Result m ASTExpr.Expr
--module2ASTPath defManager defID name = do
--    let 
--        module2ASTPath' :: Graph2ASTMonad m => Definition.ID -> Pass.Result m [String]
--        module2ASTPath' dID = case DefManager.prel defManager dID of
--            [(preID, Definition cls _ _ _ _)] -> do prev <- (module2ASTPath' preID)
--                                                    return $ prev ++ [Type.name cls]
--            []                                -> return []
--            _                                 -> fail "Definition has multiple parents"
--    prev <- module2ASTPath' defID
--    return $ ASTExpr.Path $ prev ++ [name]


function2signature :: Type -> [ASTPat.Pat]
function2signature funcls = map input2signature i where
    Type.Tuple i = Type.inputs funcls
    
    input2signature :: Type -> ASTPat.Pat
    input2signature input = ASTPat.Typed (-1) (ASTPat.Var (-1) name) (ASTType.Cons (-1) [cls]) where
        (name, ASTType.Var _ cls) = type2ASTType input
        

type2ASTType :: Type -> (String, ASTType.Type)
type2ASTType t = case t of 
    Type.Undefined                        -> (""  , ASTType.Unknown)
    Type.TypeName     name                -> (""  , ASTType.Var (-1) name)
    Type.Class        name params _       -> (""  , ASTType.Class (-1) name params)
    Type.Tuple        items               -> (""  , ASTType.Tuple (-1) (map (snd.type2ASTType) items))
    Type.Module       name _              -> (name, ASTType.Module (-1)  name)
    Type.Named        name cls            -> (name, snd $ type2ASTType cls)


defaultVal2ASTLit :: Int -> DefaultValue -> ASTLit.Lit
defaultVal2ASTLit i value = case value of 
    DefaultChar   v -> ASTLit.Char    i v
    DefaultInt    v -> ASTLit.Integer i v
    DefaultString v -> ASTLit.String  i v


type2Field :: Type -> ASTExpr.Expr
type2Field t = ASTExpr.Field (-1) name cls where
    (name, cls) = type2ASTType t


import2ASTimport :: Import -> ASTExpr.Expr
import2ASTimport (Import (Path path) name) = ASTExpr.Import (-1) path name


graph2AST :: Graph2ASTMonad m => Graph -> [String] -> Pass.Result m [ASTExpr.Expr]
graph2AST graph inputsNames = foldlM (node2AST graph inputsNames) [] $ Graph.topsortl graph


node2AST :: Graph2ASTMonad m => Graph -> [String] -> [ASTExpr.Expr] -> (Node.ID, Node.Node) -> Pass.Result m [ASTExpr.Expr] 
node2AST graph inputsNames list (nodeID, node) = do
    let 
        return' :: Graph2ASTMonad m => ASTExpr.Expr -> Pass.Result m [ASTExpr.Expr] 
        return' a = return (list ++ [a])

    case node of 
        Node.Expr    _ (Flags _ True) _ -> return' $ ASTExpr.NOP (-1) -- Comment
        Node.Inputs    _              _ -> return list
        Node.Outputs   (Flags _ True) _ -> return' $ ASTExpr.NOP (-1) -- Comment
        Node.Tuple     (Flags _ True) _ -> return' $ ASTExpr.NOP (-1) -- Comment
        _                                     -> do
            let 
                biggestPort :: ((a, b, Edge) -> Port) -> (a, b, Edge) -> Port -> Port
                biggestPort portCompare edge biggest = max (portCompare edge) biggest

                argID :: (a, b, Edge) -> Port
                argID (_, _, Edge _ p) = p

                arg :: [(Node.ID, b, Edge)] -> Port -> ASTExpr.Expr
                arg iedges port = case List.find (\a -> (argID a) == port) iedges of
                    Nothing                  -> ASTExpr.Wildcard (-1)
                    Just (nID, _, Edge p _)  -> ASTExpr.Var (-1) $ resultName nID p

                resultName :: Node.ID -> Port -> String
                resultName nID port = case Graph.lab graph nID of 
                    Just (Node.Outputs {}) -> "return"
                    Just (Node.Inputs {} ) -> case port of 
                                                Port.All      -> "arguments"
                                                Port.Number p -> inputsNames !! p
                    _                      -> case port of 
                                                Port.All      -> "result_" ++ show nID
                                                Port.Number p -> "result_" ++ show nID ++ "_" ++ show p


                inEdges = Graph.lprel graph nodeID

                maxArg = foldr (biggestPort argID) Port.All inEdges 

                args = case maxArg of 
                    Port.All      -> [arg inEdges maxArg]
                    Port.Number p -> map (\i -> arg inEdges $ Port.Number i) [0..p]
            

            call <- case node of 
                Node.Expr expression _ _ -> case Parser.parseExpr expression of 
                                                    Left  e    -> fail $ show e
                                                    Right expr -> return $ ASTExpr.App (-1) expr args
                Node.Default value _ -> return $ ASTExpr.Lit (-1) $ defaultVal2ASTLit nodeID value
                Node.Outputs _     _ -> return $ ASTExpr.Tuple (-1) args
                Node.Tuple   _     _ -> return $ ASTExpr.Tuple (-1) args


            let outEdges          = Graph.out graph nodeID 
                allConnected     = length outEdges
                gettersConnected = FList.count isGetter outEdges

                isGetter :: (Node.ID, Node.ID, Edge) -> Bool
                isGetter (_, _, Edge (Port.Number _) _) = True
                isGetter _                              = False
                
                allResultName  = resultName nodeID Port.All

                allResult      = ASTExpr.Var (-1) allResultName
                allPattern     = ASTPat.Var  (-1) allResultName


            if gettersConnected == 0 
                then return' $ ASTExpr.Assignment (-1) allPattern call
                else let (Port.Number maxGetter) = foldr (biggestPort resultID) Port.All outEdges 

                         resultID ::(a, b, Edge) -> Port
                         resultID (_, _, Edge p _) = p

                         res :: [(Node.ID, b, Edge)] -> Port -> ASTPat.Pat
                         res iedges port = case List.find (\a -> (resultID a) == port) iedges of
                                Nothing                  -> ASTPat.Wildcard (-1)
                                Just (nID, _, Edge p _)  -> ASTPat.Var (-1) $ resultName nID p

                         gettersPattern = ASTPat.Tuple (-1) $ map (\i -> res outEdges $ Port.Number i) [0..maxGetter] 
                     in if gettersConnected == allConnected
                            then return' $ ASTExpr.Assignment (-1) gettersPattern call
                            else return  $ list ++ [ASTExpr.Assignment (-1) allPattern call
                                                   ,ASTExpr.Assignment (-1) gettersPattern allResult]
