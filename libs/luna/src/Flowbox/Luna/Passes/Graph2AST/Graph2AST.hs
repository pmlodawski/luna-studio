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

import           Flowbox.Prelude                         hiding (id)
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
        getInputsNames (Type.Tuple items) = mapM ((liftM fst) . type2ASTType) items
        getInputsNames _                  = fail "Inputs is not a tuple"

    case def of 
        Definition _   _     _       (Flags _ True ) _ -> return $ ASTExpr.NOP defID -- Comment
        Definition cls graph imports (Flags _ False) _ -> case cls of
            Type.Class _ _ params       -> ASTExpr.Class defID 
                                                     <$> (liftM snd $ type2ASTType cls)
                                                     <*> nextDefs classes
                                                     <*> (mapM type2Field params)
                                                     <*> nextDefs methods
            Type.Function name inputs _ -> do inputsNames <- getInputsNames inputs
                                              graphAst    <- graph2AST graph inputsNames
                                              signature   <- function2signature cls
                                              return $ ASTExpr.Function defID name 
                                                                        signature
                                                                        graphAst
                                               -- notImplementedList
            Type.Module   _ params      -> ASTExpr.Module defID 
                                                  <$> (liftM snd $ type2ASTType cls)
                                                  <*> mapM import2ASTimport imports
                                                  <*> nextDefs classes
                                                  <*> mapM type2Field params
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


function2signature :: Graph2ASTMonad m => Type -> Pass.Result m [ASTPat.Pat]
function2signature funcls = mapM input2signature i where
    Type.Tuple i = Type.inputs funcls
    
    input2signature :: Graph2ASTMonad m => Type -> Pass.Result m ASTPat.Pat
    input2signature input = do 
        (name, ASTType.Var _ cls) <- type2ASTType input
        inpID  <- IdState.newID
        varID  <- IdState.newID
        consID <- IdState.newID
        return $ ASTPat.Typed inpID (ASTPat.Var varID name) (ASTType.Cons consID [cls]) 
        

type2ASTType :: Graph2ASTMonad m => Type -> Pass.Result m (String, ASTType.Type)
type2ASTType t = do 
    id <- IdState.newID
    case t of 
        Type.Undefined                  -> return (""  , ASTType.Unknown)
        Type.TypeName     name          -> return (""  , ASTType.Var    id name)
        Type.Class        name params _ -> return (""  , ASTType.Class  id name params)
        Type.Tuple        items         -> do astitems <- mapM (liftM snd .type2ASTType) items
                                              return (""  , ASTType.Tuple  id astitems)
        Type.Module       name _        -> return (name, ASTType.Module id name)
        Type.Named        name cls      -> do type_ <- liftM snd $ type2ASTType cls
                                              return (name, type_)


defaultVal2ASTLit :: Graph2ASTMonad m => DefaultValue -> Pass.Result m ASTLit.Lit
defaultVal2ASTLit value = do 
    id <- IdState.newID
    return $ case value of 
        DefaultChar   v -> ASTLit.Char    id v
        DefaultInt    v -> ASTLit.Integer id v
        DefaultString v -> ASTLit.String  id v


type2Field :: Graph2ASTMonad m => Type -> Pass.Result m ASTExpr.Expr
type2Field t = do
    id <- IdState.newID
    (name, cls) <- type2ASTType t
    return $ ASTExpr.Field id name cls


import2ASTimport :: Graph2ASTMonad m => Import -> Pass.Result m ASTExpr.Expr
import2ASTimport (Import (Path path) name) = do 
    id <- IdState.newID
    return $ ASTExpr.Import id path name


graph2AST :: Graph2ASTMonad m => Graph -> [String] -> Pass.Result m [ASTExpr.Expr]
graph2AST graph inputsNames = foldlM (node2AST graph inputsNames) [] $ Graph.topsortl graph


node2AST :: Graph2ASTMonad m => Graph -> [String] -> [ASTExpr.Expr] -> (Node.ID, Node.Node) -> Pass.Result m [ASTExpr.Expr] 
node2AST graph inputsNames list (nodeID, node) = do

    astNodeID <- IdState.newID

    (++) list <$> case node of 
        Node.Expr    _ (Flags _ True) _ -> return [ASTExpr.NOP astNodeID] -- Comment
        Node.Inputs    _              _ -> return []
        Node.Outputs   (Flags _ True) _ -> return [ASTExpr.NOP astNodeID] -- Comment
        Node.Tuple     (Flags _ True) _ -> return [ASTExpr.NOP astNodeID] -- Comment
        _                               -> do
            let 
                biggestPort :: ((a, b, Edge) -> Port) -> (a, b, Edge) -> Port -> Port
                biggestPort portCompare edge biggest = max (portCompare edge) biggest

                argID :: (a, b, Edge) -> Port
                argID (_, _, Edge _ p) = p

                arg :: Graph2ASTMonad m => [(Node.ID, b, Edge)] -> Port -> Pass.Result m ASTExpr.Expr
                arg iedges port = case List.find (\a -> (argID a) == port) iedges of
                    Nothing                  -> ASTExpr.Wildcard <$> IdState.newID
                    Just (nID, _, Edge p _)  -> ASTExpr.Var <$> IdState.newID 
                                                            <*> (pure $ resultName nID p)

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

            args <- case maxArg of 
                Port.All      -> do onlyArg <- arg inEdges maxArg 
                                    return [onlyArg]
                Port.Number p -> mapM (\i -> arg inEdges $ Port.Number i) [0..p]
            
            parseID <- IdState.newID

            call <- case node of 
                Node.Expr expression _ _ -> case Parser.parseExpr expression parseID of 
                                                    Left  e    -> fail $ show e
                                                    Right expr -> return $ ASTExpr.App astNodeID expr args
                Node.Default value _ -> ASTExpr.Lit astNodeID <$> defaultVal2ASTLit value
                Node.Inputs  _     _ -> fail "Graph2AST Implementation error: Node.Inputs should be already handled"
                Node.Outputs _     _ -> return $ ASTExpr.Tuple astNodeID args
                Node.Tuple   _     _ -> return $ ASTExpr.Tuple astNodeID args


            let outEdges          = Graph.out graph nodeID 
                numConnected     = length outEdges
                numGetters = FList.count isGetter outEdges

                isGetter :: (Node.ID, Node.ID, Edge) -> Bool
                isGetter (_, _, Edge (Port.Number _) _) = True
                isGetter _                              = False
                
                allResultName  = resultName nodeID Port.All

            allResult  <- ASTExpr.Var <$> IdState.newID <*> pure allResultName
            allPattern <- ASTPat.Var  <$> IdState.newID <*> pure allResultName


            if numGetters == 0 
                then do id <- IdState.newID 
                        return [ASTExpr.Assignment id allPattern call]
                else do let (Port.Number maxGetter) = foldr (biggestPort resultID) Port.All outEdges 

                            resultID ::(a, b, Edge) -> Port
                            resultID (_, _, Edge p _) = p

                            res :: Graph2ASTMonad m => [(Node.ID, b, Edge)] -> Port -> Pass.Result m ASTPat.Pat
                            res iedges port = case List.find (\a -> (resultID a) == port) iedges of
                                Nothing                  -> ASTPat.Wildcard <$> IdState.newID 
                                Just (nID, _, Edge p _)  -> ASTPat.Var <$> IdState.newID 
                                                                       <*> (pure $ resultName nID p)

                        gettersPattern <- ASTPat.Tuple <$> IdState.newID 
                                                       <*> mapM (\i -> res outEdges $ Port.Number i) [0..maxGetter]
                        if numGetters == numConnected
                            then do id <- IdState.newID 
                                    return [ASTExpr.Assignment id gettersPattern call]
                            else do id1 <- IdState.newID 
                                    id2 <- IdState.newID 
                                    return [ASTExpr.Assignment id1 allPattern call
                                           ,ASTExpr.Assignment id2 gettersPattern allResult]
