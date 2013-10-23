---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds, TupleSections #-}

module Flowbox.Luna.Passes.Transform.AST.GraphParser.GraphParser (
    run
) where

import           Control.Applicative                                     
import           Control.Monad.State                                   hiding (mapM, mapM_)
import           Data.Foldable                                           (foldlM)
import qualified Data.List                                             as List
import qualified Data.List.Split                                       as Split

import           Flowbox.Prelude                                       hiding (id)
import           Flowbox.Control.Error                                   ()
import qualified Flowbox.Data.List                                     as FList
import qualified Flowbox.Luna.Data.AST.Expr                            as ASTExpr
import qualified Flowbox.Luna.Data.AST.Module                          as ASTModule
import qualified Flowbox.Luna.Data.AST.Pat                             as ASTPat
import qualified Flowbox.Luna.Data.AST.Type                            as ASTType
import qualified Flowbox.Luna.Network.Def.DefManager                   as DefManager
import           Flowbox.Luna.Network.Def.DefManager                     (DefManager)
import qualified Flowbox.Luna.Network.Def.Definition                   as Definition
import           Flowbox.Luna.Network.Def.Definition                     (Definition(Definition))
import           Flowbox.Luna.Network.Flags                              (Flags(Flags))
import qualified Flowbox.Luna.Network.Graph.Value                      as Value
import           Flowbox.Luna.Network.Graph.Value                        (Value)
import qualified Flowbox.Luna.Network.Graph.Graph                      as Graph
import           Flowbox.Luna.Network.Graph.Graph                        (Graph)
import           Flowbox.Luna.Network.Graph.Edge                         (Edge(Edge))
import qualified Flowbox.Luna.Network.Graph.Node                       as Node
import qualified Flowbox.Luna.Network.Graph.Port                       as Port
import           Flowbox.Luna.Network.Graph.Port                         (Port)
import           Flowbox.Luna.Network.Path.Import                        (Import(Import))
import           Flowbox.Luna.Network.Path.Path                          (Path(Path))
import qualified Flowbox.Luna.Passes.Pass                              as Pass
import           Flowbox.Luna.Passes.Pass                                (PassMonad)
import qualified Flowbox.Luna.Passes.Transform.AST.GraphParser.IdState as IdState
import           Flowbox.Luna.Passes.Transform.AST.GraphParser.IdState   (IdState)
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Parser    as Parser
import qualified Flowbox.Luna.XOLD.Type.Type                           as Type
import           Flowbox.Luna.XOLD.Type.Type                             (Type)

-- TODO [PM] : refactor needed


type ASTExpr    = ASTExpr.Expr
type ASTPat     = ASTPat.Pat
type ASTModule  = ASTModule.Module
type ASTType    = ASTType.Type


type Graph2ASTMonad m = PassMonad IdState m


newID :: IdState.IdStateM m => m Int
newID = IdState.newID


tok :: (Functor f, IdState.IdStateM f) => (Int -> b) -> f b

tok a = a <$> newID

run :: PassMonad s m => DefManager -> (Definition.ID, Definition) -> Pass.Result m ASTModule
run defManager (defID, def) = (Pass.run_ (Pass.Info "GraphParser") IdState.empty) $ module2AST defManager (defID, def)


_NOT_IMPLEMENTED :: (Alternative m) => m a -- TODO [PM] : remove
_NOT_IMPLEMENTED = empty


module2AST :: Graph2ASTMonad m => DefManager -> (Definition.ID, Definition) -> Pass.Result m ASTModule
module2AST defManager (defID, Definition cls _ imports _ _) = case cls of 
    Type.Module _ params -> ASTModule.Module defID 
                                         <$> (liftM snd $ type2ASTType cls)
                                         <*> mapM import2ASTimport imports
                                         <*> nextDefs defManager defID classesFilter
                                         <*> mapM type2Field params
                                         <*> nextDefs defManager defID methodsFilter
                                         <*> nextModules defManager defID
    _ -> fail "Module type is not the root in definition tree"


def2AST :: Graph2ASTMonad m => DefManager -> (Definition.ID, Definition) -> Pass.Result m ASTExpr
def2AST defManager (defID, def) = case def of 
    Definition _   _     _ (Flags _ True ) _ -> pure $ ASTExpr.NOP defID -- Comment
    Definition cls graph _ (Flags _ False) _ -> case cls of
        Type.Class _ _ params             -> ASTExpr.Class defID 
                                                 <$> (liftM snd $ type2ASTType cls)
                                                 <*> nextDefs defManager defID classesFilter
                                                 <*> (mapM type2Field params)
                                                 <*> nextDefs defManager defID methodsFilter
        Type.Function name inputs outputs -> do inputsNames <- getInputsNames inputs
                                                graphAst    <- graph2AST graph inputsNames
                                                signature   <- function2signature cls
                                                outputsType <- (liftM snd $ type2ASTType outputs)
                                                outputsTypeMock <- tok ASTType.Unknown
                                                pure $ ASTExpr.Function defID _NOT_IMPLEMENTED name 
                                                                          signature
                                                                          outputsTypeMock
                                                                          graphAst
                                           -- _NOT_IMPLEMENTED
        Type.Undefined   -> fail "Undefined type in definition tree."
        Type.TypeName {} -> fail "TypeName type in definition tree."
        Type.Tuple    {} -> fail "Tuple type in definition tree."
        Type.Named    {} -> fail "Named type in definition tree."
        Type.Module   {} -> fail "Unexpected Module in definition tree."
       

nextModules :: Graph2ASTMonad m => DefManager -> Definition.ID -> Pass.Result m [ASTModule]
nextModules defManager defID = (mapM (module2AST defManager) $ filter modulesFilter $ DefManager.sucl defManager defID)
 

nextDefs :: Graph2ASTMonad m => DefManager -> Definition.ID -> ((Definition.ID, Definition) -> Bool) -> Pass.Result m [ASTExpr]
nextDefs defManager defID f = (mapM (def2AST defManager) $ filter f $ DefManager.sucl defManager defID)


classesFilter :: (Definition.ID, Definition) -> Bool
classesFilter (_, Definition (Type.Class {}) _ _ _ _) = True
classesFilter (_, Definition {}                     ) = False


methodsFilter :: (Definition.ID, Definition) -> Bool
methodsFilter (_, Definition (Type.Function {}) _ _ _ _) = True
methodsFilter (_, Definition {}                        ) = False


modulesFilter :: (Definition.ID, Definition) -> Bool
modulesFilter (_, Definition (Type.Module {}) _ _ _ _) = True
modulesFilter (_, Definition {}                      ) = False


getInputsNames :: Graph2ASTMonad m => Type -> Pass.Result m [String]
getInputsNames (Type.Tuple items) = mapM ((liftM fst) . type2ASTType) items
getInputsNames _                  = fail "Inputs is not a tuple"


--module2ASTPath :: Graph2ASTMonad m => DefManager -> Definition.ID -> String -> Pass.Result m ASTExpr
--module2ASTPath defManager defID name = do
--    let 
--        module2ASTPath' :: Graph2ASTMonad m => Definition.ID -> Pass.Result m [String]
--        module2ASTPath' dID = case DefManager.prel defManager dID of
--            [(preID, Definition cls _ _ _ _)] -> do prev <- (module2ASTPath' preID)
--                                                    pure $ prev ++ [Type.name cls]
--            []                                -> pure []
--            _                                 -> fail "Definition has multiple parents"
--    prev <- module2ASTPath' defID
--    pure $ ASTExpr.Path $ prev ++ [name]


function2signature :: Graph2ASTMonad m => Type -> Pass.Result m [ASTExpr]
function2signature funcls = mapM input2signature i where
    Type.Tuple i = Type.inputs funcls
    
    input2signature :: Graph2ASTMonad m => Type -> Pass.Result m ASTExpr
    input2signature input = do 
        (name, ASTType.Var _ cls) <- type2ASTType input
        pat <- tok ASTPat.Typed <*> (tok ASTPat.Var  <*> pure name) 
                                <*> (tok ASTType.Con <*> pure [cls]) 
        tok ASTExpr.Arg <*> pure pat <*> pure Nothing
        

type2ASTType :: Graph2ASTMonad m => Type -> Pass.Result m (String, ASTType)
type2ASTType t = do 
    id <- newID
    case t of 
        Type.Undefined                  -> pure (""  , ASTType.Unknown id)
        Type.Module       name _        -> pure (""  , ASTType.Module id (Split.splitOn "." name))
        Type.TypeName     name          -> pure (""  , ASTType.Var    id name)
        Type.Class        name params _ -> pure (""  , ASTType.Class  id name params)
        Type.Tuple        items         -> do astitems <- mapM (liftM snd .type2ASTType) items
                                              pure (""  , ASTType.Tuple  id astitems)
        Type.Named        name cls      -> do type_ <- liftM snd $ type2ASTType cls
                                              pure (name, type_)



defaultVal2ASTExpr :: Graph2ASTMonad m => Value -> Pass.Result m ASTExpr
defaultVal2ASTExpr defaultvalue = do
    let value = Value.value defaultvalue
    parseExpr value


type2Field :: Graph2ASTMonad m => Type -> Pass.Result m ASTExpr
type2Field t = do
    id <- newID
    (name, cls) <- type2ASTType t
    pure $ ASTExpr.Field id name cls Nothing


import2ASTimport :: Graph2ASTMonad m => Import -> Pass.Result m ASTExpr
import2ASTimport (Import (Path path) name) =  
    tok ASTExpr.Import <*> pure path
                       <*> (tok ASTExpr.Con <*> pure name) 
                       <*> pure Nothing


graph2AST :: Graph2ASTMonad m => Graph -> [String] -> Pass.Result m [ASTExpr]
graph2AST graph inputsNames = foldlM (node2AST graph inputsNames) [] $ Graph.topsortl graph


-- TODO [PM] : refactor needed
node2AST :: Graph2ASTMonad m => Graph -> [String] -> [ASTExpr] -> (Node.ID, Node.Node) -> Pass.Result m [ASTExpr] 
node2AST graph inputsNames list (nodeID, node) = do
    astNodeID <- newID
    (++) list <$> case node of 
        Node.Expr    _ (Flags _ True) _ -> pure [ASTExpr.NOP astNodeID] -- Comment
        Node.Inputs    _              _ -> pure []
        Node.Outputs   (Flags _ True) _ -> pure [ASTExpr.NOP astNodeID] -- Comment
        Node.Tuple     (Flags _ True) _ -> pure [ASTExpr.NOP astNodeID] -- Comment
        _                               -> do
            let 
                biggestPort :: ((a, b, Edge) -> Port) -> (a, b, Edge) -> Port -> Port
                biggestPort portCompare edge biggest = max (portCompare edge) biggest

                argID :: (a, b, Edge) -> Port
                argID (_, _, Edge _ p) = p

                arg :: Graph2ASTMonad m => [(Node.ID, b, Edge)] -> Port -> Pass.Result m ASTExpr
                arg iedges port = case List.find (\a -> (argID a) == port) iedges of
                    Nothing                  -> tok ASTExpr.Wildcard
                    Just (nID, _, Edge p _)  -> tok ASTExpr.Var <*> pure (resultName nID p)

                resultName :: Node.ID -> Port -> String
                resultName nID port = case Graph.lab graph nID of 
                    Just (Node.Outputs {}) -> "pure"
                    Just (Node.Inputs {} ) -> case port of 
                                                Port.All      -> "arguments"
                                                Port.Number p -> inputsNames !! p
                    _                      -> case port of 
                                                Port.All      -> "result_" ++ show nID
                                                Port.Number p -> "result_" ++ show nID ++ "_" ++ show p

                inEdges = Graph.lprel graph nodeID
                maxArg  = foldr (biggestPort argID) Port.All inEdges 

            args <- case length inEdges of 
                0 -> pure []
                _ -> case maxArg of 
                    Port.All      -> (:[]) <$> arg inEdges maxArg 
                    Port.Number p -> mapM (\i -> arg inEdges $ Port.Number i) [0..p]
            
            call <- case node of 
                Node.Expr expression _ _ -> case length args of
                                            0 -> tok ASTExpr.App <*> parseExpr expression <*> pure args
                                            _ -> do let callOn   = head args 
                                                        callArgs = tail args
                                                    accesor <- tok ASTExpr.Accessor <*> pure expression <*> pure callOn
                                                    tok ASTExpr.App <*> pure accesor <*> pure callArgs
                Node.Default value _ -> defaultVal2ASTExpr value
                Node.Inputs  _     _ -> fail "Graph2AST Implementation error: Node.Inputs should be already handled"
                Node.Outputs _     _ -> if length args == 1
                                            then pure $ head args
                                            else tok ASTExpr.Tuple <*> pure args
                Node.Tuple   _     _ -> tok ASTExpr.Tuple <*> pure args

            let outEdges     = Graph.out graph nodeID 
                numConnected = length outEdges
                numGetters   = FList.count isGetter outEdges

                isGetter :: (Node.ID, Node.ID, Edge) -> Bool
                isGetter (_, _, Edge (Port.Number _) _) = True
                isGetter _                              = False
                
                allResultName = resultName nodeID Port.All

            allResult  <- tok ASTExpr.Var <*> pure allResultName
            allPattern <- tok ASTPat.Var  <*> pure allResultName

            if numGetters == 0 
                then if numConnected == 0
                    then pure [call]
                    else do id <- newID 
                            pure [ASTExpr.Assignment id allPattern call]
                else do let (Port.Number maxGetter) = foldr (biggestPort resultID) Port.All outEdges 

                            resultID ::(a, b, Edge) -> Port
                            resultID (_, _, Edge p _) = p

                            res :: Graph2ASTMonad m => [(Node.ID, b, Edge)] -> Port -> Pass.Result m ASTPat
                            res iedges port = case List.find (\a -> (resultID a) == port) iedges of
                                Nothing                  -> tok ASTPat.Wildcard
                                Just (nID, _, Edge p _)  -> tok ASTPat.Var <*> (pure $ resultName nID p)

                        results <- mapM (\i -> res outEdges $ Port.Number i) [0..maxGetter]
                        gettersPattern <- if length results == 1
                                                then pure $ head results
                                                else tok ASTPat.Tuple <*> pure results
                        if numGetters == numConnected
                            then do id  <- newID 
                                    pure [ASTExpr.Assignment id gettersPattern call]
                            else do id1 <- newID 
                                    id2 <- newID 
                                    pure [ASTExpr.Assignment id1 allPattern call
                                         ,ASTExpr.Assignment id2 gettersPattern allResult]

parseExpr :: Graph2ASTMonad m => String -> Pass.Result m ASTExpr
parseExpr expression = do 
    parseID <- newID
    case Parser.parseExpr expression (parseID*100000) of -- TODO [PM] : fixme when COMPILER-4 is done
        Left  e             -> fail $ show e
        Right (expr, outID) -> pure expr
