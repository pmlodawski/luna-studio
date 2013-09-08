---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Flowbox.Batch.Tools.Definition2AST where
-- TODO [PM] move to Luna project!

import           Control.Applicative                       
import           Data.Foldable                             (foldrM)
import           Text.Parsec.Error                         

import           Flowbox.Control.Error                     
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
import qualified Flowbox.Luna.AST.AST                    as AST
import qualified Flowbox.Luna.AST.Type                   as ASTType
import qualified Flowbox.Luna.AST.Constant               as ASTConstant
import qualified Flowbox.Luna.Passes.Txt2AST.Parser      as Parser
import qualified Flowbox.Luna.XOLD.Type.Type             as Type
import           Flowbox.Luna.XOLD.Type.Type               (Type)
import qualified Flowbox.System.UniPath                  as UniPath
import           Flowbox.System.UniPath                    (UniPath)



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


def2AST :: DefManager -> (Definition.ID, Definition) -> Either String AST.Expr
def2AST defManager (defID, def) = do 
    let (Definition acls agraph aimports (Flags _ aomit) _) = def
    case acls of
        Type.Class _ _ params             -> do let astCls    = (snd . toASTType) acls
                                                    astFields = map type2Field params
                                                astMethods <- mapM (def2AST defManager) $ DefManager.sucl defManager defID
                                                return $ AST.Class astCls astFields astMethods
        Type.Function name inputs outputs -> AST.Function <$> pure name 
                                                          <*> pure ASTType.Unknown 
                                                          <*> graph2AST agraph
        _                          -> return AST.NOP



graph2AST :: Graph -> Either String [AST.Expr]
graph2AST agraph = foldrM (node2AST agraph) [] $ Graph.topsortl agraph


node2AST :: Graph -> (Node.ID, Node.Node) -> [AST.Expr] -> Either String [AST.Expr] 
node2AST agraph (nodeID, node) list = do

    call <- case node of 
        Node.Expr expression flags _ -> case Parser.parseExpr expression of 
                                            Left  err  -> Left $ show err
                                            Right expr -> Right expr
        Node.Default value _ -> return AST.NOP
        Node.Inputs  flags _ -> return AST.NOP
        Node.Outputs flags _ -> return AST.NOP
        Node.Tuple   flags _ -> return AST.NOP
        Node.NTuple  flags _ -> return AST.NOP

    let assignments = [AST.NOP]

    return $ list ++ assignments ++ [call]