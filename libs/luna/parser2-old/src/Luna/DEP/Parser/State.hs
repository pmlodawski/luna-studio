---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}

module Luna.DEP.Parser.State where

import qualified Data.List                   as List
import qualified Data.Maps                   as Map
import           Flowbox.Control.Monad.State (get, mapStateVal, put)
import           Flowbox.Prelude
import qualified Luna.DEP.AST.AST            as AST
import           Luna.DEP.AST.Comment        (Comment (..))
import qualified Luna.DEP.AST.Common         as AST
import           Luna.DEP.AST.IDMap          (IDMap)
import qualified Luna.DEP.AST.IDMap          as IDMap
import qualified Luna.DEP.Data.AliasInfo     as Alias
import           Luna.DEP.Data.ASTInfo       (ASTInfo)
import qualified Luna.DEP.Data.ASTInfo       as ASTInfo
import           Luna.DEP.Data.Config        (Config)
import qualified Luna.DEP.Data.Config        as Config
import           Luna.DEP.Data.Namespace     (Namespace)
import qualified Luna.DEP.Data.Namespace     as Namespace
import           Luna.DEP.Data.SourceMap     (SourceMap)
import qualified Luna.DEP.Data.SourceMap     as SourceMap
import           Luna.DEP.Parser.Operator    (OperatorMap)



data State a = State { _conf          :: Config a
                     , _info          :: ASTInfo
                     , _opFixity      :: OperatorMap
                     , _sourceMap     :: SourceMap
                     , _namespace     :: Namespace
                     , _adhocReserved :: [String]
                     , _comments      :: IDMap [Comment]
                     } deriving (Show)

makeLenses ''State


------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------

mk :: ASTInfo -> State ()
mk i = def & info .~ i

addReserved words = adhocReserved %~ (++words)
delReserved words = adhocReserved %~ (flip (foldl (flip List.delete)) words)

lastID            = view (info . ASTInfo.lastID)
addComment cmt s  = s & comments %~ Map.insertWith (++) (lastID s) [cmt]

registerComment = mapStateVal . addComment . Comment

regParent id pid = mapStateVal $ namespace %~ Namespace.regParent id pid

registerAST id ast = mapStateVal $ namespace %~ Namespace.regAST id ast

pushScope id = mapStateVal $ namespace %~ Namespace.pushScope id
popScope     = mapStateVal $ namespace %~ Namespace.popScope

regVarName id name = do
    pid <- getPid
    withAlias $ Alias.regVarName pid id name

regTypeName id name = do
    pid <- getPid
    withAlias $ Alias.regTypeName pid id name

withAlias f = mapStateVal (namespace . Namespace.info %~ f)

withReserved words p = do
    s <- get
    let reserved = view adhocReserved s
    put $ (addReserved words s)
    ret <- p
    s   <- get
    put (s & adhocReserved .~ reserved)
    return ret


withScope id p = do
    pushScope id
    ret <- p
    popScope
    return ret


getPid = do
    mpid <- Namespace.head . view namespace <$> get
    case mpid of
        Nothing  -> fail "Internal parser error. Cannot optain pid."
        Just pid -> return pid

getScope  = view (namespace . Namespace.info . Alias.scope) <$> get
getASTMap = view (namespace . Namespace.info . Alias.ast) <$> get



registerID id = do
    pid <- getPid
    regParent id pid

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance a~() => Default (State a) where
        def = State def def def def def def def



