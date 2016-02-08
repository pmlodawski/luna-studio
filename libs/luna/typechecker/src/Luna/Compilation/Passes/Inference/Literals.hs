{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Compilation.Passes.Inference.Literals
    ( assignLiteralTypes
    ) where

import           Prelude.Luna                                 hiding (Num)

import           Control.Monad.Event                          (Dispatcher)
import           Data.Construction
import           Data.Layer.Cover
import           Data.Prop

import           Data.Record                                  hiding (Layout, cons)
import           Luna.Evaluation.Runtime                      (Dynamic, Static)
import           Luna.Syntax.AST.Term                         hiding (Draft, Expr, Lit, Source, Target, Thunk, Val, source, target)
import qualified Luna.Syntax.AST.Term                         as Term
import           Luna.Syntax.Model.Graph
import           Luna.Syntax.Model.Graph.Builder              (MonadBuilder)
import           Luna.Syntax.Model.Graph.Builder.Ref
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder            hiding (cons)
import           Luna.Syntax.Model.Network.Builder.Node.Class
import           Luna.Syntax.Model.Network.Builder.Self       (MonadSelfBuilder)
import           Luna.Syntax.Model.Network.Builder.Type       (MonadTypeBuilder)
import           Luna.Syntax.Model.Network.Class              (Network)
import           Luna.Syntax.Model.Network.Term




-- TODO: ?
-- pre :: BuilderType m a => Ref Node -> m [Ref Node]
-- pre ref = do
--     node <- Builder.readRef ref
--     mapM (Builder.follow) $ Term.inputs $ uncoat node


-- type ASTNode       = Node NetNode
-- type ASTEdge       = Link NetNode
-- type AST           = NetGraph

-- type NodeRef       = Ref ASTNode
-- type EdgeRef       = Ref ASTEdge

-- type ASTOp m = ( MonadIO m
--                , MonadFix m
--                -- , MonadError Error m
--                , Destructor m NodeRef
--                , MonadBuilder (NetLayers :< Raw) ASTEdge m
--                , MonadSelfBuilder ASTNode m
--                , MonadTypeBuilder ASTNode m
--                , ElemBuilder Star              m NodeRef
--                , ElemBuilder Blank             m NodeRef
--                , ElemBuilder (Acc Str EdgeRef) m NodeRef
--                , ElemBuilder (App     EdgeRef) m NodeRef
--                , ElemBuilder (Unify   EdgeRef) m NodeRef
--                , ElemBuilder (Var Str)         m NodeRef
--                , Connectible NodeRef NodeRef m
--                , Reader m (Node (NetLayers :< Draft Static))
--                )

-- assignLiteralTypes :: forall m b term. ( MonadIO m
--                                        , b ~ Ref (Node term)
--                                        , Monad m
--                                        , ElemBuilder Star m b
--                                        , ElemBuilder (Cons Str) m b
--                                        , Reader m (Node term)
--                                        , Covered term
--                                        , Matches (Uncovered term) '[ANY, Star])

instance Monad m => Destructor m (Layer (Network ls) (Meta meta) a) where
    destruct _ = return ()

instance Monad m => Destructor m (Layer (Network ls) Markable a) where
    destruct _ = return ()

type StaticDraft a = NetLayers a :< Draft Static
type StaticNode  a = Node (StaticDraft a)
type StaticFullDraft a = ('[Type, Succs, Markable, Meta a] :< Draft Static)

type ASTOp m a b n e c = ( MonadIO m
                       , b ~ Ref (StaticNode a)
                       , Monad m
                       , TermBuilder Star m b
                       , TermBuilder Str  m b
                       , TermBuilder Num  m b
                       , TermBuilder Cons m b
                       , Reader m (StaticNode a)
                       , Reader m (Edge (StaticFullDraft a) (StaticFullDraft a))
                       , Covered (StaticDraft a)
                       , Matches (Uncovered (StaticDraft a)) '[ANY, Star]
                       , MonadBuilder n e m
                       , Reader m (StaticNode a)
                       , Writer m (StaticNode a)
                       , Connectible b b m
                       , c ~ Connection b b
                       , Unregister m c
                       )

assignLiteralTypes :: ASTOp m a b n e c
                   => Proxy b
                   -> Ref (StaticNode a)
                   -> m ()
assignLiteralTypes proxy ref = do
    (consIntRef, consStrRef) <- createLiteralTypes proxy
    assignLiteralTypesWithTypes proxy ref consIntRef consStrRef
    cleanUpLiteralTypes         proxy     consIntRef consStrRef
    return ()

-- reconnect2 :: (Reader m src, Writer m src, Connectible (Ref src) (Ref tgt) m, e ~ Connection (Ref src) (Ref tgt), Unregister m e)
--            => Ref src -> Lens' src e -> Ref tgt -> m e
-- reconnect2 srcRef l tgtRef = do
--     src  <- read srcRef
--     unregister $ src ^. l
--     conn <- connection srcRef tgtRef
--     write srcRef $ src & l .~ conn
--     return conn


assignLiteralTypesWithTypes :: ASTOp m a b n e c
                            => Proxy b
                            -> Ref (StaticNode a)
                            -> Ref (StaticNode a)
                            -> Ref (StaticNode a)
                            -> m ()
assignLiteralTypesWithTypes proxy ref consIntRef consStrRef = do
    putStrLn $ "assignLiteralTypesWithTypes " <> show ref
    node <- read ref
    caseTest (uncover node) $ do
        match $ \(Num num) -> do
            tpeRef <- follow target $ node ^. prop Type
            reconnect ref (prop Type) consIntRef
            destruct tpeRef
            putStrLn $ "tpeRef " <> show tpeRef
            -- conn <- connection ref consIntRef
            return ()
        match $ \(Str str) -> do
            tpeRef <- follow target $ node ^. prop Type
            reconnect ref (prop Type) consStrRef
            destruct tpeRef
            -- reconnect ref (prop Type) consStrRef
            putStrLn $ "tpeRef " <> show tpeRef
            return ()
        match $ \ANY -> return ()

    -- mapM_ (assignLiteralTypesWithTypes consIntTpe consStringTpe) =<< pre ref
    return ()

createLiteralTypes :: forall m a b . ( MonadIO m
                                     , b ~ Ref (StaticNode a)
                                     , Monad m
                                     , TermBuilder Cons m b
                                     , Reader m (StaticNode a)
                                     , Covered (StaticDraft a)
                                     , Matches (Uncovered (StaticDraft a)) '[ANY, Star])
                   => Proxy b
                   -> m (Ref (StaticNode a), Ref (StaticNode a))
createLiteralTypes proxy = do
    consIntRef <- cons ("Int"    :: Str) :: m b
    consStrRef <- cons ("String" :: Str) :: m b
    return (consIntRef, consStrRef)

cleanUpLiteralTypes :: forall m a b . ( MonadIO m
                                      , b ~ Ref (StaticNode a)
                                      , Monad m
                                      , Reader m (StaticNode a)
                                      , Covered (StaticDraft a)
                                      , Matches (Uncovered (StaticDraft a)) '[ANY, Star])
                    => Proxy b
                    -> Ref (StaticNode a)
                    -> Ref (StaticNode a)
                    -> m ()
cleanUpLiteralTypes proxy consIntRef consStrRef = do
--     safeRemove consIntTpe
--     safeRemove consStringTpe
    return ()



-- removeNode :: ASTOp m => NodeRef -> m ()
-- removeNode ref = do
--     node     <- Builder.read ref
--     typeNode <- Builder.follow target $ node # Type
--     destruct typeNode
--     void $ destruct ref

-- safeRemove :: ASTOp m => NodeRef -> m ()
-- safeRemove ref = do
--     refCount <- getRefCount ref
--     if refCount > 0
--         then return ()
--         else performSafeRemoval ref

-- getRefCount :: ASTOp m => NodeRef -> m Int
-- getRefCount ref = (length . (# Succs)) <$> Builder.read ref

-- performSafeRemoval :: ASTOp m => NodeRef -> m ()
-- performSafeRemoval ref = do
--     node <- Builder.read ref
--     toRemove <- mapM (Builder.follow target) $ uncover node # Inputs
--     removeNode ref
--     mapM_ safeRemove toRemove



-- createLiteralTypes :: (Ref $ Node (NetLayers :< Draft Static))
--                    -> NetGraph
--                    -> IO ((Ref $ Node (NetLayers :< Draft Static), Ref $ Node (NetLayers :< Draft Static)), NetGraph)
-- createLiteralTypes ref g = runNetworkBuilderT g $ do
--     putStrLn $ "createLiteralTypes"
--     consIntRef    <- cons "Int"
--     consStringRef <- cons "String"
    -- return (consIntRef, consStringRef)


-- type NodeType a = ( Coated a
--                   , Uncoated a ~ (Draft (Ref Edge))
--                   , HasType a (Ref Edge)
--                   , TracksSuccs a
--                   )

-- type BuilderType m a = ( NodeType a
--                        , StarBuilder.MonadStarBuilder (Maybe (Ref Node)) m
--                        , NodeBuilder.MonadNodeBuilder (Ref Node) m
--                        , BuilderMonad (Graph a DoubleArc) m
--                        , MonadFix m
--                        , CoatConstructor m a
--                        , Destructor m (Ref Node)
--                        )

-- pre :: BuilderType m a => Ref Node -> m [Ref Node]
-- pre ref = do
--     node <- Builder.readRef ref
--     mapM (Builder.follow) $ Term.inputs $ uncoat node

-- --- TODO: Make possible to reuse (Empire.ASTOps.Remove)

-- removeNode :: BuilderType m a => Ref Node -> m ()
-- removeNode ref = do
--     node     <- Builder.readRef ref
--     typeNode <- Builder.follow $ node ^. Typed.tp
--     destruct typeNode
--     destruct ref

-- safeRemove :: BuilderType m a => Ref Node -> m ()
-- safeRemove ref = do
--     refCount <- getRefCount ref
--     when (refCount == 0) $ performSafeRemoval ref

-- getRefCount :: BuilderType m a => Ref Node -> m Int
-- getRefCount ref = (length . toList . view Graph.succs) <$> Builder.readRef ref

-- performSafeRemoval :: BuilderType m a => Ref Node -> m ()
-- performSafeRemoval ref = do
--     node     <- Builder.readRef ref
--     toRemove <- mapM Builder.follow (Term.inputs $ uncoat node)
--     removeNode ref
--     mapM_ safeRemove toRemove

-- ---

-- assignLiteralTypes :: BuilderType m a => Ref Node -> m ()
-- assignLiteralTypes ref = do
--     consIntTpe    <- createConsInt
--     consStringTpe <- createConsString
--     assignLiteralTypesWithTypes consIntTpe consStringTpe ref
--     safeRemove consIntTpe
--     safeRemove consStringTpe

-- assignLiteralType :: BuilderType m a => Ref Node -> Ref Node -> m ()
-- assignLiteralType ref tpe = do
--     node     <- Builder.readRef ref
--     tnodeRef <- Builder.follow $ node ^. Typed.tp
--     tnode    <- Builder.readRef tnodeRef
--     case' (uncoat tnode) $ do
--         match $ \Star -> do
--             destruct tnodeRef
--             void $ Builder.reconnect ref Typed.tp tpe
--         match $ \ANY -> return ()

-- assignLiteralType :: BuilderType m a => Ref Node -> Ref Node -> m ()
-- assignLiteralType ref tpe = do
--     node     <- Builder.readRef ref
--     tnodeRef <- Builder.follow $ node ^. Typed.tp
--     tnode    <- Builder.readRef tnodeRef
--     case' (uncoat tnode) $ do
--         match $ \Star -> do
--             destruct tnodeRef
--             void $ Builder.reconnect ref Typed.tp tpe
--         match $ \ANY -> return ()

-- assignLiteralTypesWithTypes :: BuilderType m a => Ref Node -> Ref Node -> Ref Node -> m ()
-- assignLiteralTypesWithTypes consIntTpe consStringTpe ref = do
--     node <- Builder.readRef ref
--     case' (uncoat node) $ do
--         match $ \(Val val) -> do
--             case' val $ match $ \lit -> assignLiteralType ref $ case lit of
--                 Lit.Int    _ -> consIntTpe
--                 Lit.String _ -> consStringTpe
--         match $ \ANY -> return ()
--     mapM_ (assignLiteralTypesWithTypes consIntTpe consStringTpe) =<< pre ref

