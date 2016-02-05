{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Passes.Inference.Literals
    ( assignLiteralTypes
    ) where

import           Prelude.Luna                           hiding (Num)

import           Data.Construction
import           Data.Layer.Cover
import           Data.Prop

import           Data.Record                            hiding (Layout, cons)
import           Luna.Runtime.Model                     (Dynamic, Static)
import           Luna.Syntax.AST.Term                   hiding (Draft, Expr, Lit, Source, Target, Thunk, Val, source, target)
import qualified Luna.Syntax.AST.Term                   as Term
import           Luna.Syntax.Model.Graph
import           Luna.Syntax.Model.Graph.Builder        (MonadBuilder)
import           Luna.Syntax.Model.Graph.Builder.Ref
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder
import           Luna.Syntax.Model.Network.Builder.Self (MonadSelfBuilder)
import           Luna.Syntax.Model.Network.Builder.Type (MonadTypeBuilder)
import           Luna.Syntax.Model.Network.Term



-- TODO: ?
-- pre :: BuilderType m a => Ref Node -> m [Ref Node]
-- pre ref = do
--     node <- Builder.readRef ref
--     mapM (Builder.follow) $ Term.inputs $ uncoat node



-- bar :: forall m b. (Monad m, ElemBuilder Star m b, ElemBuilder Num m b) => m b
-- bar = do
--     s1 <- star  :: m (Ref (Node (NetLayers :< Draft Static)))
--     s2 <- int 4 :: m (Ref (Node (NetLayers :< Draft Static)))
--     node <- read s1  :: m (Node (NetLayers :< Draft Static))
--     return s1

-- bar :: forall m b term. (b ~ Ref term, Monad m, ElemBuilder Star m b, Reader m term) => m b
-- foo :: forall m b. (b ~ Ref (Node (NetLayers :< Draft Static)), Monad m, ElemBuilder Star m b, Reader m (Node (NetLayers :< Draft Static))) => m b
-- bar = do
--     bar :: m b
--     s1 <- star :: m b
--     s2 <- star :: m b
--     x <- read s1
--     return s1




-- assignLiteralTypes :: forall m b term. ( MonadIO m,
--     b ~ Ref (Node term), Monad m, ElemBuilder Star m b, Reader m (Node term), Covered term, Matches (Uncovered term) '[ANY, Star]) =>
--     m b
-- assignLiteralTypes = do
--     assignLiteralTypes :: m b
--     s1 <- star :: m b
--     return s1


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

-- b ~ Ref (Node (NetLayers :< Draft Static)
-- b ~ Ref (Node term)

-- assignLiteralTypes :: ASTOp m => m ()

-- assignLiteralTypes :: forall m b term. ( MonadIO m
--                                        , b ~ Ref (Node term)
--                                        , Monad m
--                                        , ElemBuilder Star m b
--                                        , ElemBuilder (Cons Str) m b
--                                        , Reader m (Node term)
--                                        , Covered term
--                                        , Matches (Uncovered term) '[ANY, Star])



type StaticDraft a = NetLayers a :< Draft Static
type StaticNode  a = Node (StaticDraft a)

type ASTOp m a b = ( MonadIO m
                   , b ~ Ref (StaticNode a)
                   , Monad m
                   , ElemBuilder Star       m b
                   , ElemBuilder Str        m b
                   , ElemBuilder Num        m b
                   , ElemBuilder (Cons Str) m b
                   , Reader m (StaticNode a)
                   , Covered (StaticDraft a)
                   , Matches (Uncovered (StaticDraft a)) '[ANY, Star]
                   )

assignLiteralTypes :: ASTOp m a b
                   => Proxy b
                   -> Ref (StaticNode a)
                   -> m ()
assignLiteralTypes proxy ref = do
    (consIntRef, consStrRef) <- createLiteralTypes proxy
    assignLiteralTypesWithTypes proxy ref consIntRef consStrRef
    cleanUpLiteralTypes         proxy     consIntRef consStrRef
    return ()

-- assignLiteralTypes :: forall m b term. (MonadIO m,
--     b ~ (), Monad m, ElemBuilder Star m b, Reader m (Node term), Covered term, Matches (Uncovered term) '[ANY, Star]) =>
--     (Ref $ Node (NetLayers :< Draft Static)) -> m b
-- assignLiteralTypes ref g = do
--     return ()

-- assignLiteralTypes :: (Ref $ Node (NetLayers :< Draft Static))
--                    -> NetGraph
--                    -> IO ((), NetGraph)
-- assignLiteralTypes ref g = do
--     ((consIntRef, consStringRef), g'  ) <- createLiteralTypes          ref                          g
--     ((),                          g'' ) <- assignLiteralTypesWithTypes ref consIntRef consStringRef g'
--     ((),                          g''') <- cleanUpLiteralTypes             consIntRef consStringRef g''
--     return ((), g''')


assignLiteralTypesWithTypes :: ASTOp m a b
                            => Proxy b
                            -> Ref (StaticNode a)
                            -> Ref (StaticNode a)
                            -> Ref (StaticNode a)
                            -> m ()
assignLiteralTypesWithTypes proxy ref consIntRef consStrRef = do
    putStrLn $ "assignLiteralTypesWithTypes " <> show ref
    node <- read ref
    caseTest (uncover node) $ do
        match $ \(Str str) -> do
            let tpeRef = node ^. prop Type
            putStrLn $ "tpeRef " <> show tpeRef
            -- reconnect ref (prop Type) consStrRef
            return ()
        match $ \(Num num) -> do
            let tpeRef = node ^. prop Type
            putStrLn $ "tpeRef " <> show tpeRef
            -- destruct tpeRef -- TODO: ?
            -- reconnect ref (prop Type) consIntRef
            return ()
        match $ \ANY -> return ()

    -- mapM_ (assignLiteralTypesWithTypes consIntTpe consStringTpe) =<< pre ref  -- TODO: ?
    return ()

createLiteralTypes :: forall m a b . ( MonadIO m
                                     , b ~ Ref (StaticNode a)
                                     , Monad m
                                     , ElemBuilder (Cons Str) m b
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

