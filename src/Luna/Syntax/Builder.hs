{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Luna.Syntax.Builder ( module Luna.Syntax.Builder, module X) where

import Prologue hiding (index)

import Data.Variants   as     V
import Control.Monad.Fix

import           Luna.Syntax.Builder.Star (StarBuilder, StarBuilderT, MonadStarBuilder)
import qualified Luna.Syntax.Builder.Star as StarBuilder
import           Luna.Syntax.Repr.Graph
import           Luna.Syntax.Name
import           Luna.Syntax.AST
import           Luna.Syntax.AST.Arg
import           Luna.Syntax.AST.Term
import           Luna.Syntax.AST.Lit
import           Luna.Syntax.AST.Decl
import           Luna.Syntax.AST.Term
import           Luna.Syntax.AST.Typed

import qualified Luna.Syntax.Builder.Class as Builder
import           Luna.Syntax.Builder.Class as X (runT)
import           Luna.Syntax.Builder.Class (modify2, BuilderMonad)
import qualified Luna.Syntax.Builder.Node as NodeBuilder
import           Luna.Syntax.Builder.Node (MonadNodeBuilder)

import Data.Container.Class hiding (Impossible)


import Data.Cata

import Data.Layer.Coat

--- === Graph builders ===

--type GraphStarBuilderT s g m = StarBuilderT (Maybe s) (BuilderT g m)
--type GraphStarBuilder  s g   = GraphStarBuilderT s g Identity

--runGraph :: GraphStarBuilder s g a -> g -> (a, g)
--runGraph = runIdentity .: runGraphT

--runGraphT :: Monad m => GraphStarBuilderT s g m a -> g -> m (a, g)
--runGraphT g gs = flip runBuilderT gs $ flip StarBuilder.evalT Nothing $ g


--rebuildGraph :: Maybe s -> g -> GraphStarBuilder s g a -> (a, g)
--rebuildGraph = runIdentity .:. rebuildGraphT

--rebuildGraphT :: Monad m => Maybe s -> g -> GraphStarBuilderT s g m a -> m (a, g)
--rebuildGraphT s gs g = flip runBuilderT gs $ flip StarBuilder.evalT s $ g


--- === Term builders ===

--type LayeredStarBuilder m t = (MonadFix m, MonadStarBuilder (Maybe (Mu t)) m, LayeredASTCons Star m t)

--type LayeredASTMuCons v m t = LayeredASTCons (v (Mu t)) m t
----type LayeredASTConsCtx v a m t = (SpecificCons v (ASTOf a (Mu t)), MuBuilder a m t, LayerGen (Mu t) m a)
--data Impossible a

--class                                       LayeredASTCons variant m          t where layeredASTCons :: variant -> m (Mu t)
----instance LayeredASTConsCtx variant a m t => LayeredASTCons variant m          t where layeredASTCons = mkASTRefWith genLayers
--instance                                    LayeredASTCons variant Impossible t where layeredASTCons = error "Impossible"
--                                            -- only to allow using LayeredASTCons as prefix without expanding further.

--type MuArg m t = Arg (m (Mu t))


--class     Monad m                          => ToMuM' a           m t where toMuM' :: a -> m (Mu t)
--instance (Monad m         , t ~ t'       ) => ToMuM'    (Mu t')  m t where toMuM' = return
--instance (Monad m, (m ~ n), t ~ t'       ) => ToMuM' (n (Mu t')) m t where toMuM' = id
--instance (Monad m, LayeredASTCons Lit m t) => ToMuM' String      m t where toMuM' = string
--instance (Monad m, LayeredASTCons Lit m t) => ToMuM' Int         m t where toMuM' = int

--instance (Monad m, (m ~ n)) => ToMuM' String     m t where toMuM' = string
--instance LayeredASTCons Lit m t => ToMuM String m t where toMuM = string

--class     Monad m           => ToMuM a          m t | a -> t where toMuM :: a -> m (Mu t)

--val = layeredASTCons

-- Utils

--string2 :: forall m t. LayeredASTCons (Val (Mu t)) m t => String -> m (Mu t)
--string2 s = layeredASTCons (V.cons (String $ fromString s) :: Val (Mu t))

--string2 :: LayeredASTCons Lit m t => String -> m (Mu t)
--string2 = layeredASTCons . String . fromString


--mkASTRefWith :: (SpecificCons variant ast, MuBuilder a m t) => (ast -> m (a (Mu t))) -> variant -> m (Mu t)
--mkASTRefWith f a = buildMu =<< f (specificCons a)

-- Literals

swap (a,b) = (b,a)

_int :: (CoatGen (NodeBuilder.NodeBuilderT Int m) a, SpecificCons Lit (Uncoated a), BuilderMonad (Graph a) m, MonadFix m) => Int -> m Int
_int v = mdo
    i <- modify2 . nodes $ swap . ixed add a
    a <- flip NodeBuilder.evalT i $ genCoat $ specificCons (Int v)
    return i

_string :: (CoatGen (NodeBuilder.NodeBuilderT Int m) a, SpecificCons Lit (Uncoated a), BuilderMonad (Graph a) m, MonadFix m) => String -> m Int
_string v = mdo
    i <- modify2 . nodes $ swap . ixed add a
    a <- flip NodeBuilder.evalT i $ genCoat $ specificCons (String $ fromString v)
    return i

_star :: (CoatGen (NodeBuilder.NodeBuilderT Int m) a, SpecificCons Star (Uncoated a), BuilderMonad (Graph a) m, MonadFix m) => m Int
_star = mdo
    i <- modify2 . nodes $ swap . ixed add a
    a <- flip NodeBuilder.evalT i $ genCoat $ specificCons Star
    return i

--_star2 :: (CoatGen m a, SpecificCons Star (Uncoated a), BuilderMonad (Graph a) m) => m Int
_star2 = mdo
    i <- modify2 . nodes $ swap . ixed add a
    a <- NodeBuilder.with i $ genCoat $ specificCons Star
    return i


connection src tgt = do
    modify2 . edges $ swap . ixed add (DoubleArc src tgt)


connect tgt = do
    self <- NodeBuilder.get
    g    <- Builder.get

    i <- flip connection tgt =<< NodeBuilder.get


    g    <- Builder.get
    let tgtn = index tgt (g ^. nodes)
        tgtn' = tgtn & succs %~ (self:)

    Builder.put ( g & nodes %~ unchecked inplace insert tgt tgtn' )

    return i



getStar2 ::(MonadFix m, CoatGen m a, MonadStarBuilder (Maybe Int) m, SpecificCons Star (Uncoated a), BuilderMonad (Graph a) m, MonadNodeBuilder Int m) => m Int
getStar2 =  do
    s <- StarBuilder.get
    case s of
        Just    ref -> return ref
        Nothing     -> newStar
    where newStar = mdo oldstar <- StarBuilder.get
                        StarBuilder.put (Just ref)
                        ref <- _star2
                        StarBuilder.put oldstar
                        return ref




class RegisterConnection a where
    registerConnection :: DoubleArc -> a -> a

instance {-# OVERLAPPABLE #-} (RegisterConnection (Unlayered a), Layered a)
                           => RegisterConnection a                where registerConnection c                                   = layered %~ registerConnection c    
instance {-# OVERLAPPABLE #-} RegisterConnection (SuccTracking a) where registerConnection (DoubleArc _ i) (SuccTracking is a) = SuccTracking (i : is) a
instance {-# OVERLAPPABLE #-} RegisterConnection (Coat a)         where registerConnection _                                   = id




-- Instances

--instance Monad m => LayerGen m (Typed Int a) where
--    genLayer a = Typed <$> pure 0 <*> pure a


--instance (MonadFix m, CoatGen m a, MonadStarBuilder (Maybe Int) m, SpecificCons Star (Uncoated a), BuilderMonad (Graph a) m) => LayerGen m (Typed Int a) where
--    genLayer a = Typed <$> getStar2 <*> pure a

instance ( MonadIO m, RegisterConnection a, Tup2RTup t ~ (t, ()), Show t, Layered t, TracksSuccs (Unlayered t)
         , Uncoated t ~ Uncoated (Unlayered t), LayerGen m t
         , MonadFix m, CoatGen m (Unlayered t), MonadStarBuilder (Maybe Int) m, BuilderMonad (Graph t) m, SpecificCons Star (Uncoated t), MonadNodeBuilder Int m
         ) => LayerGen m (Typed Int a) where
    genLayer a = do
        s <- getStar2
        c <- connect s
        return $ Typed c a

instance Monad m => LayerGen m (SuccTracking a) where
    genLayer a = return $ SuccTracking mempty a




--------------------------------------------------------------------


--string :: LayeredASTCons Lit m t => String -> m (Mu t)
--string = layeredASTCons . String . fromString

--int :: LayeredASTCons Lit m t => Int -> m (Mu t)
--int = layeredASTCons . Int



---- Arg

--arg :: ToMuM a m t => a -> Arg (m (Mu t))
--arg = Arg Nothing . toMuM

---- Terms

--var :: forall name m t. (ToMuM' name m t, LayeredASTMuCons Var m t) => name -> m (Mu t)
--var n = layeredASTCons . Var =<< (toMuM' n :: m (Mu t))


--cons :: forall name m t. (ToMuM' name m t, LayeredASTCons (Cons (Mu t)) m t) => name -> m (Mu t)
--cons n = layeredASTCons . flip Cons [] =<< (toMuM' n :: m (Mu t))


--star :: LayeredASTCons Star m t => m (Mu t)
--star = layeredASTCons Star

--getStar :: LayeredStarBuilder m t => m (Mu t)
--getStar =  do
--    s <- StarBuilder.get
--    case s of
--        Just    ref -> return ref
--        Nothing     -> newStar
--    where newStar = mdo oldstar <- StarBuilder.get
--                        StarBuilder.put (Just ref)
--                        ref <- star
--                        StarBuilder.put oldstar
--                        return ref

--genTopStar :: LayeredStarBuilder m t => m (Mu t)
--genTopStar = do
--    s <- getStar
--    StarBuilder.put (Just s)
--    return s

--accessor :: forall name src m t. (ToMuM' name m t, ToMuM' src m t, LayeredASTMuCons Accessor m t) => name -> src -> m (Mu t)
--accessor n r = do
--    mn <- toMuM' n :: m (Mu t)
--    mr <- toMuM' r :: m (Mu t)
--    layeredASTCons $ Accessor mn mr

--unify :: forall a b m t. (ToMuM' a m t, ToMuM' b m t, LayeredASTMuCons Unify m t) => a -> b -> m (Mu t)
--unify n r = do
--    mn <- toMuM' n :: m (Mu t)
--    mr <- toMuM' r :: m (Mu t)
--    layeredASTCons $ Unify mn mr

--app :: (ToMuM a m t, LayeredASTMuCons App m t) => a -> [MuArg m t] -> m (Mu t)
--app base args = layeredASTCons =<< (App <$> toMuM base <*> (sequence . fmap sequence) args)

---- Drafts

--blank :: LayeredASTCons Blank m t => m (Mu t)
--blank = layeredASTCons Blank


---- operators

--(@.) = flip accessor
--(@$) = app


-- === Function ===

--evalFunctionBuilderT :: Monad m => GraphStarBuilderT s g m a -> BldrState g -> m a
--execFunctionBuilderT :: Monad m => GraphStarBuilderT s g m a -> BldrState g -> m (Function g)
--runFunctionBuilderT  :: Monad m => GraphStarBuilderT s g m a -> BldrState g -> m (a, Function g)

--evalFunctionBuilderT bldr s = fst <$> runFunctionBuilderT bldr s
--execFunctionBuilderT bldr s = snd <$> runFunctionBuilderT bldr s
--runFunctionBuilderT  bldr s = do
--    (a, g) <- runGraphT bldr s
--    return $ (a, Function g)


--evalFunctionBuilder :: GraphStarBuilder s g a -> BldrState g -> a
--execFunctionBuilder :: GraphStarBuilder s g a -> BldrState g -> Function g
--runFunctionBuilder  :: GraphStarBuilder s g a -> BldrState g -> (a, Function g)

--evalFunctionBuilder = runIdentity .: evalFunctionBuilderT
--execFunctionBuilder = runIdentity .: execFunctionBuilderT
--runFunctionBuilder  = runIdentity .: runFunctionBuilderT




