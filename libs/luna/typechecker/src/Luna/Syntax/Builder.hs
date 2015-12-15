{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Syntax.Builder ( module Luna.Syntax.Builder, module X) where

import Prologue hiding (index)

import Control.Monad.Fix
import Data.Variants     as V

import           Luna.Syntax.AST
import           Luna.Syntax.AST.Arg
import           Luna.Syntax.AST.Decl
import           Luna.Syntax.AST.Lit
import           Luna.Syntax.AST.Term
import           Luna.Syntax.AST.Term
import           Luna.Syntax.Builder.Star (MonadStarBuilder, StarBuilder, StarBuilderT)
import qualified Luna.Syntax.Builder.Star as StarBuilder
import           Luna.Syntax.Name
import           Luna.Syntax.Repr.Graph
--import           Luna.Syntax.AST.Typed

import           Luna.Syntax.Builder.Class as X (runT)
import           Luna.Syntax.Builder.Class (BuilderMonad, modify2)
import qualified Luna.Syntax.Builder.Class as Builder
import           Luna.Syntax.Builder.Node  (MonadNodeBuilder)
import qualified Luna.Syntax.Builder.Node  as NodeBuilder

import Data.Cata
import Data.Construction
import Data.Container    hiding (Impossible)

import Data.Layer.Coat

import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Map    as Map

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

class                                             Monadic a      m b where monadic :: a -> m b
instance {-# OVERLAPPABLE #-} Monad m          => Monadic a      m a where monadic = return
instance {-# OVERLAPPABLE #-} (Monad m, m ~ n) => Monadic (n a)  m a where monadic = id
instance {-# OVERLAPPABLE #-} (MonadNodeBuilder (Ref Node) m, CoatConstructor m n, SpecificCons Lit (Uncoated n), BuilderMonad (Graph n e) m, MonadFix m)
                           => Monadic String m (Ref Node) where monadic = _string

swap (a,b) = (b,a)

--_int :: (MonadNodeBuilder (Ref Node) m, CoatConstructor m a, SpecificCons Lit (Uncoated a), BuilderMonad (Graph a) m, MonadFix m) => Int -> m (Ref Node)
--_int v = mdo
--    i <- modify2 . nodes $ swap . ixed add a
--    a <- NodeBuilder.with (Ref $ Node i) $ constructCoat $ specificCons (Int v)
--    return $ Ref $ Node i

_int :: forall m n e ast t. (MonadNodeBuilder (Ref Node) m, CoatConstructor m n, SpecificCons (Val t) (ast t), BuilderMonad (Graph n e) m, MonadFix m, Uncoated n ~ ast t) => Int -> m (Ref Node)
_int v = mdo
    i <- modify2 . nodes $ swap . ixed add a
    a <- NodeBuilder.with (Ref $ Node i) $ constructCoat $ specificCons (specificCons (Int v) :: Val t)
    return $ Ref $ Node i

_string :: (MonadNodeBuilder (Ref Node) m, CoatConstructor m n, SpecificCons Lit (Uncoated n), BuilderMonad (Graph n e) m, MonadFix m) => String -> m (Ref Node)
_string v = mdo
    i <- modify2 . nodes $ swap . ixed add a
    a <- NodeBuilder.with (Ref $ Node i) $ constructCoat $ specificCons (String $ fromString v)
    return $ Ref $ Node i

_string2 :: (MonadNodeBuilder (Ref Node) m, CoatConstructor m n, SpecificCons Lit (Uncoated n), BuilderMonad (Graph n e) m) => String -> m (Ref Node)
_string2 v = do
    i <- modify2 . nodes $ swap . ixed reserve
    a <- NodeBuilder.with (Ref $ Node i) $ constructCoat $ specificCons (String $ fromString v)
    Builder.modify_ $ nodes %~ unchecked inplace insert_ i a
    return $ Ref $ Node i

_star :: (MonadNodeBuilder (Ref Node) m, CoatConstructor m n, SpecificCons Star (Uncoated n), BuilderMonad (Graph n e) m, MonadFix m) => m (Ref Node)
_star = mdo
    i <- modify2 . nodes $ swap . ixed add a
    a <- NodeBuilder.with (Ref $ Node i) $ constructCoat $ specificCons Star
    return $ Ref $ Node i

_blank :: (MonadNodeBuilder (Ref Node) m, CoatConstructor m n, SpecificCons Blank (Uncoated n), BuilderMonad (Graph n e) m, MonadFix m) => m (Ref Node)
_blank = mdo
    i <- modify2 . nodes $ swap . ixed add a
    a <- NodeBuilder.with (Ref $ Node i) $ constructCoat $ specificCons Blank
    return $ Ref $ Node i

--_star2 :: (CoatConstructor m a, SpecificCons Star (Uncoated a), BuilderMonad (Graph a) m) => m Int
_star2 = mdo
    i <- modify2 . nodes $ swap . ixed add a
    a <- NodeBuilder.with (Ref $ Node i) $ constructCoat $ specificCons Star
    return $ Ref $ Node i

arrow p n' r = mdo
    p' <- mapM monadic p
    r' <- monadic r
    i <- modify2 . nodes $ swap . ixed add a
    a <- NodeBuilder.with (Ref $ Node i) $ do
        pc <- mapM connect p'
        let connectItem (k, a) = (k,) <$> connect a
        nc <- Map.fromList <$> mapM connectItem (Map.toList n') --(n' :: Map.Map Name a)
        rc <- connect r'
        constructCoat $ specificCons $ Arrow pc nc rc
    return $ Ref $ Node i

--cons :: forall name m t. (ToMuM' name m t, LayeredASTCons (Cons (Mu t)) m t) => name -> m (Mu t)
--cons n = layeredASTCons . flip Cons [] =<< (toMuM' n :: m (Mu t))
cons c = mdo
    i <- modify2 . nodes $ swap . ixed add a
    a <- NodeBuilder.with (Ref $ Node i) $ do
        cc <- connect c
        constructCoat $ specificCons (flip Cons [] cc)
    return $ Ref $ Node i



connection src tgt = fmap (Ref . Edge) . modify2 . edges $ swap . ixed add (DoubleArc src tgt)

--accessor :: _ => _
accessor name b = mdo
    name' <- monadic name
    b'    <- monadic b
    i <- modify2 . nodes $ swap . ixed add a
    a <- NodeBuilder.with (Ref $ Node i) $ do
        nc <- connect name'
        bc <- connect b'
        constructCoat $ specificCons $ Accessor nc bc
    return $ Ref $ Node i

var (name :: String) = mdo
    name' <- monadic name
    i <- modify2 . nodes $ swap . ixed add a
    a <- NodeBuilder.with (Ref $ Node i) $ do
        nc <- connect name'
        constructCoat $ specificCons $ Var nc
    return $ Ref $ Node i

app acc args = mdo
    acc'  <- monadic acc
    args' <- mapM monadic args
    i <- modify2 . nodes $ swap . ixed add a
    a <- NodeBuilder.with (Ref $ Node i) $ do
        accc <- connect acc'
        let connectArg (Arg n a) = Arg n <$> connect a
        argsc <- mapM connectArg args'
        constructCoat $ specificCons $ App accc argsc
    return $ Ref $ Node i

arg = Arg Nothing

named n (Arg _ a) = Arg (Just n) a --FIXME[WD] : use lens

unify a b = mdo
    a' <- monadic a
    b' <- monadic b
    i <- modify2 . nodes $ swap . ixed add n
    n <- NodeBuilder.with (Ref $ Node i) $ do
        nc <- connect a'
        bc <- connect b'
        constructCoat $ specificCons $ Unify nc bc
    return $ Ref $ Node i
--accessor :: forall name src m t. (ToMuM' name m t, ToMuM' src m t, LayeredASTMuCons Accessor m t) => name -> src -> m (Mu t)
--accessor n r = do
--    mn <- toMuM' n :: m (Mu t)
--    mr <- toMuM' r :: m (Mu t)
--    layeredASTCons $ Accessor mn mr

connect :: (MonadNodeBuilder (Ref Node) m, TracksSuccs n, BuilderMonad (Graph n DoubleArc) m) => Ref Node -> m (Ref Edge)
connect tgt = do
    self <- NodeBuilder.get
    --g    <- Builder.get

    i <- flip connection tgt =<< NodeBuilder.get


    g    <- Builder.get
    let tgtn = index_ (deref tgt) (g ^. nodes)
        tgtn' = tgtn & succs %~ IntSet.insert (deref i)


    Builder.put ( g & nodes %~ unchecked inplace insert_ (deref tgt) tgtn' )

    return i





getStar2 ::(MonadFix m, CoatConstructor m n, MonadStarBuilder (Maybe (Ref Node)) m, SpecificCons Star (Uncoated n), BuilderMonad (Graph n e) m, MonadNodeBuilder (Ref Node) m) => m (Ref Node)
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
instance {-# OVERLAPPABLE #-} RegisterConnection (SuccTracking a) where registerConnection (DoubleArc _ i) (SuccTracking is a) = SuccTracking (IntSet.insert (deref i) is) a
instance {-# OVERLAPPABLE #-} RegisterConnection (Coat a)         where registerConnection _                                   = id




-- Instances

--instance Monad m => LayerGen m (Typed Int a) where
--    genLayer a = Typed <$> pure 0 <*> pure a


--instance (MonadFix m, CoatConstructor m a, MonadStarBuilder (Maybe Int) m, SpecificCons Star (Uncoated a), BuilderMonad (Graph a) m) => LayerGen m (Typed Int a) where
--    genLayer a = Typed <$> getStar2 <*> pure a



instance Monad m => Constructor m (SuccTracking a) where
    construct = return . SuccTracking mempty




--------------------------------------------------------------------


reconnect :: (Builder.BuilderMonad (Graph n DoubleArc) m, TracksSuccs n, MonadNodeBuilder (Ref Node) m) => Ref Node -> Lens' n (Ref Edge) -> Ref Node -> m (Ref Edge)
reconnect src lens tgt = do
    srcNode <- readRef src
    unregisterEdge (srcNode ^. lens)

    i <-  NodeBuilder.with src $ connect tgt

    writeRef src (srcNode & lens .~ i)
    return i

unregisterEdge eid = do
    edge <- readRef eid
    withRef (edge ^. target) $ succs %~ IntSet.delete (deref $ edge ^. source)
    Builder.modify_ $ edges %~ free_ (deref eid)

type instance Destructed (Ref Node) = ()
instance ( Builder.BuilderMonad (Graph n e) m
         , Uncoated (Destructed n) ~ Uncoated n
         , CoatDestructor m (Destructed n)
         , Destructor m n
         , Uncoated (Destructed n) ~ t (Ref Edge)
         , Uncoated (Unlayered n) ~ Uncoated (Destructed n)
         , Layered n
         , Coated (Unlayered n)
         , Foldable t
         , Builder.BuilderMonad (Graph n DoubleArc) m
         , TracksSuccs (Unlayered n)
         ) => Destructor m (Ref Node) where
    destruct ref = do
        node <- readRef ref
        let ii = inputs (uncoat node) :: [Ref Edge]
        mapM_ unregisterEdge ii
        destructCoat node
        Builder.modify_ $ nodes %~ free (deref ref)


class Monad m => RefReader ref m a | ref m -> a where readRef :: Ref ref -> m a
instance (Monad m, Builder.BuilderMonad (Graph n e) m) => RefReader Node m n where readRef ptr = index_ (deref ptr) . view nodes <$> Builder.get
instance (Monad m, Builder.BuilderMonad (Graph n e) m) => RefReader Edge m e where readRef ptr = index_ (deref ptr) . view edges <$> Builder.get

class Monad m => RefWriter ref m a | ref m -> a where writeRef :: Ref ref -> a -> m ()
instance (Monad m, Builder.BuilderMonad (Graph n e) m) => RefWriter Node m n where writeRef ptr a = Builder.modify_ $ nodes %~ unchecked inplace insert_ (deref ptr) a
instance (Monad m, Builder.BuilderMonad (Graph n e) m) => RefWriter Edge m e where writeRef ptr a = Builder.modify_ $ edges %~ unchecked inplace insert_ (deref ptr) a

type RefHandler ref m a = (RefReader ref m a, RefWriter ref m a)

withRefM :: RefHandler ref m a => Ref ref -> (a -> m a) -> m ()
withRefM ref f = readRef ref >>= f >>= writeRef ref

withRef :: RefHandler ref m a => Ref ref -> (a -> a) -> m ()
withRef ref = withRefM ref . fmap return

follow :: RefReader ref f DoubleArc => Ref ref -> f (Ref Node)
follow edge = view target <$> readRef edge

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
