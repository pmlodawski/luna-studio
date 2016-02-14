{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}

{-# LANGUAGE UndecidableInstances #-} -- used for resolution monad, delete after refactoring

module Luna.Compilation.Pass.Inference.Unification where

import Prelude.Luna

import Data.Graph
import Data.Construction
import Data.Container                               hiding (impossible)
import Data.Prop
import Data.Record
import Luna.Evaluation.Runtime                      (Static, Dynamic)
import Data.Index
import Luna.Syntax.AST.Term                         hiding (source)
import Luna.Syntax.Model.Graph
import Luna.Syntax.Model.Graph.Builder              hiding (run)
import Luna.Syntax.Model.Layer
import Luna.Syntax.Model.Network.Builder.Node
import Luna.Syntax.Model.Network.Builder.Term.Class (runNetworkBuilderT, NetGraph, NetLayers)
import Luna.Syntax.Model.Network.Class              ()
import Luna.Syntax.Model.Network.Term
import Luna.Syntax.Name.Ident.Pool                  (MonadIdentPool, newVarIdent')
import Type.Inference

import qualified Luna.Syntax.Model.Graph          as Graph
import qualified Luna.Syntax.Model.Graph.Builder  as Graph
import qualified Luna.Compilation.Stage.TypeCheck as TypeCheck
import qualified Luna.Syntax.Name                 as Name
import Data.Graph.Backend.Vector as Graph

--

import Control.Monad.Fix
import Control.Monad (liftM, MonadPlus(..))

import Control.Monad.Trans.Either

#define PassCtx(m,ls,term) ( term ~ Draft Static                     \
                           , ne   ~ Link (ls :< term)                \
                           , Prop Type   (ls :< term) ~ Ref ne       \
                           , Prop Succs  (ls :< term) ~ [Ref $ ne]   \
                           , BiCastable     e ne                     \
                           , BiCastable     n (ls :< term)           \
                           , MonadBuilder n e (m)                    \
                           , HasProp Type       (ls :< term)         \
                           , HasProp Succs      (ls :< term)         \
                           , NodeInferable  (m) (ls :< term)         \
                           , TermNode Var   (m) (ls :< term)         \
                           , TermNode Lam   (m) (ls :< term)         \
                           , TermNode Unify (m) (ls :< term)         \
                           , TermNode Acc   (m) (ls :< term)         \
                           , MonadIdentPool (m)                      \
                           , Destructor     (m) (Ref $ Node (ls :< term)) \
                           )


--buildAppType :: (PassCtx(m,ls,term), nodeRef ~ Ref (Node $ (ls :< term))) => nodeRef -> m [nodeRef]
--buildAppType appRef = do
--    appNode <- read appRef
--    caseTest (uncover appNode) $ do
--        match $ \(App srcConn argConns) -> do
--            src      <- follow source srcConn
--            args     <- mapM2 (follow source) argConns
--            specArgs <- mapM2 getTypeSpec args
--            out      <- var' =<< newVarIdent'
--            l        <- lam' specArgs out

--            src_v    <- read src
--            let src_tc = src_v # Type
--            src_t    <- follow source src_tc
--            uniSrcTp <- unify src_t l
--            reconnect src (prop Type) uniSrcTp

--            app_v    <- read appRef
--            let app_tc = app_v # Type
--            app_t    <- follow source app_tc
--            uniAppTp <- unify app_t out
--            reconnect appRef (prop Type) uniAppTp

--            return [uniSrcTp, uniAppTp]

--        match $ \ANY -> impossible


--buildAccType :: (PassCtx(m,ls,term), nodeRef ~ Ref (Node $ (ls :< term))) => nodeRef -> m [nodeRef]
--buildAccType accRef = do
--    appNode <- read accRef
--    caseTest (uncover appNode) $ do
--        match $ \(Acc name srcConn) -> do
--            src      <- follow source srcConn
--            srcTSpec <- getTypeSpec src
--            newType  <- acc name srcTSpec
--            acc_v    <- read accRef
--            let acc_tc = acc_v # Type
--            acc_t    <- follow source acc_tc
--            uniTp    <- unify acc_t newType
--            reconnect accRef (prop Type) uniTp
--            return [uniTp]
--        match $ \ANY -> impossible


class Monad m => MonadResolution r m | m -> r where
    resolve :: r -> m ()





--------------------------
---- === Resolution === --
--------------------------

--data Resolution r u = Resolved2   r
--                    | Unresolved2 u
--                    deriving (Show, Functor, Traversable, Foldable)

---- === Instances === --
--instance Applicative (Resolution r) where
--    pure    = Unresolved2
--    f <*> a = case f of
--        Resolved2   r -> Resolved2 r
--        Unresolved2 u -> u <$> a
--    {-# INLINE pure  #-}
--    {-# INLINE (<*>) #-}

--instance Monad (Resolution r) where
--    m >>= f = case m of
--        Resolved2   r -> Resolved2 r
--        Unresolved2 u -> f u
--    {-# INLINE (>>=) #-}

---- MonadResolution

--instance MonadResolution r (Resolution r) where
--    resolve = Resolved2
--    {-# INLINE resolve #-}



-------------------------
-- === ResolutionT === --
-------------------------

--newtype ResolutionT r m u = ResolutionT (m (Resolution r u))
newtype ResolutionT r m u = ResolutionT (EitherT r m u) deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadTrans)
makeWrapped ''ResolutionT

-- === Utils === --

runResolutionT :: Monad m => ResolutionT r m u -> m (Resolution r u)
runResolutionT m = runEitherT (unwrap' m) >>= return ∘ \case
    Left  l -> Resolved   l
    Right r -> Unresolved r


---- === Instances === --

---- Show
deriving instance Show (Unwrapped (ResolutionT r m u)) => Show (ResolutionT r m u)

---- MonadResolution

instance Monad m => MonadResolution r (ResolutionT r m) where
    resolve = wrap' ∘ left
    {-# INLINE resolve #-}

data Resolution r u = Resolved   r
                    | Unresolved u
                    deriving (Show)





resolve_ = resolve []

resolveUnify :: forall m ls term nodeRef ne ter n e. (PassCtx(m,ls,term), nodeRef ~ Ref (Node $ (ls :< term))
                , MonadIO m, Show (ls :< term), MonadResolution [nodeRef] m)
             => nodeRef -> m ()
resolveUnify uni = do
    uni' <- read uni
    caseTest (uncover uni') $ do
        match $ \(Unify lc rc) -> do
            l  <- follow source lc
            r  <- follow source rc

            symmetrical (resolveStar uni) l r
            symmetrical (resolveVar  uni) l r
            resolveLams uni l r

        match $ \ANY -> impossible

    where symmetrical f a b = f a b *> f b a

          resolveStar uni a b = do
              uni' <- read uni
              a'   <- read (a :: nodeRef)
              whenMatched (uncover a') $ \Star -> do
                  mapM_ (reroute b) $ uni' # Succs
                  destruct uni
                  resolve_

          resolveVar uni a b = do
              uni' <- read uni
              a'   <- read (a :: nodeRef)
              whenMatched (uncover a') $ \(Var v) -> do
                  mapM_ (reroute b) $ uni' # Succs
                  mapM_ (reroute b) $ a'   # Succs
                  destruct uni
                  destruct a
                  resolve_

          resolveLams uni a b = do
              uni' <- read uni
              a'   <- read (a :: nodeRef)
              b'   <- read (b :: nodeRef)
              whenMatched (uncover a') $ \(Lam cargs cout) ->
                  whenMatched (uncover b') $ \(Lam cargs' cout') -> do
                    let cRawArgs  = unlayer <$> cargs
                    let cRawArgs' = unlayer <$> cargs'
                    args  <- mapM (follow source) (cout  : cRawArgs )
                    args' <- mapM (follow source) (cout' : cRawArgs')
                    unis  <- zipWithM unify args args'

                    mapM_ (reroute a) $ b' # Succs

                    destruct uni
                    destruct b
                    resolve unis


resolveUnify2 :: forall m ls term nodeRef ne ter n e. (PassCtx(m,ls,term), nodeRef ~ Ref (Node $ (ls :< term))
                , MonadIO m, Show (ls :< term), MonadResolution [nodeRef] m)
             => nodeRef -> m ()
resolveUnify2 uni = do
    putStrLn $ "resolveUnify2 for" <> show uni
    uni' <- read uni
    caseTest (uncover uni') $ do
        match $ \(Unify lc rc) -> do
            l  <- follow source lc
            r  <- follow source rc

            symmetrical (resolveStar uni) l r
            --symmetrical (resolveVar  uni) l r
            resolveVar uni l r
            --resolveLams uni l r

            return ()

        match $ \ANY -> impossible

    where symmetrical f a b = f a b *> f b a

          resolveStar uni a b = do
              uni' <- read uni
              a'   <- read (a :: nodeRef)
              whenMatched (uncover a') $ \Star -> do
                  putStrLn "star!!!"
                  print (uncover a')
                  mapM_ (reroute b) $ uni' # Succs
                  destruct uni
                  resolve_

          resolveVar uni a b = do
              uni' <- read uni
              a'   <- read (a :: nodeRef)
              b'   <- read (b :: nodeRef)
              whenMatched (uncover a') $ \(Var v) -> do
                  putStrLn ""
                  putStrLn $ "VAR unification between: " <> show a <> " and " <> show b
                  let a_succs = a'   # Succs
                  putStrLn $ "a (" <> show (a ^. idx) <> ") # Succs = " <> show a_succs
                  --mapM_ (reroute b) $ uni' # Succs
                  --mapM_ (reroute b) $ a'   # Succs
                  --destruct uni
                  --destruct a
                  --resolve_

    --      resolveLams uni a b = do
    --          uni' <- read uni
    --          a'   <- read (a :: nodeRef)
    --          b'   <- read (b :: nodeRef)
    --          whenMatched (uncover a') $ \(Lam cargs cout) ->
    --              whenMatched (uncover b') $ \(Lam cargs' cout') -> do
    --                let cRawArgs  = unlayer <$> cargs
    --                let cRawArgs' = unlayer <$> cargs'
    --                args  <- mapM (follow source) (cout  : cRawArgs )
    --                args' <- mapM (follow source) (cout' : cRawArgs')
    --                unis  <- zipWithM unify args args'

    --                mapM_ (reroute a) $ b' # Succs

    --                destruct uni
    --                destruct b
    --                resolve unis


-- FIXME[WD]!!!!!!
-- Tu jest bug - nie aktualizujemy successorow!
--reroute lens input edge = do
--    el  <- read edge
--    write edge $ el & lens .~ input
--    input' <- read input
--    --write input $ (input' & (prop Succs) %~ (: edge))
--    return ()

--reroute :: (BiCastable n (ls :< term), BiCastable e (Link (ls :< term)), MonadBuilder n e m, Prop Succs (ls :< term) ~ [Ref (Link (ls :< term))])
--         => Ref (Node (ls :< term)) -> Ref (Link (ls :< term)) -> m ()
reroute input edge = do
    el  <- read edge
    write edge $ el & source .~ input
    input' <- read input
    write input $ (input' & (prop Succs) %~ (edge :))
    return ()

whenMatched a f = caseTest a $ do
    match f
    match $ \ANY -> return ()

-- | Returns a concrete type of a node
--   If the type is just universe, create a new type variable
--getTypeSpec :: PassCtx(m,ls,term) => Ref (Node $ (ls :< term)) -> m (Ref (Node $ (ls :< term)))
--getTypeSpec ref = do
--    val <- read ref
--    tp  <- follow source $ val # Type
--    if tp /= universe then return tp else do
--        ntp <- var' =<< newVarIdent'
--        reconnect ref (prop Type) ntp
--        return ntp

data TCStatus = TCStatus { _terms     :: Int
                         , _coercions :: Int
                         } deriving (Show)

makeLenses ''TCStatus

-- FIXME[WD]: we should not return [Graph n e] from pass - we should use ~ IterativePassRunner instead which will handle iterations by itself
run :: forall nodeRef m ls term n e ne.
       (PassCtx(ResolutionT [nodeRef] m,ls,term), MonadBuilder n e m, nodeRef ~ Ref (Node $ (ls :< term))
       , MonadIO m, Show (ls :< term)
       , Getter Inputs (ls :< term), Prop Inputs (ls :< term) ~ [Ref (Link (ls :< term))])
    => [Int] -> [(Int,Int)] -> Int -> [nodeRef] -> m [Graph n e]
run debugits exc it unis = do
    g <- Graph.get
    let tcs = TCStatus (length (usedIxes $ g ^. Graph.nodeGraph)) (length unis)
    putStrLn $ ("Running Inference.Unification (iteration " <> show it <> "):" :: String)
    print tcs

    when (it `elem` debugits) $ do
        putStrLn ""
        putStrLn $ ">>>> " <> show it
        putStrLn $ "    " <> show (fmap (^. idx) unis)
        let n_13 = cast $ index_ 13 $ g ^. Graph.nodeGraph :: ls :< term
            n_13_ins = n_13 # Inputs
            n_13_es  = (cast ∘ flip index_ (g ^. Graph.edgeGraph) ∘ view idx) <$> n_13_ins :: [Link (ls :< term)]
        let n_21 = cast $ index_ 21 $ g ^. Graph.nodeGraph :: ls :< term
            n_21_ins = n_21 # Inputs
            n_21_es  = (cast ∘ flip index_ (g ^. Graph.edgeGraph) ∘ view idx) <$> n_21_ins :: [Link (ls :< term)]
        putStrLn $ "    13 ins: " <> show (n_13_ins)
        putStrLn $ "    13 es : " <> show (n_13_es)
        when (it == 2) $ do
            putStrLn $ "    21 ins: " <> show (n_21_ins)
            putStrLn $ "    21 es : " <> show (n_21_es)
        putStrLn ""

    results <- if ((it `elem` debugits) && it == 2)
        then mapM (\u -> if ((it, u ^. idx) `elem` exc) then return (Unresolved u) else (if u ^. idx == 21 then fmap (resolveUnifyY u) $ runResolutionT $ resolveUnify2 u else fmap (resolveUnifyY u) $ runResolutionT $ resolveUnify u)) unis
        else mapM (\u -> if ((it, u ^. idx) `elem` exc) then return (Unresolved u) else fmap (resolveUnifyY u) $ runResolutionT $ resolveUnify u) unis

    let resolutions = catResolved results
        newUnis     = concat resolutions
        oldUnis     = catUnresolved results
        unis'       = newUnis <> oldUnis

    g <- Graph.get
    putStrLn $ ("Result size of Inference.Unification (iteration " <> show it <> "):" :: String)
    let tcs' = TCStatus (length (usedIxes $ g ^. Graph.nodeGraph)) (length unis')
    print tcs'

    if (not $ null resolutions) then (g :) <$> run debugits exc (it + 1) unis'
                                else return []




universe = Ref 0 -- FIXME [WD]: Implement it in safe way. Maybe "star" should always result in the top one?

---- FIXME[WD]: Change the implementation to list builder
--resolveUnifyX :: (PassCtx(ResolutionT [nodeRef] m,ls,term), nodeRef ~ Ref (Node $ (ls :< term)), MonadIO m, Show (ls :< term))
--              => nodeRef -> m [nodeRef]
--resolveUnifyX uni = (runResolutionT ∘ resolveUnify) uni >>= return ∘ \case
--    Resolved unis -> unis
--    Unresolved _  -> [uni]

resolveUnifyY uni = \case
    Resolved unis -> Resolved   unis
    Unresolved _  -> Unresolved uni


catUnresolved [] = []
catUnresolved (a : as) = ($ (catUnresolved as)) $ case a of
    Resolved   _ -> id
    Unresolved u -> (u :)

catResolved [] = []
catResolved (a : as) = ($ (catResolved as)) $ case a of
    Unresolved _ -> id
    Resolved   r -> (r :)


--------------------------
-- !!!!!!!!!!!!!!!!!!!! --
--------------------------

-- User - nody i inputy do funkcji bedace varami sa teraz zjadane, moze warto dac im specjalny typ?
-- pogadac z Marcinem o tym

-- cos jest zle z wynikowym unifikatorem
