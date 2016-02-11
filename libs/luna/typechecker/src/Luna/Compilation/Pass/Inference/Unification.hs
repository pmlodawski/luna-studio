{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}

module Luna.Compilation.Pass.Inference.Unification where

import Prelude.Luna

import Data.Construction
import Data.Container                               hiding (impossible)
import Data.Prop
import Data.Record
import Luna.Evaluation.Runtime                      (Static, Dynamic)
import Luna.Syntax.AST.Term                         hiding (source)
import Luna.Syntax.Model.Graph
import Luna.Syntax.Model.Graph.Builder
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


#define PassCtx(m,ls,term) ( term ~ Draft Static                   \
                           , ne   ~ Link (ls :< term)              \
                           , Prop Type   (ls :< term) ~ Ref ne     \
                           , Prop Succs  (ls :< term) ~ [Ref $ ne] \
                           , BiCastable     e ne                   \
                           , MonadBuilder n e m                    \
                           , HasProp Type     (ls :< term)         \
                           , HasProp Succs    (ls :< term)         \
                           , NodeInferable  m (ls :< term)         \
                           , TermNode Var   m (ls :< term)         \
                           , TermNode Lam   m (ls :< term)         \
                           , TermNode Unify m (ls :< term)         \
                           , TermNode Acc   m (ls :< term)         \
                           , MonadIdentPool m                      \
                           , Destructor     m (Ref $ Node (ls :< term)) \
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

data ResolutionStatus a = Resolved
                        | Unresolved a
                        deriving (Show)

catUnresolved [] = []
catUnresolved (a : as) = ($ (catUnresolved as)) $ case a of
    Resolved     -> id
    Unresolved u -> (u :)

resolveUnify :: forall m ls term nodeRef ne ter n e. (PassCtx(m,ls,term), nodeRef ~ Ref (Node $ (ls :< term))
                , MonadIO m, Show (ls :< term))
             => nodeRef -> m (ResolutionStatus nodeRef)
resolveUnify uni = do
    uni' <- read uni
    caseTest (uncover uni') $ do
        match $ \(Unify lc rc) -> do
            l  <- follow source lc
            r  <- follow source rc
            resolve uni l r
            --symmetrical (resolve uni) l r

        match $ \ANY -> impossible
    where --symmetrical f a b = f a b *> f b a
          resolve uni a b = do
              uni' <- read uni
              a'   <- read (a :: nodeRef)
              b'   <- read (b :: nodeRef)
              caseTest (uncover a') $ do
                  match $ \Star -> do
                      mapM_ (reroute source b) $ uni' # Succs
                      destruct uni
                      return Resolved
                  match $ \ANY  -> return $ Unresolved uni


reroute lens input edge = do
    el  <- read edge
    write edge $ el & lens .~ input



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

run :: (PassCtx(m,ls,term), nodeRef ~ Ref (Node $ (ls :< term))
       , MonadIO m, Show (ls :< term))
    => [nodeRef] -> m ()
run unis = do
    g <- Graph.get
    let it  = (1 :: Int)
        tcs = TCStatus (length (usedIxes $ g ^. Graph.nodeGraph)) (length unis)
    putStrLn $ ("Running Inference.Unification (iteration " <> show it <> "):" :: String)
    print tcs

    unis' <- catUnresolved <$> mapM resolveUnify unis
    g <- Graph.get
    putStrLn $ ("Result size of Inference.Unification (iteration " <> show it <> "):" :: String)
    let tcs' = TCStatus (length (usedIxes $ g ^. Graph.nodeGraph)) (length unis')
    print tcs'

    return ()



universe = Ref $ Ptr 0 -- FIXME [WD]: Implement it in safe way. Maybe "star" should always result in the top one?
