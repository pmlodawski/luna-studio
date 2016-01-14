
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NoOverloadedStrings #-}

-- {-# LANGUAGE PolyKinds #-}

module Main where

import Prologue hiding (simple, empty, Indexable, Simple, cons, lookup, index, children, Cons, Ixed, Repr, repr, minBound, maxBound)
--import Data.Repr

import qualified Data.Map            as Map
import           Data.Map            (Map)
import qualified Data.Text.Lazy      as Text
import           Data.Text.Lazy      (Text)
import           GHC.Prim (Any)
import           GHC.Int
import           Unsafe.Coerce (unsafeCoerce)
import           Data.Convert
import qualified Data.IntMap.Lazy as IntMap
import           Data.IntMap.Lazy (IntMap)
import Data.Typeable       hiding (cast)
import qualified Control.Monad.State as State
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Vector.Mutable ()
import Data.Maybe (fromJust)
import System.Process
import qualified Data.Text.AutoBuilder as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Constraint
import Control.Error.Util (hush)
import Data.Convert.Errors (TypeMismatch (TypeMismatch))
import Data.Constraint.Void
import Data.Variants hiding (cons)
import qualified Data.Variants as V
import Flowbox.System.Types hiding ((.:), insert, Index)
import           Control.Monad.State.Generate (newState)
import Text.Read (readMaybe)
import           Luna.Syntax.Repr.Graph
import qualified Luna.Syntax.Repr.Graph as GraphBuilder
import           Luna.Syntax.Builder
import qualified Luna.Syntax.Builder    as Builder
import qualified Luna.Syntax.Builder.Class as Builder
import           Luna.Syntax.AST.Term
import           Luna.Syntax.AST.Lit
import           Luna.Syntax.AST
import           Luna.Syntax.AST.Decl
import           Luna.Syntax.Name.Pool
import Control.Monad.Fix
import Data.Cata
import           Luna.Syntax.Builder.Star (MonadStarBuilder)
import           Luna.Syntax.Builder.Star (StarBuilder, StarBuilderT)
import qualified Luna.Syntax.Builder.Star as StarBuilder
import Control.Monad.Trans.Identity
--import Luna.Diagnostic.AST (toGraphViz, display)
import Luna.Diagnostic.AST as Diag (toGraphViz, display, render, open)
import Luna.Syntax.AST.Typed
import Luna.Syntax.Layer.Labeled
import qualified Type.BaseType as BT
--import Data.Container
--import Data.Container.Hetero
import Data.Container.Resizable
import Data.Container.Reusable
--import Data.Container.Interface
--import           Data.Container.Poly {- x -} hiding (append)
import Data.Container
import Data.Container.Poly -- (Ixed)
--import Data.Text.CodeBuilder.Builder
import Data.Text.CodeBuilder.Builder as CB hiding (render, app)

import Data.Vector.Dynamic as VD

import Data.Container.Parametrized
import Data.Container.Auto
import Data.Container.Weak
import qualified Data.Container.Opts as Mods
import qualified Data.Container.Instances.Vector.Lazy as Lazy



import Data.STRef
import Control.Monad.ST
import Data.Reprx
import Data.Layer
import qualified System.Mem.Weak      as Mem
import Data.IORef

import Data.Container.Immersed
import Data.Container.Hetero (Ptr(Ptr), ptrIdx)

import Data.Container.Hetero
import Data.Layer.Coat
import qualified Luna.Syntax.Builder.Node as NodeBuilder
import           Luna.Syntax.Builder.Node (MonadNodeBuilder)
import qualified Data.IntSet as IntSet
import           Data.IntSet (IntSet)
import           Data.Construction

-- === HomoBuilder ===

newtype HomoG (t :: * -> *) m a = HomoG { fromHomoG :: IdentityT m a } deriving (MonadFix, Monad, Functor, Applicative, MonadTrans)

runHomoBuilder :: HomoG t m a -> m a
runHomoBuilder = runIdentityT . fromHomoG


instance (MuBuilder a m t, t ~ t') => MuBuilder a (HomoG t m) t' where
    buildMu = lift . buildMu

-------------------------------------------------------------

--nytst2 ::(Arc (Labeled Int (Typed Draft)), HomoGraph ArcPtr (Labeled Int (Typed Draft)))
--nytst2 =
--    flip runGraph (VectorGraph def) $ do
--        s    <- genTopStar
--        i1   <- int 1
--        i2   <- blank
--        plus <- i1 @. "+"
--        sum  <- plus @$ [arg i1, arg i2]
--        return s

--type HomoGraph ref t = VectorGraph (t (Mu (ref t)))

--VectorGraph (Labeled Int (Typed Draft) (Mu (ArcPtr (Labeled Int (Typed Draft)))))

--VectorGraph (Labeled Int (Typed Draft)    Int   )
--VectorGraph (DoubleArc   )

--data DoubleArc a = DoubleArc (Ref Int a) (Ref Int a)




--modifyM2 :: MonadGraphBuilder g m => (g -> m (a, g)) -> m a


--nytst2 :: (Arc (Labeled Int (Typed Draft)), HomoGraph ArcPtr (Labeled Int (Typed Draft)))
--nytst2 =
--    flip runGraph (VectorGraph def) $ do
--        s    <- genTopStar
--        i1   <- int 1
--        --i2   <- int 2
--        --i3   <- int 3
--        --i2   <- blank
--        --plus <- i1 @. "+"
--        --sum  <- plus @$ [arg i1, arg i2]
--        return s

--type FReg = Map String (Arc (Labeled Int (Typed Draft)))

--typed a t = StarBuilder.with (const $ Just t) a

--addStdLiterals :: Arc (Labeled Int (Typed Draft)) -> HomoGraph ArcPtr (Labeled Int (Typed Draft)) -> (FReg, HomoGraph ArcPtr (Labeled Int (Typed Draft)))
--addStdLiterals s g = rebuildGraph Nothing g $ mdo
--    strLit <- string "String" `typed` strTp
--    strTp  <- cons strLit

--    intLit <- string "Int" `typed` strTp
--    intTp  <- cons intLit

--    return $ Map.insert "String" strTp
--           $ Map.insert "Int"    intTp
--           $ Map.empty


--pass2_old :: FReg -> Arc (Labeled Int (Typed Draft)) -> HomoGraph ArcPtr (Labeled Int (Typed Draft)) -> ([Arc (Labeled Int (Typed Draft))], HomoGraph ArcPtr (Labeled Int (Typed Draft)))
--pass2_old freg s gr = rebuildGraph (Just s) gr $ do
--    let Just strTp = Map.lookup "String" freg
--    let Just intTp = Map.lookup "Int"    freg
--    g <- GraphBuilder.get
--    let ptrs = usedIxes g :: [Int]
--    unis <- fmap concat . flip mapM ptrs $ \ptr -> do
--        g <- GraphBuilder.get
--        let (Labeled l (Typed t ast)) = index ptr g
--        out <- case' ast $ do
--            match $ \case
--                Int _ -> do
--                    s <- star `typed` intTp
--                    u <- unify (Mu (Ref (Ptr ptr))) s
--                    return [u]
--                String _ -> do
--                    s <- star `typed` strTp
--                    u <- unify (Mu (Ref (Ptr ptr))) s
--                    return [u]
--                _        -> return []
--            match $ \ANY -> return []
--        return out
--    return unis

--pass2 :: FReg -> Arc (Labeled Int (Typed Draft)) -> HomoGraph ArcPtr (Labeled Int (Typed Draft)) -> ([Arc (Labeled Int (Typed Draft))], HomoGraph ArcPtr (Labeled Int (Typed Draft)))
--pass2 freg s gr = rebuildGraph Nothing gr $ do
--    let cptr       = ptrIdx . fromRef . unwrap
--    let Just strTp = Map.lookup "String" freg
--    let Just intTp = Map.lookup "Int"    freg
--    g <- GraphBuilder.get
--    let ptrs = usedIxes g :: [Int]
--    unis <- fmap concat . flip mapM ptrs $ \ptr -> do
--        g <- GraphBuilder.get
--        let tptr                         = cptr t
--            (Labeled l  (Typed t  ast )) = index ptr g
--            (Labeled lt (Typed tt astt)) = index tptr g
--        out <- case' ast $ do
--            match $ \case
--                Int _ -> do
--                    let (g', idx') = ixed add (Labeled lt (Typed tt astt)) g
--                    GraphBuilder.put g'
--                    --s <- star `typed` intTp
--                    --u <- unify (Mu (Ref (Ptr idx'))) intTp
--                    --u <- mkASTRefWith genLayers $ Unify (Mu (Ref (Ptr idx'))) intTp
--                    --u <- mkASTRefWith genLayers $ Unify (Mu (Ref (Ptr idx'))) intTp

--                    u <- (fmap (Mu . Ref . ptrFrom) . modifyM . unchecked inplace ixed insertM tptr) =<< genLayers (specificCons $ Unify (Mu (Ref (Ptr idx'))) intTp)


--                    return [u]
--                --String _ -> do
--                --    s <- star `typed` strTp
--                --    u <- unify (Mu (Ref (Ptr ptr))) s
--                --    return [u]
--                _        -> return []
--            match $ \ANY -> return []
--        return out
--    return unis



--pass3 :: [Arc (Labeled Int (Typed Draft))] -> Arc (Labeled Int (Typed Draft)) -> HomoGraph ArcPtr (Labeled Int (Typed Draft)) -> ((), HomoGraph ArcPtr (Labeled Int (Typed Draft)))
--pass3 unis s gr = rebuildGraph (Just s) gr $ do
--    flip mapM unis $ \uni -> do
--        g <- GraphBuilder.get
--        let cptr                     = ptrIdx . fromRef . unwrap
--            Labeled l (Typed t ast') = index (cptr uni) g
--        case' ast' $ match $ \(Unify pa pb) -> do
--            let ptra = cptr pa
--                ptrb = cptr pb
--                Labeled la (Typed ta asta) = index ptra g
--                Labeled lb (Typed tb astb) = index ptrb g
--            case' astb $ do
--                match $ \Star -> do
--                    let Labeled lta (Typed tta astta) = index (cptr ta) g
--                    case' astta $ do
--                        match $ \Star -> do
--                            let g' = free (cptr uni)
--                                   $ unchecked inplace insert ptra (Labeled la (Typed tb asta)) g
--                            GraphBuilder.put g'
--                        match $ \ANY -> return ()
--                match $ \ANY -> return ()
--                    --string "fooooooo"
--            case' asta $ do
--                match $ \Star -> do
--                    let Labeled lta (Typed tta astta) = index (cptr ta) g
--                    case' astta $ do
--                        match $ \Star -> do
--                            let g' = free (cptr uni)
--                                   $ unchecked inplace insert ptra (Labeled la (Typed tb asta)) g
--                            GraphBuilder.put g'
--                        match $ \ANY -> return ()
--                match $ \ANY -> return ()
--                    --string "fooooooo"

--            return ()
--        return ()
--    return ()

--type Network = Graph (Labeled2 Int (Typed Int (Coat (Draft Int))))

type LibMap = Map String (Ref Node)

type Network = Graph (Labeled2 Int (Typed (Ref Edge) (SuccTracking (Coat (Draft (Ref Edge)))))) DoubleArc

typed a t = StarBuilder.with (const $ Just t) a

addStdLiterals :: Network -> (LibMap, Network)
addStdLiterals g = runIdentity
                 $ flip StarBuilder.evalT Nothing
                 $ flip NodeBuilder.evalT (Ref $ Node (0 :: Int))
                 $ flip Builder.runT g
                 $ mdo
    strLit <- string "String" `typed` (Ref $ Node 0)
    strTp  <- cons strLit
    reconnect strLit tp strTp

    intLit <- string "Int" `typed` strTp
    intTp  <- cons intLit

    return $ Map.insert "String" strTp
           $ Map.insert "Int"    intTp
           $ Map.empty

    --g <- Builder.get
    --let ns = g ^. nodes
    --    n  = index_ (unwrap strLit) ns
    --    n' = n & tp .~ i
    --    ns' = unchecked inplace insert_ (unwrap strLit) n' ns
    --Builder.put (g & nodes .~ ns')
    --Builder.modify_ $ nodes %~
    --strTp  <- cons strLit
    --return ()


--newtype XCoat a = XCoat a

--tstx2 :: ((), Network)
--tstx2 = runIdentity
--      $ flip StarBuilder.evalT Nothing
--      $ flip Builder.runT def
--      $ flip NodeBuilder.evalT (Node' $ HRef 0 :: Node' (HRef (Draft2 XCoat)))
--      $ do
--            s  <- string "hello"
--            return s

type H t a = t (a t)
--data Labeled l a t = Labeled l (a t)



--tstx2 :: (Node' (HRef2 Coat (Draft2 (HRef2 Coat))), Graph2)
tstx2 :: (Node' (H (HRef2 (Labeled Int (Typed2 Coat))) Draft2), Graph2)
tstx2 = runIdentity
      $ flip StarBuilder.evalT Nothing 
      $ flip Builder.runT def
      $ flip NodeBuilder.evalT (Node' $ HRef2 0 :: Node' (H (HRef2 (Labeled Int (Typed2 Coat))) Draft2))
      $ string2 "hello"

tstx1 :: ((), Network)
tstx1 = runIdentity
      $ flip StarBuilder.evalT Nothing
      $ flip Builder.runT def
      $ flip NodeBuilder.evalT (Ref $ Node (0 :: Int))
      $ do
            topStar <- getStar2
            i1 <- int 7
            s  <- string "hello"
            --i1 <- _int 5
            --i1 <- _int 4
            --i2 <- _int 3
            --str <- _string "plus"
            --acc <- accessor "plus" i1
            --str <- _string "plus"
            --s <- getStar2
            --i1 <- _int 4
            --i1 <- _int 4
            --i1 <- _star
            return ()


--pass2 :: FReg -> Arc (Labeled Int (Typed Draft)) -> HomoGraph ArcPtr (Labeled Int (Typed Draft)) -> ([Arc (Labeled Int (Typed Draft))], HomoGraph ArcPtr (Labeled Int (Typed Draft)))
pass2 :: LibMap -> Network -> ([Ref Node], Network)
pass2 lmap gr = runIdentity
              $ flip StarBuilder.evalT Nothing
              $ flip Builder.runT gr
              $ flip NodeBuilder.evalT (Ref $ Node (0 :: Int))
              $ do
    --let cptr       = ptrIdx . fromRef . unwrap
    let Just strTp = Map.lookup "String" lmap
    let Just intTp = Map.lookup "Int"    lmap
    g <- Builder.get
    let ptrs = Ref . Node <$> usedIxes (g ^. nodes) :: [Ref Node]

    let process (ref :: Ref Node) = do
        node <- readRef ref
        let procnod ast = case' ast $ do
                match $ \case
                    Int _ -> do
                        tnode <- follow $ node ^. tp
                        uni   <- unify tnode intTp
                        reconnect ref tp uni
                        return [uni]
                        -- FIXME: poprawic wkladanie unify - unify powinno "inplace" zastepowac node, nie przepinac go

                    _     -> return []
                match $ \(Val a :: Val (Ref Edge)) -> procnod (V.cast $ Val a)
                match $ \ANY -> return []
        procnod (uncoat node)

    unis <- concat <$> mapM process ptrs
    return unis


-- unifikacja lewo i obustronna!
-- najlepiej chyba wprowadzc skoki zliczajace poprzednikow


pass3 :: LibMap -> [Ref Node] -> Network -> ((), Network)
pass3 lmap unis gr = runIdentity
                   $ flip StarBuilder.evalT Nothing
                   $ flip Builder.runT gr
                   $ flip NodeBuilder.evalT (Ref $ Node (0 :: Int))
                   $ do
    flip mapM_ unis $ \ uni -> do
        node <- readRef uni
        let Unify a b = unsafeFrom $ uncoat node
        a' <- follow a
        b' <- follow b
        na <- readRef a'
        nb <- readRef b'
        atp' <- follow . view tp =<< readRef a'
        --destruct atp'
        case' (uncoat na) $ do
            match $ \Star -> do
                let Labeled2 _ (Typed t (SuccTracking ss (Coat ast))) = node
                mapM_ (retarget b') (Ref . Edge <$> toList ss)
                --destruct a'
                --mapM_ (retarget $ Ref $ Node 0) (Ref . Edge <$> toList ss)
                return ()
            match $ \ANY  -> return ()
        destruct uni
        return ()
    return ()

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


runUniqM :: State.MonadState IntSet m => Int -> m () -> m ()
runUniqM a f = do
    s <- State.get
    if IntSet.member a s then return ()
                         else State.put (IntSet.insert a s) >> f

--data CondState = CondState s (s -> )
--exDbg1 :: ([Ref Node], Network)
--exDbg1 = runIdentity
--      $ flip StarBuilder.evalT Nothing
--      $ flip Builder.runT def
--      $ flip NodeBuilder.evalT (Ref $ Node (0 :: Int))
--      $ do
--            --topStar <- getStar2
--            t  <- _string "type"
--            s  <- _star
--            i2 <- _int 2 `typed` s
--            u  <- unify s t
--            --i2 <- _int 3
--            --str <- _string "plus"
--            --acc <- accessor "plus" i1
--            --str <- _string "plus"
--            --s <- getStar2
--            --i1 <- _int 4
--            --i1 <- _int 4
--            --i1 <- _star
--            return [u]

--type Network = Graph (Labeled2 Int (Typed (Ref Edge) (SuccTracking (Coat (Draft (Ref Edge)))))) DoubleArc
retarget tgt edge = withRef edge $ target .~ tgt

main :: IO ()
main = do
    let g  = snd tstx1
    let (lmap, gs) = addStdLiterals g
    let (unis, g2) = pass2 lmap gs
    let (_   , g3) = pass3 lmap unis g2

    renderAndOpen [ ("g" , g)
                --   , ("gs", gs)
                --   , ("g2", g2)
                --   , ("g3", g3)
                  ]

    pprint g2

    print "end"
    --let (unis, g)  = exDbg1
    --let (_   , g3) = pass3 undefined unis g

    --renderAndOpen [
    --                ("g" , g)  ,
    --                --("gs", gs) ,
    --                --("g2", g2) ,
    --                ("g3", g3)
    --              ]

    --pprint (zip [0..] $ elems (g ^. nodes))
    --pprint (zip [0..] $ elems (g ^. edges))

    --render "g"  $ toGraphViz g
    --render "gs" $ toGraphViz gs
    --render "g2" $ toGraphViz g2

    --open $ fmap (\i -> "/tmp/t" <> show i <> ".png") $ reverse [1..2]

    --pprint g2


    --print tstmv`

    --putStrLn $ repr y
    --print . repr =<< nytst2
        --let (s, g) = nytst2

        --print $ ixes g
        --print $ usedIxes g
        --print $ freeIxes g

        --let (freg, g') = addStdLiterals s g

        ----print   gv

        --let (unis2, g2) = pass2 freg s g'
        --let (_    , g3) = pass3 unis2 s g2

        --render "t1" $ toGraphViz g'
        --render "t2" $ toGraphViz g2
        --render "t3" $ toGraphViz g3

        --open $ fmap (\i -> "/tmp/t" <> show i <> ".png") [1..3]
    --let xa = fromList [1,2,3] :: Auto (Weak Vector) Int
    --let xb = fromList [1,2,3] :: WeakAuto Vector Int
    --let xa = fromList [1,2,3] :: Weak Vector Int
    --xc <- addM 5 xb
    --print $ elems xb
    --print $ (elems xa :: [Int])
    --print $ unlayer xa
    --print $ elems xa
    --Lazy.main
    return ()



renderAndOpen lst = do
    flip mapM_ lst $ \(name, g) -> render name $ toGraphViz g
    open $ fmap (\s -> "/tmp/" <> s <> ".png") (reverse $ fmap fst lst)



data L1 a = L1 a deriving (Show)
type instance (Container (L1 a)) = Container a
instance (HasContainerM m a, Functor m) => HasContainerM m (L1 a)

instance (IsContainerM m a, Functor m) => IsContainerM m (L1 a) where fromContainerM = fmap L1 . fromContainerM
instance Wrapped (L1 a) where
    type Unwrapped (L1 a) = a
    _Wrapped' = iso (\(L1 a) -> a) L1




type instance Container (IORef a) = Container a
instance (HasContainerM m a, MonadIO m) => HasContainerM m (IORef a) where
    viewContainerM   ref = viewContainerM =<< liftIO (readIORef ref)
    setContainerM  v ref = ref <$ (liftIO (readIORef ref) >>= setContainerM v >>= liftIO . writeIORef ref)

instance (IsContainerM m a, MonadIO m) => IsContainerM m (IORef a) where
    fromContainerM a = liftIO . newIORef =<< fromContainerM a

type instance Unlayered (IORef a) = a
instance MonadIO m => LayeredM m (IORef a) where
    viewLayeredM    = liftIO . readIORef
    setLayeredM a l = l <$ liftIO (writeIORef l a)


--class LayeredM m l where
--    viewLayeredM         ::                         l -> m (Unlayered l)
--    default viewLayeredM :: (Layered l, Monad m) => l -> m (Unlayered l)
--    viewLayeredM = return . unlayer

--    setLayeredM         ::                         Unlayered l -> l -> m l
--    default setLayeredM :: (Layered l, Monad m) => Unlayered l -> l -> m l
--    setLayeredM = (fmap . fmap) return $ set layered


--xxxt :: Ixed Appendable Int a => a -> (a, Index (Container a))
xxxt :: Ixed Appendable Int a => a -> (a, Index (Container a))
xxxt v = ixed append (4 :: Int) v

xxxt2 :: Appendable Int a => a -> a
xxxt2 v = append (4 :: Int) v

type TT = Vector Int
--main :: IO ()
--main = do
--    --(v1,i,_) <- singletonQM (Query :: Query '[M.Ixed,M.Ixed] '[]) 5 :: IO (L1 (V.Vector Int), Int, Int)
--    --print (v1,i)



--    print " ---------------------- "

--    let v2  = fromList [1,2,3] :: (Auto' Exponential (L1 (Vector (Int))))
--    v2' <- appendM 5 v2        :: IO (Auto' Exponential (L1 (Vector (Int))))
--    i1  <- indexM  0 v2'
--    print i1

--    tt <- viewImmersedM v2' :: IO (PrimStoreOf (Auto' Exponential (L1 (Vector (Int)))))
--    print tt

--    vref2 <- newIORef v2
--    ir1 <- indexM 1 vref2
--    print ir1

--    vref3 <- allocM 10 :: IO (IORef (Weak' (Auto' Exponential (L1 (Vector (Mem.Weak Int))))))
--    vref3' <- setFinalizerM (Just $ IdxFinalizer $ \idx -> () <$ freeM idx vref3) vref3
--    vref3_1 <- appendM 5 vref3'
--    --ttref <- viewImmersedM vref3 :: IO (Vector Int)

--    --print ttref

--    let v4 = fromList [1,2,3] :: Weak' (Auto' Exponential (L1 (Vector (Mem.Weak Int))))

--    --putStrLn $ "v2: "           <> show v2
--    --putStrLn $ "size: "         <> show (size     v2)
--    --putStrLn $ "min bounds: "   <> show (minBound v2)
--    --putStrLn $ "max bounds: "   <> show (maxBound v2)
--    --print $ elems (expand v2)
--    --print $ size v2'
--    --putStrLn $ "after expand: " <> show (expand   v2)
--    --putStrLn $ "after grow: "   <> show (grow 10  v2)

--    --s <- sizeM v2
--    --print s

--    --print =<< ixed ixed expandM v2

--    print "END"




--(fmap . fmap . fmap) (fromJust . unsafePerformIO . Mem.deRefWeak) . queried (Proxy :: Proxy q) elemsM' . unlayer


--nytst3 :: Mu (Typed Draft)
--nytst3 = flip StarBuilder.eval Nothing $ runIdentityT $ do
--    v1 <- var "foo"
--    return v1

--nytst2f :: (Arc (Labeled Int (Typed Draft)), Function (HomoGraph ArcPtr (Labeled Int (Typed Draft))))
--nytst2f = flip runFunctionBuilder def $ do
--    v1 <- var "foo"
--    v2 <- var "bar"
--    s  <- star
--    a  <- v1 @. "x"
--    x  <- v1 @$ [arg v2]
--    y  <- x @. "y"
--    return v1

--viewGraph = view graph <$> GraphBuilder.get
--mapMGraph f = do
--    g   <- viewGraph
--    mapM f g


--unifyLit = \case
--    _ -> do
--        (s :: Arc (Labeled Int (Typed Draft))) <- string "foo"
--        return ()

--data Constraint a = ConstraintType a a

--withType t = do


    --(unis2, gr2) = pass2

    --class Union m a where
    --    union :: a -> a -> m ()

    --pass3 :: ((), HomoGraph ArcPtr (Labeled Int (Typed Draft)))
    --pass3 = buildGraph (Just s) (BldrState [] gr2) $ do
    --    BldrState x g <- GraphBuilder.get
    --    flip mapM unis2 $ \uni -> do
    --        let cptr                     = ptrIdx . fromRef . fromMu
    --            Labeled l (Typed t ast') = index (cptr uni) g
    --        case' ast' $ match $ \(Unify pa pb) -> do
    --            let ptra = cptr pa
    --                ptrb = cptr pb
    --                Labeled la (Typed ta asta) = index ptra g
    --                Labeled lb (Typed tb astb) = index ptrb g
    --            case' astb $ do
    --                match $ \Star -> do
    --                    let Labeled lta (Typed tta astta) = index (cptr ta) g
    --                    case' astta $ do
    --                        match $ \Star -> do
    --                            let g' = insert ptra (Labeled la (Typed tb asta)) g
    --                            GraphBuilder.put $ BldrState x g'
    --                            return ()
    --                    --string "fooooooo"

    --            return ()
    --        return ()
    --    return ()

    --gr3 = snd pass3

--valCons :: Variant a (Val t) => a -> Val t
--valCons = cons

--val = cons . valCons


-- TODO: tworzenie Val i Thunk, poprawic polimorficznosc w konstruktorach Variantow
-- jak zrobic castowanie pomiedzy Thunk i Val? Czy castowanie kiedykolwiek bedzie potrzebne? chyba nie!
-- nawet jesli nie to castowanie polimorficznych variantow nie dziala dobrze

--valx :: Val t
--valx :: forall t. Term t
--valx = cons $ (valCons (Int 5) :: Val t)

--gr2 :: ((), HomoGraph ArcPtr (Labeled Int (Typed Draft)))
--gr2 = buildGraph (Just s) (BldrState [] gr) $ do

    --let ptrs = indexes g :: [Int]
    --flip mapM ptrs $ \ptr -> do
    --    BldrState x g <- GraphBuilder.get
    --    let (Labeled l (Typed t ast)) = index ptr g
    --    i <- case' ast $ do
    --        match $ \case
    --            Int    i -> string "foo1"
    --            String s -> string "foo2"
    --        match $ \ANY -> return t

    --    BldrState x g <- GraphBuilder.get
    --    let out = Labeled l (Typed i ast)
    --        g'  = insert ptr out g
    --    GraphBuilder.put $ BldrState x g'
    --    return ()

    --g <- view graph <$> GraphBuilder.get
    --withGraphM_ $ \g -> flip mapM g $ \(Labeled l (Typed t ast)) -> do
    --    xx <- case' ast $ do
    --        match $ \case
    --            Int    i -> string "foo1"
    --            String s -> string "foo2"
    --        match $ \ANY -> string "foo3"
    --    i <- string "xxx"
    --    return (Labeled l (Typed t ast))
    --return ()

--withGraphM :: MonadGraphBuilder g m => (g -> m (g, a)) -> m a


--gr3 :: (Arc Term, HomoGraph ArcPtr Term)
--gr3 = flip runGraph def $ do
--    genTopStar
--    i1   <- int 1
--    flip mapM gr $ \(Labeled l t) -> do
--        --case' t $ do
--        --    match $ \ANY -> return ()
--        i <- int 7
--        return ()
--    return i1

--gr2 = flip mapM gr $ \(Labeled l t) -> do
--    v <- int 7
--    return (Labeled l t)

--type AutoVector a = Reusable (Resizable Duplicate (Vector a))
--type AutoVector a = Reusable (Resizable Duplicate (Vector a))




--class                          Appendable2 opts cont     el out | opts cont el -> out where append2 :: Mods opts -> el -> cont -> out

    --return ()
--foo :: _ => _
--foo = append2 (Mods :: Mods '[Ixed])
--ixed append 5 c

--unsafeAppend 5 c

--unsafe append 5 c

--unsafe f opts = f (Mods :: Mods '[Unsafe])
--data
--Trans [Unsafe, Ixed]



data HSIndent  = HSIndent  deriving (Show)


runMeI = renderCode HSIndent

--tstc = runMeI ("o" <+> ("ala" <+> "ola"))

instance Repr s (VectorGraph a) where repr _ = fromString "mu"




--c = singleton (0 :: Int) :: Auto (Weak Vector) Int


--xxx :: Ixed (Addable el) t => el -> t -> (Index' (DataStoreOf (Container t)),t)
--xxx v = ixed add v

--xxx :: Unchecked (Ixed Expandable) t => t -> (_,t)
--xxx v = unchecked ixed expand v

--data Dt = Dt Int deriving (Show)

--mkAs = fromList (fmap Dt [1..1000]) :: Weak Vector Dt

----c = mempty :: Auto (Weak Vector) Int

--c = (mempty :: Weak Vector Int) & finalizer .~ Just print
--c' = (mempty :: Weak (Auto Vector) Int) & finalizer .~ Just print

--d = mempty :: Vector Int

----c = Weak (Just $ const $ print ("uh" :: String)) (mempty :: Vector Int) :: Weak Vector Int

--xxf :: Ixed (AddableM el m) t => t -> el -> m (Index' (DataStoreOf (Container t)), t)
--xxf v c = ixed addM c v

            --main = do
            --    --print c
            --    --print $ (runIdentity $ sizeM d)
            --    --print =<< ixed appendM (2 :: Int) =<< appendM (2 :: Int) =<< appendM (2 :: Int) =<< appendM (2 :: Int) =<< appendM (2 :: Int) c'

            --    print "end"

--xxx :: _ => _
--xxx v = unchecked try index (10 :: Int) v


checkNothing a = if (a == Nothing) then 1 else 0




data MR = MR
data MI = MI
data MX = MX

data Bundle a b = Bundle a b

b = flip Bundle undefined

class Foom mods where
    foom :: mods -> mods


t1 = foom (b MR, (b MI, (b MX, ())))


class SetMod mod a query where setMod :: Bundle mod a -> query -> query
instance {-# OVERLAPPABLE #-} (a ~ a')        => SetMod mod a (Bundle mod  a', qs) where setMod b (_, qs) = (b,qs)
instance {-# OVERLAPPABLE #-} SetMod mod a qs => SetMod mod a (Bundle mod' a', qs) where setMod b (q, qs) = (q, setMod b qs)

--class Set
