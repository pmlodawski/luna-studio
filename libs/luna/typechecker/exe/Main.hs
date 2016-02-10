{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE RankNTypes                #-}

-- {-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE PolyKinds     #-}

module Main where

import           Prologue                                        hiding (Version, cons, read, ( # ), Num, Cons)

import           Control.Monad.Event
import           Data.Attr                                       (attr)
import           Data.Construction
import           Data.Container                                  (elems, index_)
import           Data.Container
import           Data.Graph.Sort                                 hiding (Graph)
import qualified Data.Graph.Sort                                 as Sort
import           Data.Index                                      (idx)
import           Data.Layer.Cover
import           Data.Prop
import           Data.Record                                     hiding (Layout)
import           Data.Version.Semantic
import           Development.Placeholders
import           Type.Inference

import qualified Luna.Compilation.Env.Class                      as Env
import           Luna.Compilation.Pass.Inference.Literals        as LiteralsAssignement
import qualified Luna.Compilation.Pass.Inference.Struct          as S
import qualified Luna.Compilation.Pass.Inference.Struct          as StructInference
import           Luna.Compilation.Pass.Utils.Literals            as LiteralsUtils
import qualified Luna.Compilation.Stage.TypeCheck                as TypeCheck
import           Luna.Diagnostic.Vis.GraphViz
import           Luna.Evaluation.Runtime                         (Dynamic, Static)
import qualified Luna.Evaluation.Runtime                         as Runtime
import qualified Luna.Evaluation.Model                           as EvalModel
import           Luna.Syntax.AST.Term                            hiding (Draft, Expr, Lit, Source, Target, Thunk, Val, source, target)
import qualified Luna.Syntax.AST.Term                            as Term
import           Luna.Syntax.Model.Graph
import           Luna.Syntax.Model.Graph.Builder.Ref             as Ref
import qualified Luna.Syntax.Model.Graph.Builder.Class           as Graph
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder               (rebuildNetwork')
import           Luna.Syntax.Model.Network.Builder.Node
import           Luna.Syntax.Model.Network.Builder.Node.Class    ()
import qualified Luna.Syntax.Model.Network.Builder.Node.Inferred as Inf
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetGraph, NetLayers, runNetworkBuilderT, fmapInputs)
import           Luna.Syntax.Model.Network.Class                 (Network)
import           Luna.Syntax.Model.Network.Term

import qualified Luna.Syntax.Model.Graph.Cluster as Cluster



title s = putStrLn $ "\n" <> "-- " <> s <> " --"


-- --------------------------------------
--  !!! KEEP THIS ON THE BEGINNING !!! --
-- --------------------------------------
-- - vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv ---
prebuild :: Show a => IO (Ref $ Node (NetLayers a :< Draft Static), NetGraph a)
prebuild = runBuild def star


runBuild (g :: NetGraph a) m = runInferenceT ELEMENT (Proxy :: Proxy (Ref $ Node (NetLayers a :< Draft Static)))
                             $ runNetworkBuilderT g m

evalBuild = fmap snd ∘∘ runBuild


input_g1 :: ( term ~ Draft Static
            , nr   ~ Ref (Node $ (ls :< term))
            , MonadIO       m
            , NodeInferable m (ls :< term)
            , TermNode Star m (ls :< term)
            , TermNode Var  m (ls :< term)
            , TermNode App  m (ls :< term)
            , TermNode Acc  m (ls :< term)
            ) => m ([nr],[nr])
input_g1 = do
    f  <- var' "f"
    a  <- var' "a"
    b  <- var' "b"
    r1 <- app' f [arg a, arg b]

    x  <- acc' "x" r1

    g  <- var' "g"
    r2 <- app' g [arg x]
    return ([r1,r2], [x])



input_g2 :: ( ls   ~ NetLayers ()
            , term ~ Draft Static
            , nr   ~ Ref (Node $ (ls :< term))
            , MonadIO       m
            , NodeInferable m (ls :< term)
            , TermNode Star m (ls :< term)
            , TermNode Var  m (ls :< term)
            , TermNode Num  m (ls :< term)
            , TermNode Str  m (ls :< term)
            , TermNode Acc  m (ls :< term)
            , TermNode App  m (ls :< term)
            )
         => m (nr, ([nr], [nr]))
input_g2 = do
    i1 <- int 2
    i2 <- int 3
    i3 <- int 4
    s1 <- str "abc"
    s2 <- str "def"
    s3 <- str "ghi"

    accPlus1a  <- acc "+" i1
    appPlus1a  <- app accPlus1a [arg i2]

    accPlus1b  <- acc "+" i3
    appPlus1b  <- app accPlus1b [arg appPlus1a]

    accConc1a  <- acc "++" s2
    appConc1a  <- app accConc1a [arg s1]

    accConc1b  <- acc "++" appConc1a
    appConc1b  <- app accConc1b [arg s3]

    accLen     <- acc "len" appConc1b
    appLen     <- app accLen []

    accPlus2   <- acc "+" appPlus1b
    appPlus2   <- app accPlus2 [arg appLen]

    return ( appPlus2
           , ( [appPlus1a, appPlus1b, appConc1a, appConc1b, appLen, appPlus2]
             , [accPlus1a, accPlus1b, accConc1a, accConc1b, accLen, accPlus2]
             )
           )



main :: IO ()
main = do
    --showcase
    --test1
    test2
    return ()


test1 :: IO ()
test1 = do
    (_,  g :: NetGraph () ) <- prebuild

    -- Running compiler environment
    flip Env.evalT def $ do
        v <- view version <$> Env.get
        putStrLn $ "Luna compiler version " <> showVersion v

        -- Running Type Checking compiler stage
        TypeCheck.runT $ do
            ((all_apps, all_accs), g') <- runBuild g input_g1
            g''            <- evalBuild g' $ StructInference.run all_apps all_accs
            renderAndOpen [ ("g1", g')
                          , ("g2", g'')
                          ]
    print "end"


test2 :: IO ()
test2 = do
    (_,  g00 :: NetGraph ()) <- prebuild

    -- Running compiler environment
    flip Env.evalT def $ do
        v <- view version <$> Env.get
        putStrLn $ "Luna compiler version " <> showVersion v

        -- Running Type Checking compiler stage
        TypeCheck.runT $ do
            ((root, (all_apps, all_accs)), g01) <- runBuild  g00 input_g2
            (literals, g02)     <- runBuild  g01 $ LiteralsUtils.run root
            g03                 <- evalBuild g02 $ LiteralsAssignement.run literals
            g04                 <- evalBuild g03 $ StructInference.run all_apps all_accs
            renderAndOpen [ ("g02", g02)
                          , ("g03", g03)
                          , ("g04", g04)
                          ]
    putStrLn "done"



-----------------------
-- === Showcase === ---
-----------------------

showcase :: IO ()
showcase = do
    (_,  g :: NetGraph () ) <- prebuild
    (_, g') <- foo g
    renderAndOpen [ ("g", g')
                  ]

foo :: forall a. Show a => NetGraph a -> IO (Ref $ Node (NetLayers a :< Draft Static), NetGraph a)
--foo :: NetGraph -> IO ((), NetGraph)
foo g = runNetworkBuilderT g
    $ do
    title "basic element building"
    s1 <- int 8
    s2 <- str "test"
    print s1

    title "reading node references"
    s1_v <- read s1
    s2_v <- read s2
    print s1_v

    title "manual connection builing"
    c1 <- connection s1 s2

    title "reading connection references"
    c1_v <- read c1
    print c1_v

    title "edge following"
    c1_tgt <- follow target c1
    when (c1_tgt /= s2) $ fail "reading is broken!"
    print "ok!"

    title "pattern matching"
    print $ uncover s1_v
    print $ caseTest (uncover s1_v) $ do
        match $ \Star -> "its a star! <3"
        match $ \ANY  -> "something else!"

    title "complex element building"
    u1 <- unify s1 s2
    print (u1 :: Ref $ Node (NetLayers a :< Draft Static))
    u1_v <- read u1

    title "inputs reading"
    let u1_ins = u1_v # Inputs
    print u1_ins

    title "params reading"
    let s1t = s1_v # Type
        s1s = s1_v # Succs
    print s1t
    print s1s

    title "cluster definition"
    cl1 <- cluster
    cl2 <- cluster
    include s1 cl1
    include s2 cl1
    include s2 cl2
    print "done"

    title "cluster lookup"
    print =<< cl1 `includes` s2

    title "cluster modification"
    exclude s2 cl1
    print =<< cl1 `includes` s2

    let x1 = s1_v :: Node $ (NetLayers a :< Draft Static)
        x2 = fmapInputs id x1
    return s1


--fmapInputs :: OverElement (MonoTMap t) r => (t -> t) -> (r -> r)


--class TFunctor t r a a' | t r a -> a' where fmapT :: (t -> r) -> a -> a'



--class OverElement ctx rec where overElement :: Proxy ctx -> (forall v. ctx v => v -> v) -> rec -> rec


--instance (out ~ Str          ) => TFunctor2 t r Str           out where fmapT2 = undefined
--instance (out ~ Star         ) => TFunctor2 t r Star          out where fmapT2 = undefined
--instance (out ~ Num          ) => TFunctor2 t r Num           out where fmapT2 = undefined
--instance (out ~ (Lam      r')) => TFunctor2 t r (Lam      t') out where fmapT2 = undefined
--instance (out ~ (Acc    n r')) => TFunctor2 t r (Acc    n t') out where fmapT2 = undefined
--instance (out ~ (Native n r')) => TFunctor2 t r (Native n t') out where fmapT2 = undefined
--instance (out ~ (App      r')) => TFunctor2 t r (App      t') out where fmapT2 = undefined
--instance (out ~ (Unify    r')) => TFunctor2 t r (Unify    t') out where fmapT2 = undefined
--instance (out ~ (Var    n   )) => TFunctor2 t r (Var    n   ) out where fmapT2 = undefined
--instance (out ~ (Cons   n   )) => TFunctor2 t r (Cons   n   ) out where fmapT2 = undefined
--instance (out ~ Blank        ) => TFunctor2 t r Blank         out where fmapT2 = undefined







--instance (out ~ Str          ) => TFunctor2 t r Str           out where fmapT2 = undefined
--instance (out ~ Star         ) => TFunctor2 t r Star          out where fmapT2 = undefined
--instance ()                    => TFunctor2 t r Num           out where fmapT2 = undefined
--instance ()                    => TFunctor2 t r (Lam      t') out where fmapT2 = undefined
--instance ()                    => TFunctor2 t r (Acc    n t') out where fmapT2 = undefined
--instance ()                    => TFunctor2 t r (Native n t') out where fmapT2 = undefined
--instance ()                    => TFunctor2 t r (App      t') out where fmapT2 = undefined
--instance ()                    => TFunctor2 t r (Unify    t') out where fmapT2 = undefined
--instance ()                    => TFunctor2 t r (Var    n   ) out where fmapT2 = undefined
--instance ()                    => TFunctor2 t r (Cons   n   ) out where fmapT2 = undefined
--instance ()                    => TFunctor2 t r Blank         out where fmapT2 = undefined

--fmaptmp :: forall layout term rt x.
--      (MapTryingElemList_
--                            (Elems term (ByRuntime rt Str x) x)
--                            (TFoldable x)
--                            (Term layout term rt), x ~ Layout layout term rt) => Term layout term rt -> [x]


--class TFunctor t r a a' | t r a -> a' where fmapT :: (t -> r) -> a -> a'

--class WithElement' ctx rec a where withElement' :: Proxy ctx -> (forall v. ctx v a => v -> a) -> rec -> a
--instance (MapTryingElemList els ctx rec a, els ~ Layout2 Variant (RecordOf rec)) => WithElement' ctx rec a where withElement' = mapTryingElemList (p :: P els)



cluster :: Constructor m (Ref Cluster) => m (Ref Cluster)
cluster = constructLayer $ Cluster mempty

includes :: Graph.MonadBuilder n e m => Ref Cluster -> Ref a -> m Bool
include  :: Graph.MonadBuilder n e m => Ref a -> Ref Cluster -> m ()
exclude  :: Graph.MonadBuilder n e m => Ref a -> Ref Cluster -> m ()

includes cluster el = Cluster.member (el ^. idx) <$> read cluster
include  el cluster = Ref.with cluster $ Cluster.add    (el ^. idx)
exclude  el cluster = Ref.with cluster $ Cluster.remove (el ^. idx)

----------------------------
-- === Sorting stuff === ---
----------------------------

type instance Item (NetGraph a) = Ref $ Node (NetLayers a :< Draft Static)

instance Sort.CompleteGraph     (Graph (NetLayers a :< Raw) (Link (NetLayers a :< Raw)))

instance Sort.MarkableGraph     (Graph (NetLayers a :< Raw) (Link (NetLayers a :< Raw))) where
    markNode ref g = snd $ rebuildNetwork' g $ do
        Ref.with ref $ prop Markable .~ True
    isMarked ref g = fst $ rebuildNetwork' g $ do
        node <- read ref
        return $ node # Markable

instance Sort.Graph             (Graph (NetLayers a :< Raw) (Link (NetLayers a :< Raw))) where
    listNodes g = map (Ref . Ptr) $ (usedIxes $ g ^. nodeGraph)

instance Sort.ForwardEdgedGraph (Graph (NetLayers a :< Raw) (Link (NetLayers a :< Raw))) where
    successors ref g = fst $ rebuildNetwork' g $ do
        node <- read ref
        mapM (follow target) $ node ^. prop Succs



-------------------------
-- === Benchmarks === ---
-------------------------


--data Bench a = Bench1 a
--             | Bench2
--             deriving (Show)

--main = do


--    args <- getArgs
--    let mode   = read (args !! 0) :: Int
--        argnum = read (args !! 1) :: Int
--        nums = [0..argnum]


--    case mode of
--        0 -> do
--            let ls = const star . show <$> nums
--                pattest l = caseTest l $ do
--                    variantMatch (\Star -> (1 :: Int))
--                getnum _ = 0
--            print $ sum $ pattest <$> ls
--            --print $ sum $ getnum <$> ls
--        1 -> do
--            let ls = const Bench2 . show <$> nums
--                pattest l = case l of
--                    Bench2 -> (1 :: Int)
--                getnum _ = 0
--            print $ sum $ pattest <$> ls
--            --print $ sum $ getnum <$> ls


-- === Performance notes === ---
-- Performance drops observed:
--     - using custom State class and a wrapper for pattern-matches causes drop
--       probably because automatically derived methods in the State wrapper are not inlined (TBI).
--     - using the `reverse` function in pattern match causes a drop, but it should be computed always during the compile time.
