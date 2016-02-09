{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}

-- {-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE PolyKinds                 #-}

module Main where

import           Prologue                                        hiding (Version, cons, read, ( # ), Num)

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
import           Luna.Syntax.AST.Term                            hiding (Draft, Expr, Lit, Source, Target, Thunk, Val, source, target)
import qualified Luna.Syntax.AST.Term                            as Term
import           Luna.Syntax.Model.Graph
import           Luna.Syntax.Model.Graph.Builder.Ref             as Ref
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder               (rebuildNetwork')
import           Luna.Syntax.Model.Network.Builder.Node
import           Luna.Syntax.Model.Network.Builder.Node.Class    ()
import qualified Luna.Syntax.Model.Network.Builder.Node.Inferred as Inf
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetGraph, NetLayers, runNetworkBuilderT)
import           Luna.Syntax.Model.Network.Term


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
            , MonadIO       m
            , NodeInferable m (ls :< term)
            , TermNode Star m (ls :< term)
            , TermNode Var  m (ls :< term)
            , TermNode App  m (ls :< term)
            ) => m [Ref (Node $ (ls :< term))]
input_g1 = do
    f  <- var' "f"
    a  <- var' "a"
    b  <- var' "b"
    r1 <- app' f [arg a, arg b]

    g  <- var' "g"
    r2 <- app' g [arg r1]
    return [r1,r2]

input_g2 :: ( ls   ~ NetLayers ()
            , term ~ Draft Static
            , MonadIO       m
            , NodeInferable m (ls :< term)
            , TermNode Star m (ls :< term)
            , TermNode Var  m (ls :< term)
            , TermNode Num  m (ls :< term)
            , TermNode Str  m (ls :< term)
            , TermNode Acc  m (ls :< term)
            , TermNode App  m (ls :< term)
            )
         => m (Ref (Node $ (ls :< term)), [Ref (Node $ (ls :< term))])
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

    return (appPlus2, [appPlus1a, appPlus1b, appConc1a, appConc1b, appLen, appPlus2])

test1 :: IO ()
test1 = do
    (_,  g :: NetGraph () ) <- prebuild
    flip Env.evalT def $ do
        v <- view version <$> Env.get
        putStrLn $ "Luna compiler version " <> showVersion v
        TypeCheck.runT $ do
            (all_apps, g') <- runBuild g input_g1
            g''            <- evalBuild g' $ StructInference.run all_apps
            renderAndOpen [ ("g1", g')
                          , ("g2", g'')
                          ]
    putStrLn "done"

test2 :: IO ()
test2 = do
    (_,  g00 :: NetGraph ()) <- prebuild

    -- Running compiler environment
    flip Env.evalT def $ do
        v <- view version <$> Env.get
        putStrLn $ "Luna compiler version " <> showVersion v

        -- Running Type Checking compiler stage
        TypeCheck.runT $ do
            ((root, apps), g01) <- runBuild  g00 input_g2
            (literals, g02)     <- runBuild  g01 $ LiteralsUtils.run root
            g03                 <- evalBuild g02 $ LiteralsAssignement.run literals
            g04                 <- evalBuild g03 $ StructInference.run apps
            renderAndOpen [ ("g03", g03)
                          , ("g04", g04)
                          ]
    putStrLn "done"


main :: IO ()
main = test2

-----------------------
-- === Showcase === ---
-----------------------

foo :: forall a. Show a => NetGraph a -> IO (Ref $ Node (NetLayers a :< Draft Static), NetGraph a)
--foo :: NetGraph -> IO ((), NetGraph)
foo g = runNetworkBuilderT g
    $ do
    title "basic element building"
    s1 <- star
    s2 <- star
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

    return s1




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
